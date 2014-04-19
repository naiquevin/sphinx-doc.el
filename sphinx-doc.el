;;; sphinx-doc.el --- Sphinx friendly docstrings for Python functions

;; Copyright (c) 2013 <naikvin@gmail.com>

;; Author: Vineet Naik <naikvin@gmail.com>
;; URL: https://github.com/naiquevin/sphinx-doc.el
;; Version: 0.2.0
;; Keywords: Sphinx, Python
;; Package-Requires: ((s "1.9.0") (cl-lib "0.5"))

;; This program is *not* a part of emacs and is provided under the MIT
;; License (MIT) <http://opensource.org/licenses/MIT>
;;
;; Copyright (c) 2013 <naikvin@gmail.com>
;;
;; Permission is hereby granted, free of charge, to any person
;; obtaining a copy of this software and associated documentation
;; files (the "Software"), to deal in the Software without
;; restriction, including without limitation the rights to use, copy,
;; modify, merge, publish, distribute, sublicense, and/or sell copies
;; of the Software, and to permit persons to whom the Software is
;; furnished to do so, subject to the following conditions:
;;
;; The above copyright notice and this permission notice shall be
;; included in all copies or substantial portions of the Software.
;;
;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
;; EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
;; MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
;; NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS
;; BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN
;; ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
;; CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
;; SOFTWARE.

;;; Commentary:
;;
;; This file provides a minor mode for inserting docstring skeleton
;; for Python functions and methods. The structure of the docstring is
;; as per the requirements of the Sphinx documentation generator
;; <http://sphinx-doc.org/index.html>

;;; Code:

(require 'cl-lib)
(require 's)


;; Some good-to-have helper functions. Note that these have not been
;; written for performance but rather to avoid including any more
;; third party deps such as dash.el. These work sufficiently well for
;; small input

(defun sphinx-doc-current-line ()
  "Return current line as string"
  (buffer-substring-no-properties (point-at-bol) (point-at-eol)))


(defun sphinx-doc-filter (condp lst)
  (delq nil
        (mapcar (lambda (x) (and (funcall condp x) x)) lst)))


(defun sphinx-doc-take-while (f seq)
  (if (funcall f (car seq))
      '()
    (cons (car seq) (sphinx-doc-take-while f (cdr seq)))))


(defun sphinx-doc-drop-while (f seq)
  (if (funcall f (car seq))
      seq
    (sphinx-doc-drop-while f (cdr seq))))


(defun sphinx-doc-interpose (sep seq)
  (cl-labels ((aux (xs)
                 (if (equal xs nil)
                     (cons sep nil)
                   (cons sep
                         (cons (car xs)
                               (aux (cdr xs)))))))
    (butlast (cdr (aux seq)) 1)))


;; regular expression to identify a valid function definition in
;; python and match it's name and arguments
(defconst sphinx-doc-fun-regex "^\s*def \\([a-zA-Z0-9_]+\\)(\\(.*\\)):$")


;; struct definitions

(cl-defstruct sphinx-doc-arg
  name      ; name of the arg
  default)  ; optional default value if specified


(cl-defstruct sphinx-doc-fndef
  name  ; name of the function
  args) ; list of arg objects


(cl-defstruct sphinx-doc-field
  key        ; one of the allowed field name keyword
  type       ; optional datatype
  arg        ; optional argument
  (desc "")) ; description


(cl-defstruct sphinx-doc-doc
  (summary "FIXME! briefly describe function") ; summary line that fits on the first line
  before-fields                                ; list of comments before fields
  after-fields                                 ; list of comments after fields
  fields)                                      ; list of field objects


(defun sphinx-doc-str->arg (s)
  "Builds an arg object from string"
  (let ((parts (mapcar #'s-trim (split-string s "="))))
    (if (cdr parts)
        (make-sphinx-doc-arg :name (car parts)
                             :default (cadr parts))
      (make-sphinx-doc-arg :name (car parts)))))


(defun sphinx-doc-fndef->doc (f)
  "Builds a doc object solely from the fndef object. This is used
  when a new docstring is being added"
  (make-sphinx-doc-doc
   :fields (append
            (mapcar (lambda (a)
                      (make-sphinx-doc-field
                       :key "param"
                       :arg (sphinx-doc-arg-name a)))
                    (sphinx-doc-fndef-args f))
            (list (make-sphinx-doc-field :key "returns")
                  (make-sphinx-doc-field :key "rtype")))))


(defun sphinx-doc-fun-args (argstrs)
  "Returns arguments (list of arg struct objects) from the Python
  function definition as a string. Note that args of type `self`,
  `*args` and `**kwargs` will be ignored"
  (when (not (string= argstrs ""))
    (mapcar #'sphinx-doc-str->arg
            (sphinx-doc-filter
             (lambda (str)
               (and (not (string= (substring str 0 1) "*"))
                    (not (string= str "self"))))
             (mapcar #'s-trim
                     (split-string argstrs ","))))))


(defun sphinx-doc-str->fndef (s)
  "Builds a fndef object from the python function definition
  represented by a string. Returns fndef object or nil"
  (when (string-match sphinx-doc-fun-regex s)
    (make-sphinx-doc-fndef
     :name (match-string 1 s)
     :args (sphinx-doc-fun-args (match-string 2 s)))))


(defun sphinx-doc-field->str (f)
  "Convert a field object to it's string representation"
  (cond ((and (stringp (sphinx-doc-field-arg f))
              (stringp (sphinx-doc-field-type f)))
         (s-format ":${key} ${type} ${arg}: ${desc}"
                   'aget
                   `(("key" . ,(sphinx-doc-field-key f))
                     ("type" . ,(sphinx-doc-field-type f))
                     ("arg" . ,(sphinx-doc-field-arg f))
                     ("desc" . ,(sphinx-doc-field-desc f)))))
        ((stringp (sphinx-doc-field-arg f))
         (s-format ":${key} ${arg}: ${desc}"
                   'aget
                   `(("key" . ,(sphinx-doc-field-key f))
                     ("arg" . ,(sphinx-doc-field-arg f))
                     ("desc" . ,(sphinx-doc-field-desc f)))))
        (t (s-format ":${key}: ${desc}"
                     'aget
                     `(("key" . ,(sphinx-doc-field-key f))
                       ("desc" . ,(sphinx-doc-field-desc f)))))))


(defun sphinx-doc-doc->str (ds)
  "Converts a doc object into it's string representation that
  will be inserted as the docstring"
  (s-join
   "\n"
   (sphinx-doc-filter
    (lambda (x) (not (equal x nil)))
    (list (s-format "\"\"\"$0\n" 'elt (list (sphinx-doc-doc-summary ds)))
          (when (and (sphinx-doc-doc-before-fields ds)
                     (not (string= (sphinx-doc-doc-before-fields ds) "")))
            (concat (sphinx-doc-doc-before-fields ds) "\n"))
          (s-join "\n" (mapcar #'sphinx-doc-field->str
                               (sphinx-doc-doc-fields ds)))
          ""
          (when (and (sphinx-doc-doc-after-fields ds)
                     (not (string= (sphinx-doc-doc-after-fields ds) "")))
            (concat (sphinx-doc-doc-after-fields ds) "\n"))
          "\"\"\""))))


(defun sphinx-doc-parse (docstr indent)
  "Parse a docstring into it's equivalent doc object"
  (let* ((lines (mapcar (lambda (line)
                          (s-chop-prefix (make-string indent 32) line))
                        (split-string docstr "\n")))
         (paras (sphinx-doc-lines->paras lines))
         (field-para? #'(lambda (p) (s-starts-with? ":" (car p)))))
    (progn
      (make-sphinx-doc-doc
       :summary (caar paras)
       :before-fields (sphinx-doc-paras->str
                       (sphinx-doc-take-while field-para? (cdr paras)))
       :after-fields (sphinx-doc-paras->str
                      (cdr (sphinx-doc-drop-while field-para? (cdr paras))))
       :fields (sphinx-doc-parse-fields
                (car (sphinx-doc-filter field-para? paras)))))))


(defun sphinx-doc-paras->str (paras)
  "Converts a list of paras (which in turn is a list of lines) to
  text. This is done by adding a newline between two lines of
  each para and a blank line between each para"
  (s-join
   ""
   (apply #'append
          (sphinx-doc-interpose '("\n\n")
                                (mapcar (lambda (p)
                                          (sphinx-doc-interpose "\n" p))
                                        paras)))))


(defun sphinx-doc-lines->paras (lines)
  "Groups lines represented as list of strings into paras
  represented as list of lists of lines"
  (reverse
   (mapcar
    #'reverse
    (car
     (cl-reduce (lambda (acc x)
                  (let ((paras (car acc))
                        (prev-blank? (cdr acc)))
                    (cond ((string= x "") (cons paras t))
                          (prev-blank? (cons (cons (list x) paras) nil))
                          (t (cons (cons (cons x (car paras)) (cdr paras)) nil)))))
                (cdr lines)
                :initial-value (cons (list (list (car lines))) nil))))))


(defun sphinx-doc-parse-fields (fields-para)
  "Parse fields section of a docstring into list of field
  objects"
  (mapcar
   (lambda (s)
     (cond ((string-match "^:\\([a-z]+\\) \\([a-z]+\\) \\([a-zA-Z0-9_]+\\): \\(.*\n?\s*.*\\)$" s)
            (make-sphinx-doc-field :key (match-string 1 s)
                                   :type (match-string 2 s)
                                   :arg (match-string 3 s)
                                   :desc (match-string 4 s)))
           ((string-match "^:\\([a-z]+\\) \\([a-zA-Z0-9_]+\\): \\(.*\n?\s*.*\\)$" s)
            (make-sphinx-doc-field :key (match-string 1 s)
                                   :arg (match-string 2 s)
                                   :desc (match-string 3 s)))
           ((string-match "^:\\([a-z]+\\): \\(.*\n?\s*.*\\)$" s)
            (make-sphinx-doc-field :key (match-string 1 s)
                                   :desc (match-string 2 s)))))
   (mapcar (lambda (s)
             (if (s-starts-with? ":" s) s (concat ":" s)))
           (split-string (s-join "\n" fields-para) "\n:"))))


(defun sphinx-doc-merge-docs (old new)
  "Merge a new doc object into an old one. Effectively, only the
  fields field of new doc are merged whereas the remaining fields
  of the old object remain as they are"
  (make-sphinx-doc-doc
   :summary (sphinx-doc-doc-summary old)
   :before-fields (sphinx-doc-doc-before-fields old)
   :after-fields (sphinx-doc-doc-after-fields old)
   :fields (sphinx-doc-merge-fields
            (sphinx-doc-doc-fields old)
            (sphinx-doc-doc-fields new))))


(defun sphinx-doc-merge-fields (old new)
  "Merge new fields (list of field objects) into old fields"
  (let ((field-index (mapcar (lambda (f)
                               (if (sphinx-doc-field-arg f)
                                   (cons (sphinx-doc-field-arg f) f)
                                 (cons (sphinx-doc-field-key f) f)))
                             old)))
    (progn
      (mapcar (lambda (f)
                (cond ((assoc (sphinx-doc-field-arg f) field-index)
                       (cdr (assoc (sphinx-doc-field-arg f) field-index)))
                      ((assoc (sphinx-doc-field-key f) field-index)
                       (cdr (assoc (sphinx-doc-field-key f) field-index)))
                      (t f)))
              new))))


;; Note: Following few functions (those using `save-excursion`) must
;; be invoked only when the cursor is on the function definition line.


(defun sphinx-doc-get-region (srch-beg srch-end direction)
  "Selects a region and returns the beginning and end point as
  vector"
  (save-excursion
    (if (string= direction "forward")
        (search-forward srch-beg)
      (search-backward srch-beg))
    (let ((beg (point)))
      (search-forward srch-end)
      (vector beg (point)))))


(defun sphinx-doc-current-indent ()
  "Returns the indentation level of the current line, ie. by how
  many number of spaces the current line is indented"
  (save-excursion
    (let ((bti (progn (back-to-indentation) (point)))
          (bol (progn (beginning-of-line) (point))))
      (- bti bol))))


(defun sphinx-doc-exists? ()
  "Returns whether the docstring already exists for a function"
  (save-excursion
    (next-line)
    (s-starts-with? "\"\"\"" (s-trim (sphinx-doc-current-line)))))


(defun sphinx-doc-existing ()
  "Returns the docstring for a function if it's already added
  otherwise nil"
  (when (sphinx-doc-exists?)
    (let* ((ps (sphinx-doc-get-region "\"\"\"" "\"\"\"" "forward"))
           (docstr (buffer-substring-no-properties (aref ps 0)
                                                   (- (aref ps 1) 3)))
           (indent (save-excursion
                     (next-line)
                     (sphinx-doc-current-indent))))
      (sphinx-doc-parse docstr indent))))


(defun sphinx-doc-kill-old-doc ()
  "Kill the old docstring"
  (save-excursion
    (let ((ps (sphinx-doc-get-region "\"\"\"" "\"\"\"" "forward")))
      (kill-region (- (elt ps 0) 3) (elt ps 1))
      (next-line)
      (beginning-of-line)
      (kill-line))))


(defun sphinx-doc-insert-doc (doc)
  "Insert the doc as string for the current function"
  (save-excursion
    (move-end-of-line nil)
    (newline-and-indent)
    (insert (sphinx-doc-doc->str doc))))


(defun sphinx-doc-indent-doc (indent)
  "Indent the docstring for the current function"
  (save-excursion
    (let ((ps (sphinx-doc-get-region "\"\"\"" "\"\"\"" "forward")))
      (indent-rigidly (elt ps 0) (elt ps 1) indent))))


(defun sphinx-doc ()
  "Interactive command to insert docstring skeleton for the
  function definition at point"
  (interactive)
  (let ((fd (sphinx-doc-str->fndef (sphinx-doc-current-line))))
    (if fd
        (let ((curr-indent (sphinx-doc-current-indent))
              (old-ds (sphinx-doc-existing))
              (new-ds (sphinx-doc-fndef->doc fd)))
          (progn
            (when old-ds (sphinx-doc-kill-old-doc))
            (sphinx-doc-insert-doc
             (if old-ds
                 (sphinx-doc-merge-docs old-ds new-ds)
               new-ds))
            (sphinx-doc-indent-doc (+ curr-indent python-indent))
            (search-forward "\"\"\""))))))


(defvar sphinx-doc-mode-map
  (let ((m (make-sparse-keymap)))
    (define-key m (kbd "C-c M-d") 'sphinx-doc)
    m))


;;;###autoload
(define-minor-mode sphinx-doc-mode
  "Sphinx friendly docstring skeleton generation for Python code"
  :init-value t
  :lighter " Spnxd"
  :keymap sphinx-doc-mode-map)


(provide 'sphinx-doc)

;;; sphinx-doc.el ends here
