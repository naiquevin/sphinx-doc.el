;;; sphinx-doc.el -- Generate Sphinx friendly docstrings for Python
;;; functions.

;; Author: Vineet Naik <naikvin@gmail.com>
;; Created on: 14th October 2013
;; URL: https://github.com/naiquevin/sphinx-doc.el
;; Version: 0.2.0

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

;;; Installation:
;;
;; This package is yet to be published on MELPA. Until then, copy this
;; file somewhere in your path and add the following lines to your
;; .emacs (or the equivalent config file)
;;
;; (add-hook 'python-mode-hook (lambda () (require 'sphinx-doc)))
;;
;; This will bind the interactive function `sphinx-doc` to `C-c
;; M-d`.

;;; Usage:
;;
;; Inside a Python file, move the cursor to some function/method
;; definition and hit `C-c M-d`

;;; code begins here

;; include libs

(require 'cl)

;; 3rd party deps
(require 's)


;; Some good-to-have helper functions. Note that these have not been
;; written for performance but rather to avoid including any more
;; third party deps such as dash.el. These work sufficiently well for
;; small input

(defun current-line-string ()
  "Return current line as string"
  (buffer-substring-no-properties (point-at-bol) (point-at-eol)))


(defun filter (condp lst)
  (delq nil
        (mapcar (lambda (x) (and (funcall condp x) x)) lst)))


(defun take-while (f seq)
  (if (funcall f (car seq))
      '()
    (cons (car seq) (take-while f (cdr seq)))))


(defun drop-while (f seq)
  (if (funcall f (car seq))
      seq
    (drop-while f (cdr seq))))


(defun interpose (sep seq)
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

(cl-defstruct arg
  name      ; name of the arg
  default)  ; optional default value if specified


(cl-defstruct fndef
  name  ; name of the function
  args) ; list of arg objects


(cl-defstruct field
  key        ; one of the allowed field name keyword
  type       ; optional datatype
  arg        ; optional argument
  (desc "")) ; description


(cl-defstruct doc
  (summary "FIXME! briefly describe function") ; summary line that fits on the first line
  before-fields                                ; list of comments before fields
  after-fields                                 ; list of comments after fields
  fields)                                      ; list of field objects


(defun sphinx-doc-str->arg (s)
  "Builds an arg object from string"
  (let ((parts (mapcar #'s-trim (split-string s "="))))
    (if (cdr parts)
        (make-arg :name (car parts)
                  :default (cadr parts))
      (make-arg :name (car parts)))))


(defun sphinx-doc-fndef->doc (f)
  "Builds a doc object solely from the fndef object. This is used
  when a new docstring is being added"
  (make-doc
   :fields (append
            (mapcar (lambda (a)
                      (make-field :key "param"
                                  :arg (arg-name a)))
                    (fndef-args f))
            (list (make-field :key "returns")
                  (make-field :key "rtype")))))


(defun sphinx-doc-fun-args (argstrs)
  "Returns arguments (list of arg struct objects) from the Python
  function definition as a string. Note that args of type `self`,
  `*args` and `**kwargs` will be ignored"
  (when (not (string= argstrs ""))
    (mapcar #'sphinx-doc-str->arg
            (filter (lambda (str)
                      (and (not (string= (substring str 0 1) "*"))
                           (not (string= str "self"))))
                    (mapcar #'s-trim
                            (split-string argstrs ","))))))


(defun sphinx-doc-str->fndef (s)
  "Builds a fndef object from the python function definition
  represented by a string. Returns fndef object or nil"
  (when (string-match sphinx-doc-fun-regex s)
    (make-fndef :name (match-string 1 s)
                :args (sphinx-doc-fun-args (match-string 2 s)))))


(defun sphinx-doc-field->str (f)
  "Convert a field object to it's string representation"
  (cond ((and (stringp (field-arg f)) (stringp (field-type f)))
         (s-format ":${key} ${type} ${arg}: ${desc}"
                   'aget
                   `(("key" . ,(field-key f))
                     ("type" . ,(field-type f))
                     ("arg" . ,(field-arg f))
                     ("desc" . ,(field-desc f)))))
        ((stringp (field-arg f))
         (s-format ":${key} ${arg}: ${desc}"
                   'aget
                   `(("key" . ,(field-key f))
                     ("arg" . ,(field-arg f))
                     ("desc" . ,(field-desc f)))))
        (t (s-format ":${key}: ${desc}"
                     'aget
                     `(("key" . ,(field-key f))
                       ("desc" . ,(field-desc f)))))))


(defun sphinx-doc-doc->str (ds)
  "Converts a doc object into it's string representation that
  will be inserted as the docstring"
  (s-join
   "\n"
   (filter
    (lambda (x) (not (equal x nil)))
    (list (s-format "\"\"\"$0\n" 'elt (list (doc-summary ds)))
          (when (and (doc-before-fields ds)
                     (not (string= (doc-before-fields ds) "")))
            (concat (doc-before-fields ds) "\n"))
          (s-join "\n" (mapcar #'sphinx-doc-field->str (doc-fields ds)))
          ""
          (when (and (doc-after-fields ds)
                     (not (string= (doc-after-fields ds) "")))
            (concat (doc-after-fields ds) "\n"))
          "\"\"\""))))


(defun sphinx-doc-parse (docstr indent)
  "Parse a docstring into it's equivalent doc object"
  (let* ((lines (mapcar (lambda (line)
                          (s-chop-prefix (make-string indent 32) line))
                        (split-string docstr "\n")))
         (paras (sphinx-doc-lines->paras lines))
         (field-para? #'(lambda (p) (s-starts-with? ":" (car p)))))
    (progn
      (make-doc :summary (caar paras)
                :before-fields (sphinx-doc-paras->str
                                (take-while field-para? (cdr paras)))
                :after-fields (sphinx-doc-paras->str
                               (cdr (drop-while field-para? (cdr paras))))
                :fields (sphinx-doc-parse-fields (car (filter field-para? paras)))))))


(defun sphinx-doc-paras->str (paras)
  "Converts a list of paras (which in turn is a list of lines) to
  text. This is done by adding a newline between two lines of
  each para and a blank line between each para"
  (s-join
   ""
   (apply #'append
          (interpose '("\n\n")
                     (mapcar (lambda (p)
                               (interpose "\n" p))
                             paras)))))


(defun sphinx-doc-lines->paras (lines)
  "Groups lines represented as list of strings into paras
  represented as list of lists of lines"
  (reverse
   (mapcar
    #'reverse
    (car
     (reduce (lambda (acc x)
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
            (make-field :key (match-string 1 s)
                        :type (match-string 2 s)
                        :arg (match-string 3 s)
                        :desc (match-string 4 s)))
           ((string-match "^:\\([a-z]+\\) \\([a-zA-Z0-9_]+\\): \\(.*\n?\s*.*\\)$" s)
            (make-field :key (match-string 1 s)
                        :arg (match-string 2 s)
                        :desc (match-string 3 s)))
           ((string-match "^:\\([a-z]+\\): \\(.*\n?\s*.*\\)$" s)
            (make-field :key (match-string 1 s)
                        :desc (match-string 2 s)))))
   (mapcar (lambda (s)
             (if (s-starts-with? ":" s) s (concat ":" s)))
           (split-string (s-join "\n" fields-para) "\n:"))))


(defun sphinx-doc-merge-docs (old new)
  "Merge a new doc object into an old one. Effectively, only the
  fields field of new doc are merged whereas the remaining fields
  of the old object remain as they are"
  (make-doc :summary (doc-summary old)
            :before-fields (doc-before-fields old)
            :after-fields (doc-after-fields old)
            :fields (sphinx-doc-merge-fields
                     (doc-fields old)
                     (doc-fields new))))


(defun sphinx-doc-merge-fields (old new)
  "Merge new fields (list of field objects) into old fields"
  (let ((field-index (mapcar (lambda (f)
                               (if (field-arg f)
                                   (cons (field-arg f) f)
                                 (cons (field-key f) f)))
                             old)))
    (progn
      (mapcar (lambda (f)
                (cond ((assoc (field-arg f) field-index)
                       (cdr (assoc (field-arg f) field-index)))
                      ((assoc (field-key f) field-index)
                       (cdr (assoc (field-key f) field-index)))
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
    (s-starts-with? "\"\"\"" (s-trim (current-line-string)))))


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
  (let ((fd (sphinx-doc-str->fndef (current-line-string))))
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

;;; code ends here
