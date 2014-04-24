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
;; for Python functions and methods.  The structure of the docstring is
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
  "Return current line as string."
  (buffer-substring-no-properties (point-at-bol) (point-at-eol)))


(defun sphinx-doc-filter (f xs)
  "Return items from list that pass the predicate.
F is the predicate, XS is the list of items"
  (delq nil
        (mapcar (lambda (x) (and (funcall f x) x)) xs)))


(defun sphinx-doc-take-while (f xs)
  "Return items from list while predicate returns true.
F is the predicate, XS is the list of items."
  (if (not (funcall f (car xs)))
      '()
    (cons (car xs) (sphinx-doc-take-while f (cdr xs)))))


(defun sphinx-doc-drop-while (f xs)
  "Return items from list after predicate first returns true.
F is the predicate, XS is the list of items."
  (when xs
    (if (funcall f (car xs))
        (sphinx-doc-drop-while f (cdr xs))
      xs)))


(defun sphinx-doc-interpose (sep seq)
  "Return list of items separated by the separator.
SEP is the separator, SEQ is the list of items."
  (cl-labels ((aux (xs)
                 (if (equal xs nil)
                     (cons sep nil)
                   (cons sep
                         (cons (car xs)
                               (aux (cdr xs)))))))
    (butlast (cdr (aux seq)) 1)))


;; regular expression to identify a valid function definition in
;; python and match it's name and arguments
(defconst sphinx-doc-fun-regex "^ *def \\([a-zA-Z0-9_]+\\)(\\(\\(?:.\\|\n\\)*\\)):$")

;; regexes for beginning and end of python function definitions
(defconst sphinx-doc-fun-beg-regex "def")
(defconst sphinx-doc-fun-end-regex ":\\(?:\n\\)?")


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
  "Build an arg object from string S."
  (let ((parts (mapcar #'s-trim (split-string s "="))))
    (if (cdr parts)
        (make-sphinx-doc-arg :name (car parts)
                             :default (cadr parts))
      (make-sphinx-doc-arg :name (car parts)))))


(defun sphinx-doc-fndef->doc (f)
  "Build a doc object solely from fndef F."
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
  "Extract list of arg objects from string ARGSTRS.
ARGSTRS is the string representing function definition in Python.
Note that the arguments self, *args and **kwargs are ignored."
  (when (not (string= argstrs ""))
    (mapcar #'sphinx-doc-str->arg
            (sphinx-doc-filter
             (lambda (str)
               (and (not (string= (substring str 0 1) "*"))
                    (not (string= str "self"))))
             (mapcar #'s-trim
                     (split-string argstrs ","))))))


(defun sphinx-doc-str->fndef (s)
  "Build a fndef object from string S.
S is a string representation of the python function definition
Returns nil if string is not a function definition."
  (when (string-match sphinx-doc-fun-regex s)
    (make-sphinx-doc-fndef
     :name (match-string 1 s)
     :args (sphinx-doc-fun-args (match-string 2 s)))))


(defun sphinx-doc-field->str (f)
  "Convert a field object F to it's string representation."
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
  "Convert a doc object DS into string representation."
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
  "Parse docstring DOCSTR into it's equivalent doc object.
INDENT is the current indentation level of the Python function."
  (let* ((lines (mapcar (lambda (line)
                          (s-chop-prefix (make-string indent 32) line))
                        (split-string docstr "\n")))
         (paras (sphinx-doc-lines->paras lines))
         (field-para? #'(lambda (p) (s-starts-with? ":" (car p))))
         (comment? #'(lambda (p) (not (funcall field-para? p)))))
    (progn
      (make-sphinx-doc-doc
       :summary (caar paras)
       :before-fields (sphinx-doc-paras->str
                       (sphinx-doc-take-while comment? (cdr paras)))
       :after-fields (sphinx-doc-paras->str
                      (cdr (sphinx-doc-drop-while comment? (cdr paras))))
       :fields (sphinx-doc-parse-fields
                (car (sphinx-doc-filter field-para? paras)))))))


(defun sphinx-doc-paras->str (paras)
  "Convert PARAS to string.
PARAS are list of paragraphs (which in turn are list of lines).
This is done by adding a newline between two lines of each para
and a blank line between each para."
  (s-join
   ""
   (apply #'append
          (sphinx-doc-interpose '("\n\n")
                                (mapcar (lambda (p)
                                          (sphinx-doc-interpose "\n" p))
                                        paras)))))


(defun sphinx-doc-lines->paras (lines)
  "Group LINES which are list of strings into paragraphs."
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
  "Parse FIELDS-PARA into list of field objects.
FIELDS-PARA is the fields section of the docstring."
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
  "Merge OLD and NEW doc objects.
Effectively, only the fields field of new doc are merged whereas
the remaining fields of the old object stay as they are."
  (make-sphinx-doc-doc
   :summary (sphinx-doc-doc-summary old)
   :before-fields (sphinx-doc-doc-before-fields old)
   :after-fields (sphinx-doc-doc-after-fields old)
   :fields (sphinx-doc-merge-fields
            (sphinx-doc-doc-fields old)
            (sphinx-doc-doc-fields new))))


(defun sphinx-doc-merge-fields (old new)
  "Merge old and new fields together.
OLD is the list of old field objects, NEW is the list of new
field objects."
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
  "Return the beginning and end points of a region by searching.
SRCH-BEG and SRCH-END are the chars to search for and DIRECTION
is the direction to search in."
  (save-excursion
    (if (string= direction "forward")
        (search-forward-regexp srch-beg)
      (search-backward-regexp srch-beg))
    (let ((beg (point)))
      (search-forward-regexp srch-end)
      (vector beg (point)))))


(defun sphinx-doc-current-indent ()
  "Return the indentation level of the current line.
ie. by how many number of spaces the current line is indented"
  (save-excursion
    (let ((bti (progn (back-to-indentation) (point)))
          (bol (progn (beginning-of-line) (point))))
      (- bti bol))))


(defun sphinx-doc-fndef-str ()
  "Return the Python function definition as a string."
  (save-excursion
    (let ((ps (sphinx-doc-get-region sphinx-doc-fun-beg-regex
                                     sphinx-doc-fun-end-regex
                                     "forward")))
      (buffer-substring-no-properties (- (elt ps 0) 3) (- (elt ps 1) 1)))))


(defun sphinx-doc-exists? ()
  "Return whether the docstring already exists for a function."
  (save-excursion
    (search-forward-regexp sphinx-doc-fun-end-regex)
    (s-starts-with? "\"\"\"" (s-trim (sphinx-doc-current-line)))))


(defun sphinx-doc-existing ()
  "Return docstring of the function if it exists else nil."
  (when (sphinx-doc-exists?)
    (let* ((ps (sphinx-doc-get-region "\"\"\"" "\"\"\"" "forward"))
           (docstr (buffer-substring-no-properties (aref ps 0)
                                                   (- (aref ps 1) 3)))
           (indent (save-excursion
                     (search-forward-regexp sphinx-doc-fun-end-regex)
                     (sphinx-doc-current-indent))))
      (sphinx-doc-parse docstr indent))))


(defun sphinx-doc-kill-old-doc (indent)
  "Kill the old docstring for the current Python function.
INDENT is an integer representing the number of spaces the
function body is indented from the beginning of the line"
  (save-excursion
    (let ((ps (sphinx-doc-get-region "\"\"\"" "\"\"\"\\(?:\n\\)?" "forward")))
      (kill-region (- (elt ps 0) 3) (+ (elt ps 1) indent)))))


(defun sphinx-doc-insert-doc (doc)
  "Insert the DOC as string for the current Python function."
  (save-excursion
    (search-forward-regexp sphinx-doc-fun-end-regex)
    (forward-line -1)
    (move-end-of-line nil)
    (newline-and-indent)
    (insert (sphinx-doc-doc->str doc))))


(defun sphinx-doc-indent-doc (indent)
  "Indent docstring for the current function.
INDENT is the level of indentation"
  (save-excursion
    (let ((ps (sphinx-doc-get-region "\"\"\"" "\"\"\"" "forward")))
      (indent-rigidly (elt ps 0) (elt ps 1) indent))))


(defun sphinx-doc ()
  "Insert docstring for the Python function definition at point.
This is an interactive function and the docstring generated is as
per the requirement of Sphinx documentation generator."
  (interactive)
  (if (string= (thing-at-point 'word) "def")
      (back-to-indentation)
    (search-backward-regexp sphinx-doc-fun-beg-regex))
  (let ((fd (sphinx-doc-str->fndef (sphinx-doc-fndef-str))))
    (if fd
        (let ((indent (+ (sphinx-doc-current-indent) python-indent))
              (old-ds (sphinx-doc-existing))
              (new-ds (sphinx-doc-fndef->doc fd)))
          (progn
            (when old-ds (sphinx-doc-kill-old-doc indent))
            (sphinx-doc-insert-doc
             (if old-ds
                 (sphinx-doc-merge-docs old-ds new-ds)
               new-ds))
            (sphinx-doc-indent-doc indent)
            (search-forward "\"\"\""))))))


(defvar sphinx-doc-mode-map
  (let ((m (make-sparse-keymap)))
    (define-key m (kbd "C-c M-d") 'sphinx-doc)
    m))


;;;###autoload
(define-minor-mode sphinx-doc-mode
  "Sphinx friendly docstring generation for Python code."
  :init-value t
  :lighter " Spnxd"
  :keymap sphinx-doc-mode-map)


(provide 'sphinx-doc)

;;; sphinx-doc.el ends here
