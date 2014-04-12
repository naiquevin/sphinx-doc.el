;;; sphinx-doc.el -- Generate Sphinx friendly docstrings for Python
;;; functions.

;; Author: Vineet Naik <naikvin@gmail.com>
;; Created: 14th October 2013

;; License: MIT <http://opensource.org/licenses/MIT>

;;; Commentary:

;; This file just provides a function `sphinx-doc` that can be used to
;; generate skeleton of docstrings for Python functions. The structure
;; of the docstring is according to Sphinx documentation generator
;; <http://sphinx-doc.org/index.html>

;;; Installation:

;; Copy this file somewhere in your path and add the following lines
;; to your .emacs (or the equivalent config file)
;;
;; (require 'sphinx-doc)
;;
;; (add-hook 'python-mode-hook (lambda () (local-set-key (kbd "C-c M-d") #'sphinx-doc)))
;;
;; This will bind the function `sphinx-doc` to `C-c M-d`. Choose this
;; as per your liking.

;;; Usage:

;; In a Python file, move the cursor the function definition and type
;; `C-c M-d` (or the key binding you have chosen). Alternatively,
;; execute the function using `M-x sphinx-doc RET`

;; include libs

(require 'cl)

;; 3rd party deps
(require 's)


;; some good-to-have helper functions

(defun current-line-string ()
  "Return current line as string"
  (buffer-substring (point-at-bol) (point-at-eol)))


(defun filter (condp lst)
  (delq nil
        (mapcar (lambda (x) (and (funcall condp x) x)) lst)))


(defun join-str (strs sep)
  "Joins list of strings `strs` together by the separator `sep`"
  (mapconcat 'identity strs sep))


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
  (desc "")                                    ; more elaborate description
  fields                                       ; list of field objects
  fndef)                                       ; the current defn object


(defun sphinx-doc-str->arg (s)
  "Builds an arg object from string"
  (let ((parts (mapcar #'s-trim (split-string s "="))))
    (if (cdr parts)
        (make-arg :name (car parts)
                  :default (cadr parts))
      (make-arg :name (car parts)))))


(defun sphinx-doc-arg->field (a)
  "Converts an arg object to a field object"
  (make-field :key "param"
              :arg (arg-name a)))


(defun sphinx-doc-fndef->doc (f)
  (make-doc :fields (append
                     (mapcar #'sphinx-doc-arg->field (fndef-args f))
                     (list (make-field :key "returns")
                           (make-field :key "rtype")))
            :fndef f))


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


(defun sphinx-doc-fun-def (s)
  "Returns a fndef object from the python function definition
  represented by the input string"
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


(defun sphinx-doc->str (ds)
  "Converts a doc object into it's string representation"
  (join-str
   (list (s-format "\"\"\"$0" 'elt (list (doc-summary ds)))
         ""
         (join-str (mapcar #'sphinx-doc-field->str
                           (doc-fields ds))
                   "\n")
         ""
         "\"\"\"")
   "\n"))


(defun sphinx-doc-with-region (srch-beg srch-end f)
  "Selects a region by searching for the beginning expression
  `srch-beg` and the end expression `srch-end` and executes the
  function `f` on the region. Finally returns the cursor to the
  initial position"
  (save-excursion
    (previous-line)
    (search-backward srch-beg)
    (next-line)
    (move-beginning-of-line nil)
    (let ((beg (point-at-bol)))
      (search-forward srch-end)
      (funcall f beg (point-at-eol)))))


(defun sphinx-doc-with-comment (f)
  "Selects the comment and runs the function `f` on region"
  (sphinx-doc-with-region "\"\"\"" "\"\"\"" f))


(defun sphinx-doc-with-fields (f)
  "Selects the field info section of the comment and executes the
  function `f` on the region"
  (sphinx-doc-with-region "FIXME!" ":rtype: " f))


(defun sphinx-doc-current-indent ()
  "Returns the indentation level of the current line, ie. by how
  many number of spaces the current line is indented"
  (save-excursion
    (let ((bti (progn (back-to-indentation) (point)))
          (bol (progn (beginning-of-line) (point))))
      (- bti bol))))


(defun sphinx-doc ()
  "Interactive command to insert docstring skeleton for the
  function definition at point"
  (interactive)
  (when (string= major-mode "python-mode")
    (let ((fd (sphinx-doc-fun-def (current-line-string))))
      (if fd
          (let ((curr-indent (sphinx-doc-current-indent))
                (new-ds (sphinx-doc-fndef->doc fd)))
            (progn
              (move-end-of-line nil)
              (newline-and-indent)
              (insert (sphinx-doc->str new-ds))
              (sphinx-doc-with-comment
               (lambda (b e)
                 (indent-rigidly b e (+ curr-indent python-indent))))
              (search-backward "FIXME!")))))))


(provide 'sphinx-doc)

;; sphinx-doc.el ends
