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


(defun sphinx-doc-fun-args (argstr)
  "Returns arguments from the Python function definition as a
  string. Note that args of type `self`, `*args` and `**kwargs`
  will be ignored"
  (if (string= argstr "")
      '()
    (filter (lambda (str)
              (and (not (string= (substring str 0 1) "*"))
                   (not (string= str "self"))))
            (mapcar (lambda (str)
                      (replace-regexp-in-string
                       "\\`[ \t\n]*" ""
                       str))
                    (split-string  argstr ",")))))


(defun sphinx-doc-fun-def (string)
  "Returns a pair of name of the function and list of the name of
  the arguments"
  (when (string-match sphinx-doc-fun-regex string)
    (list (match-string 1 string)
          (sphinx-doc-fun-args (match-string 2 string)))))


(defun sphinx-doc-fun-fields (args)
  "Returns field info as per sphinx doc as string"
  (let ((param-name (lambda (p)
                      (car (split-string p "=")))))
    (join-str (append
               (mapcar (lambda (arg)
                         (concat ":param " (funcall param-name arg) ": "))
                       args)
               (list (concat ":rtype: ")))
              "\n")))


(defun sphinx-doc-fun-comment (def)
  "Returns docstring skeleton as string from function
  definition (pair of name and args)"
  (join-str (list "\"\"\"FIXME! briefly describe function"
                  ""
                  (sphinx-doc-fun-fields (cadr def))
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
    (let ((fndef (sphinx-doc-fun-def (current-line-string)))
          (curr-indent (sphinx-doc-current-indent)))
      (if fndef
          (progn
            (move-end-of-line nil)
            (newline-and-indent)
            (insert (sphinx-doc-fun-comment fndef))
            (sphinx-doc-with-comment
             (lambda (b e)
               (indent-rigidly b e (+ curr-indent python-indent))))
            (sphinx-doc-with-fields
             (lambda (b e)
               (align-regexp b e "\\(\\s-*\\): ")))
            (search-backward "FIXME!"))))))


(provide 'sphinx-doc)

;; sphinx-doc.el ends
