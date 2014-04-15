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
  (buffer-substring-no-properties (point-at-bol) (point-at-eol)))


(defun filter (condp lst)
  (delq nil
        (mapcar (lambda (x) (and (funcall condp x) x)) lst)))


(defun join-str (strs sep)
  "Joins list of strings `strs` together by the separator `sep`"
  (mapconcat 'identity strs sep))


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


; (gv-define-simple-setter doc )


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
   (filter
    (lambda (x) (not (equal x nil)))
    (list (s-format "\"\"\"$0\n" 'elt (list (doc-summary ds)))
          (when (not (string= (doc-before-fields ds) ""))
            (concat (doc-before-fields ds) "\n"))
          (join-str (mapcar #'sphinx-doc-field->str (doc-fields ds)) "\n")
          ""
          (when (not (string= (doc-after-fields ds) ""))
            (concat (doc-after-fields ds) "\n"))
          "\"\"\""))
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


(defun sphinx-get-region (srch-beg srch-end direction)
  (save-excursion
    (if (string= direction "forward")
        (search-forward srch-beg)
      (search-backward srch-beg))
    (let ((beg (point)))
      (search-forward srch-end)
      (vector beg (point)))))


(defun sphinx-doc-with-comment (f)
  "Selects the comment and runs the function `f` on region"
  (sphinx-doc-with-region "\"\"\"" "\"\"\"" f))


(defun sphinx-doc-current-indent ()
  "Returns the indentation level of the current line, ie. by how
  many number of spaces the current line is indented"
  (save-excursion
    (let ((bti (progn (back-to-indentation) (point)))
          (bol (progn (beginning-of-line) (point))))
      (- bti bol))))


(defun sphinx-doc-exists? ()
  (save-excursion
    (next-line)
    (s-starts-with? "\"\"\"" (s-trim (current-line-string)))))


(defun sphinx-doc-existing ()
  (when (sphinx-doc-exists?)
    (let* ((ps (sphinx-get-region "\"\"\"" "\"\"\"" "forward"))
           (docstr (buffer-substring-no-properties (aref ps 0)
                                                   (- (aref ps 1) 3))))
      (sphinx-doc-parse docstr))))


(defun sphinx-doc-parse (docstr)
  (let* ((indent (save-excursion
                   (next-line)
                   (sphinx-doc-current-indent)))
         (lines (mapcar (lambda (line)
                          (s-chop-prefix (make-string indent 32) line))
                        (split-string docstr "\n")))
         (paras (sphinx-doc-lines->paras lines))
         (field-para? #'(lambda (p) (s-starts-with? ":" (car p)))))
    (make-doc :summary (caar paras)
              :before-fields (sphinx-doc-paras->str
                              (take-while field-para? (cdr paras)))
              :after-fields (sphinx-doc-paras->str
                             (cdr (drop-while field-para? (cdr paras))))
              :fields (sphinx-doc-parse-fields (car (filter field-para? paras))))))


(defun sphinx-doc-paras->str (paras)
  "Converts a list of paras (which in turn is a list of lines) to
  text. This is done by adding a newline between two lines of
  each para and a blank line between each para"
  (join-str
   (apply #'append
          (interpose '("\n\n")
                     (mapcar (lambda (p)
                               (interpose "\n" p))
                             paras)))
   ""))


(defun sphinx-doc-lines->paras (lines)
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
           (split-string (join-str fields-para "\n") "\n:"))))


(defun sphinx-doc-merge-docs (old new)
  (make-doc :summary (doc-summary old)
            :before-fields (doc-before-fields old)
            :after-fields (doc-after-fields old)
            :fields (sphinx-doc-merge-fields
                     (doc-fields old)
                     (doc-fields new))))


(defun sphinx-doc-merge-fields (old new)
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


(defun sphinx-doc-kill-old-doc ()
  (save-excursion
    (let ((ps (sphinx-get-region "\"\"\"" "\"\"\"" "forward")))
      (kill-region (- (elt ps 0) 3) (elt ps 1))
      (next-line)
      (beginning-of-line)
      (kill-line))))


(defun sphinx-doc ()
  "Interactive command to insert docstring skeleton for the
  function definition at point"
  (interactive)
  (when (string= major-mode "python-mode")
    (let ((fd (sphinx-doc-fun-def (current-line-string))))
      (if fd
          (let ((curr-indent (sphinx-doc-current-indent))
                (old-ds (sphinx-doc-existing))
                (new-ds (sphinx-doc-fndef->doc fd)))
            (progn
              (when old-ds (sphinx-doc-kill-old-doc))
              (move-end-of-line nil)
              (newline-and-indent)
              (insert
               (sphinx-doc->str
                (if old-ds (sphinx-doc-merge-docs old-ds new-ds) new-ds)))
              (sphinx-doc-with-comment
               (lambda (b e)
                 (indent-rigidly b e (+ curr-indent python-indent))))
              (dotimes (i 2) (search-backward "\"\"\""))
              (dotimes (i 3) (forward-char))))))))


(provide 'sphinx-doc)

;; sphinx-doc.el ends
