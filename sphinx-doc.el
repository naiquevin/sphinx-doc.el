;;; sphinx-doc.el --- Sphinx friendly docstrings for Python functions

;; Copyright (c) 2013 <naikvin@gmail.com>

;; Author: Vineet Naik <naikvin@gmail.com>
;; URL: https://github.com/naiquevin/sphinx-doc.el
;; Version: 0.3.0
;; Keywords: Sphinx, Python
;; Package-Requires: ((s "1.9.0") (cl-lib "0.5") (dash "2.10.0"))

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
(require 'dash)
(require 's)


(defun sphinx-doc-current-line ()
  "Return current line as string."
  (buffer-substring-no-properties (point-at-bol) (point-at-eol)))


;; regular expression to identify a valid function definition in
;; python and match it's name and arguments.
(let ((fname-regex "\\([a-zA-Z0-9_]+\\)")
      (args-regex "(\\(\\(?:.\\|\n\\)*\\))")
      (return-type-regex "\\(?: -> \\([a-zA-Z0-9\\.]*\\)\\)?"))
  (defconst sphinx-doc-fun-regex
    (format "^ *def %1$s%2$s%3$s:$"
            fname-regex
            args-regex
            return-type-regex)))

;; regexes for beginning and end of python function definitions
(defconst sphinx-doc-fun-beg-regex "def")
(defconst sphinx-doc-fun-end-regex ":[^ ]\\(?:\n\\)?")

;; Variations for some field keys recognized by Sphinx
(defconst sphinx-doc-param-variants '("param" "parameter" "arg" "argument"
                                      "key" "keyword"))
(defconst sphinx-doc-raises-variants '("raises" "raise" "except" "exception"))
(defconst sphinx-doc-returns-variants '("returns" "return"))

(defcustom sphinx-doc-python-indent t
  "If non-nil, the docstring will be indented.")
(defcustom sphinx-doc-include-types nil
  "If non-nil, the docstring will also include the type.")

;; struct definitions

(cl-defstruct sphinx-doc-arg
  name      ; name of the arg
  default   ; optional default value if specified
  type)     ; optional type


(cl-defstruct sphinx-doc-fndef
  name  ; name of the function
  args) ; list of arg objects


(cl-defstruct sphinx-doc-field
  key        ; one of the allowed field name keyword
  type       ; optional datatype
  arg        ; optional argument
  (desc "")) ; description

;; Note about various types of reST fields recognized by Sphinx and
;; how they are represented using the `sphinx-doc-field` struct
;; above. The `key` should be non-nil in all since that's how they are
;; identified:
;;
;; 1. param: All params must have a valid `arg` whereas `type` is
;;           optional and `desc` will initially be an empty string
;; 2. type: Must have valid `arg`
;; 3. rtype: Must NOT have `type` or `arg`
;; 4. returns: Must NOT have `type` or `arg`
;; 5. raises: Must have a valid `arg`
;;
;; See Also: http://sphinx-doc.org/domains.html#info-field-lists


(cl-defstruct sphinx-doc-doc
  (summary "TODO describe function") ; summary line that fits on the first line
  before-fields                      ; list of comments before fields
  after-fields                       ; list of comments after fields
  fields)                            ; list of field objects


(defun sphinx-doc-str->arg (s)
  "Build an arg object from string S."
  (let* ((split-on-equal (mapcar #'s-trim (split-string s "=")))
         (default (-second-item split-on-equal))
         (split-on-colon
          (mapcar #'s-trim (split-string (-first-item split-on-equal) ":")))
         (type (-second-item split-on-colon))
         (name (-first-item split-on-colon)))
    (make-sphinx-doc-arg :name name
                         :default default
                         :type type)))

(defun sphinx-doc-arg->fields (a)
  (let* ((arg-name (sphinx-doc-arg-name a))
         (param-field
          (make-sphinx-doc-field
           :key "param"
           :arg arg-name))
         (type-str (or (sphinx-doc-arg-type a) ""))
         (type-field (make-sphinx-doc-field
                      :key "type"
                      :arg arg-name
                      :desc type-str)))
    (if sphinx-doc-include-types
        (list param-field type-field)
      (list param-field))))


(defun sphinx-doc-fndef->doc (f)
  "Build a doc object solely from fndef F."
  (make-sphinx-doc-doc
   :fields (append
            (-mapcat 'sphinx-doc-arg->fields
                     (sphinx-doc-fndef-args f))
             (list (make-sphinx-doc-field :key "returns")
                   ;;(make-sphinx-doc-field :key "rtype")
                   ))))

(defun sphinx-doc-split-args (input)
  "Like (split-string input \",\") but don't split on coma inside type hints"
  (let ((bracket_counter 0)
        (cursor -1)
        (strings '()))
    (dotimes (i (length input))
      (let ((char (aref input i)))
        (cond
         ((= char ?\[)
          (setq bracket_counter (1+ bracket_counter)))
         ((= char ?\])
          (setq bracket_counter (1- bracket_counter)))
         ((and (= char ?,) (= bracket_counter 0))
          (setq strings (append strings (list (substring input (1+ cursor) i))))
          (setq cursor i))
         )))
    (setq strings (append strings (list (substring input (1+ cursor)))))
    ))

(defun sphinx-doc-fun-args (argstrs)
  "Extract list of arg objects from string ARGSTRS.
ARGSTRS is the string representing function definition in Python.
Note that the arguments self, *args and **kwargs are ignored. Also empty strings are
  ignored, to handle the final trailing comma."
  (message argstrs)
  (when (not (string= argstrs ""))
    (mapcar #'sphinx-doc-str->arg
            (-filter
             (lambda (str)
               (and (not (string= str ""))
                    (not (string= (substring str 0 1) "*"))
                    (not (string= str "self"))))
             (mapcar #'s-trim
                     (sphinx-doc-split-args argstrs))))))


(defun sphinx-doc-str->fndef (s)
  "Build a fndef object from string S.
S is a string representation of the python function definition
Returns nil if string is not a function definition."
  (if (string-match sphinx-doc-fun-regex s)
    (make-sphinx-doc-fndef
     :name (match-string 1 s)
     :args (sphinx-doc-fun-args (match-string 2 s)))
    (message (format "Failed to parse function definition '%s'." s))
    nil))


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
   (-filter
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
            (sphinx-doc-doc-after-fields ds))
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
                       (-take-while comment? (cdr paras)))
       :after-fields (sphinx-doc-paras->str
                      (cdr (-drop-while comment? (cdr paras))))
       :fields (sphinx-doc-parse-fields
                (car (-filter field-para? paras)))))))


(defun sphinx-doc-paras->str (paras)
  "Convert PARAS to string.
PARAS are list of paragraphs (which in turn are list of lines).
This is done by adding a newline between two lines of each para
and a blank line between each para."
  (s-join
   ""
   (apply #'append
          (-interpose '("\n\n")
                      (mapcar (lambda (p)
                                (-interpose "\n" p))
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


(defun sphinx-doc-str->field (s)
  "Parse a single field into field object.
Argument S represents a single field in the fields paragraph of
the docstring."
  (cond ((string-match "^:\\([a-z]+\\) \\([a-z.]+\\) \\([a-zA-Z0-9_]+\\):\s?\\(.*\\(?:\n\s*.*\\)*\\)$" s)
         (make-sphinx-doc-field :key (match-string 1 s)
                                :type (match-string 2 s)
                                :arg (match-string 3 s)
                                :desc (match-string 4 s)))
        ((string-match "^:\\([a-z]+\\) \\([a-zA-Z0-9_]+\\):\s?\\(.*\\(?:\n\s*.*\\)*\\)$" s)
         (make-sphinx-doc-field :key (match-string 1 s)
                                :arg (match-string 2 s)
                                :desc (match-string 3 s)))
        ((string-match "^:\\([a-z]+\\):\s?\\(.*\\(?:\n\s*.*\\)*\\)$" s)
         (make-sphinx-doc-field :key (match-string 1 s)
                                :desc (match-string 2 s)))))


(defun sphinx-doc-parse-fields (fields-para)
  "Parse FIELDS-PARA into list of field objects.
FIELDS-PARA is the fields section of the docstring."
  (when fields-para
    (mapcar #'sphinx-doc-str->field
            (mapcar (lambda (s)
                      (if (s-starts-with? ":" s) s (concat ":" s)))
                    (split-string (s-join "\n" fields-para) "\n:")))))


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


(defun sphinx-doc-select-fields (keys fields)
  "Return subset of fields with select keys.
KEYS is a list of strings and FIELDS is a list of field objects."
  (-filter (lambda (f)
             (member (sphinx-doc-field-key f) keys))
           fields))


(defun sphinx-doc-field-map (fields)
  "Create a mapping of field arg with the field for all FIELDS."
  (mapcar (lambda (f) (cons (sphinx-doc-field-arg f) f)) fields))


(defun sphinx-doc-field-map-get (key mapping)
  "Return the value in the field mapping for the key or nil.
KEY is a string and MAPPING is an associative list."
  (cdr (assoc key mapping)))


(defun sphinx-doc-merge-fields (old new)
  "Merge old and new fields together.
OLD is the list of old field objects, NEW is the list of new
field objects."
  (let ((param-map (sphinx-doc-field-map
                    (sphinx-doc-select-fields sphinx-doc-param-variants old)))
        (type-map (sphinx-doc-field-map
                   (sphinx-doc-select-fields '("type") old)))
        (fixed-fields (sphinx-doc-select-fields
                       (cons "rtype" (append sphinx-doc-returns-variants
                                             sphinx-doc-raises-variants))
                       old)))
    (append (-mapcat
             (lambda (f)
               (let* ((key (sphinx-doc-field-arg f))
                      (param (sphinx-doc-field-map-get key param-map))
                      (type (sphinx-doc-field-map-get key type-map)))
                 (cond ((and param type) (list param type))
                       (param (list param))
                       (t (list f)))))
             (sphinx-doc-select-fields sphinx-doc-param-variants new))
            fixed-fields)))


;; Note: Following few functions (those using `save-excursion`) must
;; be invoked only when the cursor is on the function definition line.

(defun sphinx-doc-get-region (srch-beg srch-end)
  "Return the beginning and end points of a region by searching.
SRCH-BEG and SRCH-END are the chars to search for."
  (save-excursion
    (search-forward-regexp srch-beg)
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
                                     sphinx-doc-fun-end-regex)))
      (buffer-substring-no-properties (- (elt ps 0) 3) (- (elt ps 1) 1)))))


(defun sphinx-doc-exists? ()
  "Return whether the docstring already exists for a function."
  (save-excursion
    (search-forward-regexp sphinx-doc-fun-end-regex)
    (s-starts-with? "\"\"\"" (s-trim (sphinx-doc-current-line)))))


(defun sphinx-doc-existing ()
  "Return docstring of the function if it exists else nil."
  (when (sphinx-doc-exists?)
    (let* ((ps (sphinx-doc-get-region "\"\"\"" "\"\"\""))
           (docstr (buffer-substring-no-properties (elt ps 0)
                                                   (- (elt ps 1) 3)))
           (indent (save-excursion
                     (search-forward-regexp sphinx-doc-fun-end-regex)
                     (sphinx-doc-current-indent))))
      (sphinx-doc-parse docstr indent))))


(defun sphinx-doc-kill-old-doc (indent)
  "Kill the old docstring for the current Python function.
INDENT is an integer representing the number of spaces the
function body is indented from the beginning of the line"
  (save-excursion
    (let ((ps (sphinx-doc-get-region "\"\"\"" "\"\"\"\\(?:\n\\)?")))
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
    (let ((ps (sphinx-doc-get-region "\"\"\"" "\"\"\"")))
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
        (let ((indent (+ (sphinx-doc-current-indent) sphinx-doc-python-indent))
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
  :init-value nil
  :lighter " Spnxd"
  :keymap sphinx-doc-mode-map
  (when sphinx-doc-mode ; ON
    (set (make-local-variable 'sphinx-doc-python-indent)
         (cond ((boundp 'python-indent-offset)
                python-indent-offset)
               ((boundp 'python-indent)
                python-indent)
               (t 4)))))


(provide 'sphinx-doc)

;;; sphinx-doc.el ends here
