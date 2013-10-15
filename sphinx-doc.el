;; Minor Mode for generating Sphinx friendly function docstrings for
;; Python (at the moment)


(defun current-line-string ()
  "Return current line as string"
  (buffer-substring (point-at-bol) (point-at-eol)))


(defun filter (condp lst)
  (delq nil
        (mapcar (lambda (x) (and (funcall condp x) x)) lst)))


(defun join-str (strs sep)
  (mapconcat 'identity strs sep))


(defconst sphinx-doc-fun-regex "^\s*def \\([a-zA-Z0-9_]+\\)(\\(.*\\)):$")


(defun sphinx-doc-fun-args (argstr)
  "Returns arguments from the Python function definition as a
  list. Note that args of type `self`, `*args` and `**kwargs`
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
  (when (string-match sphinx-doc-fun-regex string)
    (list (match-string 1 string)
          (sphinx-doc-fun-args (match-string 2 string)))))


(defun sphinx-doc-fun-sign (def)
  (let ((name (car def))
        (args (cadr def))
        (args-sign (lambda (args)
                     (join-str args ", "))))
    (concat ".. function:: " name "(" (funcall args-sign args) ")")))


(defun sphinx-doc-fun-fields (args)
  (let ((param-name (lambda (p)
                      (car (split-string p "=")))))
    (join-str (append
               (mapcar (lambda (arg)
                         (concat ":param " (funcall param-name arg) ": "))
                       args)
               (list (concat ":rtype: ")))
              "\n")))


(defun sphinx-doc-fun-comment (def)
  (join-str (list "\"\"\""
                  (sphinx-doc-fun-sign def)
                  ""
                  "<TODO: describe function here>"
                  ""
                  (sphinx-doc-fun-fields (cadr def))
                  ""
                  "\"\"\"")
            "\n"))


(defun sphinx-doc-with-region (srch-beg srch-end f)
  (save-excursion 
    (previous-line)
    (search-backward srch-beg)
    (next-line)
    (move-beginning-of-line nil)
    (let ((beg (point-at-bol)))
      (search-forward srch-end)
      (funcall f beg (point-at-eol)))))


(defun sphinx-doc-with-comment (f)
  (sphinx-doc-with-region "\"\"\"" "\"\"\"" f))


(defun sphinx-doc-with-fields (f)
  (sphinx-doc-with-region "<TODO:" ":rtype: " f))


(defun sphinx-doc-current-indent ()
  (save-excursion
    (let ((bti (progn (back-to-indentation) (point)))
          (bol (progn (beginning-of-line) (point))))
      (- bti bol))))


(defun sphinx-doc ()
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
            (search-backward "<TODO:"))))))

