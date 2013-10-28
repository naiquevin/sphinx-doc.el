sphinx-doc.el
=============

What?
-----

This `sphinx-doc.el` file in this repo provides a function
`sphinx-doc` that can be used to generate skeleton of docstrings for
Python functions. The structure of the docstring is according to
Sphinx documentation generator <http://sphinx-doc.org/index.html>


Installation
------------

Copy this file somewhere in your path and add the following lines
to your .emacs (or the equivalent config file)

```elisp
    ;; sphinx-doc
    (require 'sphinx-doc)
    (add-hook 'python-mode-hook
              (lambda () (local-set-key (kbd "C-c M-d") #'sphinx-doc)))
```

This will bind the function `sphinx-doc` to `C-c M-d`. Choose the key
binding as per your liking.


Usage
-----

In a Python file, move the cursor on any function or method definition
and type `C-c M-d` (or the key binding you have chosen).
Alternatively, execute the function using `M-x sphinx-doc RET`


Demo
----

![Sphinx-doc](../master/demo.gif?raw=true)


License
-------

[MIT](http://opensource.org/licenses/MIT)



