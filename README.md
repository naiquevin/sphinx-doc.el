sphinx-doc.el
=============

What is it?
-----------

``sphinx-doc`` is an emacs minor mode for inserting docstring skeleton
for Python functions and methods. The structure of the docstring is as
per the requirement of the
[Sphinx documentation generator](http://sphinx-doc.org/index.html).


Installation
------------

This package is yet to be added on
[MELPA](http://melpa.milkbox.net/#/). Until then, it can be manually
installed by copying the `sphinx-doc.el` file somewhere in the load
path and adding the following lines to your `.emacs` (or the
equivalent config file)

```elisp
    (add-hook 'python-mode-hook (lambda () (require 'sphinx-doc)))
```

This will enable the `sphinx-doc-mode` and bind the interactive
function `sphinx-doc` to `C-c M-d`.


Usage
-----

Inside a Python file, move the cursor to some function/method
definition and hit `C-c M-d` (see gif demo below).


Demo
----

![Sphinx-doc](../master/demo.gif?raw=true)


See also
--------

The [autodoc extension](http://sphinx-doc.org/ext/autodoc.html) needs
to be enabled for the sphinx instance in order to generate
documentation from the docstrings.


License
-------

[MIT](http://opensource.org/licenses/MIT)
