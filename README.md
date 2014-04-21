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

This package can be installed from
[MELPA](http://melpa.milkbox.net/#/). To do so, run `M-x
package-list-packages`, then search for "sphinx-doc". Mark it with `i`
and finally press `x` to install.

To install manually, download and copy the `sphinx-doc.el` file
somewhere in the load path.


Configuration
-------------

Add the following line to your `.emacs` (or the equivalent config
file)

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
