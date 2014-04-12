;; tests for sphinx-doc.el

(require 'sphinx-doc)


(ert-deftest sphinx-doc-test-fun-args ()
  (assert (equal (sphinx-doc-fun-args "") '()))
  (assert (equal (sphinx-doc-fun-args "name") '("name")))
  (assert (equal (sphinx-doc-fun-args "name, email") '("name" "email")))
  (assert (equal (sphinx-doc-fun-args "name,email") '("name" "email")))
  (assert (equal (sphinx-doc-fun-args "name, email=None") '("name" "email=None")))
  (assert (equal (sphinx-doc-fun-args "self, name") '("name")))
  (assert (equal (sphinx-doc-fun-args "name, *args, **kwargs") '("name"))))


(ert-deftest sphinx-doc-test-fun-def ()
  (assert (equal (sphinx-doc-fun-def "def greet(greeting, name='world'):")
                 '("greet" ("greeting" "name='world'"))))
  (assert (equal (sphinx-doc-fun-def "def greet(greeting, name=\"world\"):")
                 '("greet" ("greeting" "name=\"world\"")))))


(ert-deftest sphinx-doc-test-fun-fields ()
  (assert (equal (sphinx-doc-fun-fields '("greeting" "name=\"world\""))
                 ":param greeting: \n:param name: \n:returns: \n:rtype: "))
  (assert (equal (sphinx-doc-fun-fields '())
                 ":returns: \n:rtype: ")))


