;; tests for sphinx-doc.el

(require 'sphinx-doc)


(ert-deftest sphinx-doc-test-fun-args ()
  (assert (equal (sphinx-doc-fun-args "") '()))
  (assert (equal (sphinx-doc-fun-args "name")
                 (list (make-arg :name "name"))))
  (assert (equal (sphinx-doc-fun-args "name, email")
                 (list (make-arg :name "name") (make-arg :name "email"))))
  (assert (equal (sphinx-doc-fun-args "name,email")
                 (list (make-arg :name "name") (make-arg :name "email"))))
  (assert (equal (sphinx-doc-fun-args "name, email=None")
                 (list (make-arg :name "name") (make-arg :name "email" :default "None"))))
  (assert (equal (sphinx-doc-fun-args "name, email = None")
                 (list (make-arg :name "name") (make-arg :name "email" :default "None"))))
  (assert (equal (sphinx-doc-fun-args "name, city='Mumbai', editor=\"emacs\"")
                 (list (make-arg :name "name")
                       (make-arg :name "city" :default "'Mumbai'")
                       (make-arg :name "editor" :default "\"emacs\""))))
  (assert (equal (sphinx-doc-fun-args "self, name")
                 (list (make-arg :name "name"))))
  (assert (equal (sphinx-doc-fun-args "name, *args, **kwargs")
                 (list (make-arg :name "name")))))


(ert-deftest sphinx-doc-test-field->str ()
  (assert (string= (sphinx-doc-field->str (make-field :key "param" :arg "greeting"))
                   ":param greeting: "))
  (assert (string= (sphinx-doc-field->str (make-field :key "param"
                                                      :type "str"
                                                      :arg "greeting"))
                   ":param str greeting: "))
  (assert (string= (sphinx-doc-field->str (make-field :key "rtype"))
                   ":rtype: ")))
