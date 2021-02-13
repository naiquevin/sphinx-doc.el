;; tests for sphinx-doc.el

(require 'sphinx-doc)

(customize-set-variable 'sphinx-doc-include-types nil)


(ert-deftest sphinx-doc-test-str->arg ()
  (should (equal (sphinx-doc-str->arg "email")
                 (make-sphinx-doc-arg :name "email")))
  (should (equal (sphinx-doc-str->arg "domain='example.com'")
                 (make-sphinx-doc-arg :name "domain" :default "'example.com'")))
  (should (equal (sphinx-doc-str->arg "domain=\"example.com\"")
                 (make-sphinx-doc-arg :name "domain" :default "\"example.com\"")))
  (should (equal (sphinx-doc-str->arg "ignore = None")
                 (make-sphinx-doc-arg :name "ignore" :default "None"))))


(ert-deftest sphinx-doc-test-fndef->doc ()
  (should (equal (sphinx-doc-fndef->doc
                  (make-sphinx-doc-fndef :name "greet"
                                         :args (list (make-sphinx-doc-arg :name "name")
                                                     (make-sphinx-doc-arg :name "greeting" :default "'Hello'"))))
                 (make-sphinx-doc-doc :fields (list (make-sphinx-doc-field :key "param" :arg "name")
                                                    (make-sphinx-doc-field :key "param" :arg "greeting")
                                                    (make-sphinx-doc-field :key "returns")
                                                    ;; (make-sphinx-doc-field :key "rtype")
                                                    )))))


(ert-deftest sphinx-doc-test-fun-args ()
  (should (equal (sphinx-doc-fun-args "") '()))
  (should (equal (sphinx-doc-fun-args "name")
                 (list (make-sphinx-doc-arg :name "name"))))
  (should (equal (sphinx-doc-fun-args "name, email")
                 (list (make-sphinx-doc-arg :name "name") (make-sphinx-doc-arg :name "email"))))
  (should (equal (sphinx-doc-fun-args "name,email")
                 (list (make-sphinx-doc-arg :name "name") (make-sphinx-doc-arg :name "email"))))
  (should (equal (sphinx-doc-fun-args "name, email=None")
                 (list (make-sphinx-doc-arg :name "name") (make-sphinx-doc-arg :name "email" :default "None"))))
  (should (equal (sphinx-doc-fun-args "name, city='Mumbai', editor=\"emacs\"")
                 (list (make-sphinx-doc-arg :name "name")
                       (make-sphinx-doc-arg :name "city" :default "'Mumbai'")
                       (make-sphinx-doc-arg :name "editor" :default "\"emacs\""))))
  (should (equal (sphinx-doc-fun-args "self, name")
                 (list (make-sphinx-doc-arg :name "name"))))
  (should (equal (sphinx-doc-fun-args "name, *args, **kwargs")
                 (list (make-sphinx-doc-arg :name "name"))))
  (should (equal (sphinx-doc-fun-args "name, city='Mumbai',\n                      editor=\"emacs\"")
                 (list (make-sphinx-doc-arg :name "name")
                       (make-sphinx-doc-arg :name "city" :default "'Mumbai'")
                       (make-sphinx-doc-arg :name "editor" :default "\"emacs\"")))))


(ert-deftest sphinx-doc-test-field->str ()
  (should (string= (sphinx-doc-field->str (make-sphinx-doc-field :key "param" :arg "greeting"))
                   ":param greeting: "))
  (should (string= (sphinx-doc-field->str (make-sphinx-doc-field :key "param"
                                                                 :type "str"
                                                                 :arg "greeting"))
                   ":param str greeting: "))
  (should (string= (sphinx-doc-field->str (make-sphinx-doc-field :key "param"
                                                                 :type "pkg.module.Class"
                                                                 :arg "greeting"))
                   ":param pkg.module.Class greeting: "))
  (should (string= (sphinx-doc-field->str (make-sphinx-doc-field :key "rtype"))
                   ":rtype: ")))


(ert-deftest sphinx-doc-test-doc->str ()
  (let ((d1 (make-sphinx-doc-doc :summary "FIXME! briefly describe function"
                                 :before-fields ""
                                 :after-fields ""
                                 :fields (list (make-sphinx-doc-field :key "param" :arg "name")
                                               (make-sphinx-doc-field :key "returns")
                                               (make-sphinx-doc-field :key "rtype")
                                       )))
        (d2 (make-sphinx-doc-doc :summary "Just another function"
                                 :before-fields "This is some text before the fields section."
                                 :after-fields "This is some text after the fields section."
                                 :fields (list (make-sphinx-doc-field :key "param" :arg "name")
                                               (make-sphinx-doc-field :key "returns" :desc "constant 42")
                                               (make-sphinx-doc-field :key "rtype" :desc "integer")
                                               ))))
    (should (string= (sphinx-doc-doc->str d1)
                     "\"\"\"FIXME! briefly describe function\n\n:param name: \n:returns: \n:rtype: \n\n\"\"\""))
    (should (string= (sphinx-doc-doc->str d2)
                     "\"\"\"Just another function\n\nThis is some text before the fields section.\n\n:param name: \n:returns: constant 42\n:rtype: integer\n\nThis is some text after the fields section.\n\"\"\""))))


(ert-deftest sphinx-doc-test-parse ()
  (should (equal (sphinx-doc-parse "This is a docstring without params." 0)
                 (make-sphinx-doc-doc :summary "This is a docstring without params." :before-fields "" :after-fields "" :fields nil)))
  (should (equal (sphinx-doc-parse "FIXME! briefly describe function\n\n    :param name: \n    :returns: constant 42\n    :rtype: integer\n\n    " 4)
                 (make-sphinx-doc-doc :summary "FIXME! briefly describe function"
                                      :before-fields ""
                                      :after-fields ""
                                      :fields (list (make-sphinx-doc-field :key "param" :arg "name")
                                                    (make-sphinx-doc-field :key "returns" :desc "constant 42")
                                                    (make-sphinx-doc-field :key "rtype" :desc "integer")))))

  (should (equal (sphinx-doc-parse "FIXME! briefly describe function\n\n    :param name: \n    :returns: constant 42\n    :rtype: integer\n\n    " 4)
                 (make-sphinx-doc-doc :summary "FIXME! briefly describe function"
                                      :before-fields ""
                                      :after-fields ""
                                      :fields (list (make-sphinx-doc-field :key "param" :arg "name")
                                                    (make-sphinx-doc-field :key "returns" :desc "constant 42")
                                                    (make-sphinx-doc-field :key "rtype" :desc "integer")))))
  )


(ert-deftest sphinx-doc-test-lines->paras ()
  (should (equal
           (sphinx-doc-lines->paras
            '("Send message to a recipient as per the priority"
              ""
              "This is before comment. "
              ""
              "This is also before the comment but a second para"
              ""
              ":param str sender: email address of the sender"
              "                   this is the second line of sender param"
              "                   this is the third line of sender param"
              ":param str recipient: email address of the receiver"
              ":param str message_body: message to send"
              ":param int priority: priority"
              ":returns: nothing"
              ":rtype: None"
              ""
              "This is after comment" "" ""))
           '(("Send message to a recipient as per the priority")
             ("This is before comment. ")
             ("This is also before the comment but a second para")
             (":param str sender: email address of the sender"
              "                   this is the second line of sender param"
              "                   this is the third line of sender param"
              ":param str recipient: email address of the receiver"
              ":param str message_body: message to send"
              ":param int priority: priority"
              ":returns: nothing"
              ":rtype: None")
             ("This is after comment")))))


(ert-deftest sphinx-doc-test-str->field ()
  (should (equal
           (sphinx-doc-str->field ":param name: name of the recipient")
           (make-sphinx-doc-field :key "param"
                                  :arg "name"
                                  :desc "name of the recipient")))
  (should (equal
           (sphinx-doc-str->field ":param str name: name of the recipient")
           (make-sphinx-doc-field :key "param"
                                  :type "str"
                                  :arg "name"
                                  :desc "name of the recipient")))
  (should (equal
           (sphinx-doc-str->field ":param pkg.module.Class client: the client object")
           (make-sphinx-doc-field :key "param"
                                  :type "pkg.module.Class"
                                  :arg "client"
                                  :desc "the client object")))
  (should (equal
           (sphinx-doc-str->field ":rtype: NoneType")
           (make-sphinx-doc-field :key "rtype"
                                  :desc "NoneType"))))


(ert-deftest sphinx-doc-test-parse-fields ()
  (should (equal
           (sphinx-doc-parse-fields nil)
           '()))
  (should (equal
           (sphinx-doc-parse-fields
            (list ":param str sender: email address of the sender"
                  "                   this is the second line of sender param"
                  ":param str recipient: email address of the receiver"
                  ":param str message_body: message to send"
                  ":param priority: priority of message sending which is"
                  "                 is determined by joining date"
                  ":returns: this is a case where the return spans more"
                  "          than two lines. So I need to add a third line"
                  "          here and see if tests pass"
                  ":rtype: None"))
           (list (make-sphinx-doc-field :key "param" :type "str" :arg "sender" :desc "email address of the sender\n                   this is the second line of sender param")
                 (make-sphinx-doc-field :key "param" :type "str" :arg "recipient" :desc "email address of the receiver")
                 (make-sphinx-doc-field :key "param" :type "str" :arg "message_body" :desc "message to send")
                 (make-sphinx-doc-field :key "param" :type nil :arg "priority" :desc "priority of message sending which is\n                 is determined by joining date")
                 (make-sphinx-doc-field :key "returns" :type nil :arg nil :desc "this is a case where the return spans more\n          than two lines. So I need to add a third line\n          here and see if tests pass")
                 (make-sphinx-doc-field :key "rtype" :type nil :arg nil :desc "None"))))
  (should (equal
           (sphinx-doc-parse-fields
            (list ":param str sender: email address of the sender"
                  "                   this is the second line of sender param"
                  "                   this is the third line of sender param"
                  ":param int priority: priority"
                  ":returns:"
                  ":rtype: None"))
           (list (make-sphinx-doc-field :key "param" :type "str" :arg "sender" :desc "email address of the sender\n                   this is the second line of sender param\n                   this is the third line of sender param")
                 (make-sphinx-doc-field :key "param" :type "int" :arg "priority" :desc "priority")
                 (make-sphinx-doc-field :key "returns" :type nil :arg nil :desc "")
                 (make-sphinx-doc-field :key "rtype" :type nil :arg nil :desc "None")))))


(ert-deftest sphinx-doc-test-merge-fields ()
  (let ((t1-old (list (make-sphinx-doc-field :key "param" :type "str" :arg "name" :desc "This is name")
                      (make-sphinx-doc-field :key "returns" :type nil :arg nil :desc "constant 42")
                      (make-sphinx-doc-field :key "rtype" :type nil :arg nil :desc "integer")))

        (t1-new (list (make-sphinx-doc-field :key "param" :type nil :arg "name" :desc "")
                      (make-sphinx-doc-field :key "param" :type nil :arg "age" :desc "")
                      (make-sphinx-doc-field :key "returns" :type nil :arg nil :desc "")
                      (make-sphinx-doc-field :key "rtype" :type nil :arg nil :desc "")))

        (t2-old (list (make-sphinx-doc-field :key "param" :type nil :arg "name" :desc "name of the person")
                      (make-sphinx-doc-field :key "type" :type nil :arg "name" :desc "str")
                      (make-sphinx-doc-field :key "returns" :type nil :arg nil :desc "constant 42")
                      (make-sphinx-doc-field :key "rtype" :type nil :arg nil :desc "integer")
                      (make-sphinx-doc-field :key "raises" :type nil :arg nil :desc "KeyError")))

        (t2-new (list (make-sphinx-doc-field :key "param" :type nil :arg "name" :desc "")
                      (make-sphinx-doc-field :key "param" :type nil :arg "age" :desc "")
                      (make-sphinx-doc-field :key "returns" :type nil :arg nil :desc "")
                      (make-sphinx-doc-field :key "rtype" :type nil :arg nil :desc ""))))
    (should (equal (sphinx-doc-merge-fields t1-old t1-new)
                   (list (make-sphinx-doc-field :key "param" :arg "name" :type "str" :desc "This is name")
                         (make-sphinx-doc-field :key "param" :arg "age" :type nil :desc "")
                         (make-sphinx-doc-field :key "returns" :type nil :desc "constant 42")
                         (make-sphinx-doc-field :key "rtype" :type nil :desc "integer"))))

    (should (equal (sphinx-doc-merge-fields t2-old t2-new)
                   (list (make-sphinx-doc-field :key "param" :type nil :arg "name" :desc "name of the person")
                         (make-sphinx-doc-field :key "type" :type nil :arg "name" :desc "str")
                         (make-sphinx-doc-field :key "param" :type nil :arg "age" :desc "")
                         (make-sphinx-doc-field :key "returns" :type nil :arg nil :desc "constant 42")
                         (make-sphinx-doc-field :key "rtype" :type nil :arg nil :desc "integer")
                         (make-sphinx-doc-field :key "raises" :type nil :arg nil :desc "KeyError"))))))

(ert-deftest sphinx-doc-test-str->fndef ()
  (should (equal
           (sphinx-doc-str->fndef "def fun(a, b):")
           (make-sphinx-doc-fndef
            :name "fun"
            :args (list (make-sphinx-doc-arg :name "a")
                        (make-sphinx-doc-arg :name "b")))))

  (should (equal
           (sphinx-doc-str->fndef "def fun(a: int, b):")
           (make-sphinx-doc-fndef
            :name "fun"
            :args (list (make-sphinx-doc-arg :name "a" :type "int")
                        (make-sphinx-doc-arg :name "b")))))

  (should (equal
           (sphinx-doc-str->fndef "def fun(a, b: Optional[str]):")
           (make-sphinx-doc-fndef
            :name "fun"
            :args (list (make-sphinx-doc-arg :name "a")
                        (make-sphinx-doc-arg :name "b" :type "Optional[str]")))))

  (should (equal
           (sphinx-doc-str->fndef "def fun(a, b: Union[str, list]):")
           (make-sphinx-doc-fndef
            :name "fun"
            :args (list (make-sphinx-doc-arg :name "a")
                        (make-sphinx-doc-arg :name "b" :type "Union[str, list]")))))
  )
