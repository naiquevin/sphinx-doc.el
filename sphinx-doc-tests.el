;; tests for sphinx-doc.el

(require 'sphinx-doc)


(ert-deftest sphinx-doc-test-str->arg ()
  (cl-assert (equal (sphinx-doc-str->arg "email")
                    (make-sphinx-doc-arg :name "email")))
  (cl-assert (equal (sphinx-doc-str->arg "domain='example.com'")
                    (make-sphinx-doc-arg :name "domain" :default "'example.com'")))
  (cl-assert (equal (sphinx-doc-str->arg "domain=\"example.com\"")
                    (make-sphinx-doc-arg :name "domain" :default "\"example.com\"")))
  (cl-assert (equal (sphinx-doc-str->arg "ignore = None")
                    (make-sphinx-doc-arg :name "ignore" :default "None"))))


(ert-deftest sphinx-doc-test-fndef->doc ()
  (cl-assert (equal (sphinx-doc-fndef->doc
                     (make-sphinx-doc-fndef :name "greet"
                                            :args (list (make-sphinx-doc-arg :name "name")
                                                        (make-sphinx-doc-arg :name "greeting" :default "'Hello'"))))
                    (make-sphinx-doc-doc :fields (list (make-sphinx-doc-field :key "param" :arg "name")
                                                       (make-sphinx-doc-field :key "param" :arg "greeting")
                                                       (make-sphinx-doc-field :key "returns")
                                                       (make-sphinx-doc-field :key "rtype"))))))


(ert-deftest sphinx-doc-test-fun-args ()
  (cl-assert (equal (sphinx-doc-fun-args "") '()))
  (cl-assert (equal (sphinx-doc-fun-args "name")
                    (list (make-sphinx-doc-arg :name "name"))))
  (cl-assert (equal (sphinx-doc-fun-args "name, email")
                    (list (make-sphinx-doc-arg :name "name") (make-sphinx-doc-arg :name "email"))))
  (cl-assert (equal (sphinx-doc-fun-args "name,email")
                    (list (make-sphinx-doc-arg :name "name") (make-sphinx-doc-arg :name "email"))))
  (cl-assert (equal (sphinx-doc-fun-args "name, email=None")
                    (list (make-sphinx-doc-arg :name "name") (make-sphinx-doc-arg :name "email" :default "None"))))
  (cl-assert (equal (sphinx-doc-fun-args "name, city='Mumbai', editor=\"emacs\"")
                    (list (make-sphinx-doc-arg :name "name")
                          (make-sphinx-doc-arg :name "city" :default "'Mumbai'")
                          (make-sphinx-doc-arg :name "editor" :default "\"emacs\""))))
  (cl-assert (equal (sphinx-doc-fun-args "self, name")
                    (list (make-sphinx-doc-arg :name "name"))))
  (cl-assert (equal (sphinx-doc-fun-args "name, *args, **kwargs")
                    (list (make-sphinx-doc-arg :name "name"))))
  (cl-assert (equal (sphinx-doc-fun-args "name, city='Mumbai',\n                      editor=\"emacs\"")
                    (list (make-sphinx-doc-arg :name "name")
                          (make-sphinx-doc-arg :name "city" :default "'Mumbai'")
                          (make-sphinx-doc-arg :name "editor" :default "\"emacs\"")))))


(ert-deftest sphinx-doc-test-field->str ()
  (cl-assert (string= (sphinx-doc-field->str (make-sphinx-doc-field :key "param" :arg "greeting"))
                      ":param greeting: "))
  (cl-assert (string= (sphinx-doc-field->str (make-sphinx-doc-field :key "param"
                                                                    :type "str"
                                                                    :arg "greeting"))
                      ":param str greeting: "))
  (cl-assert (string= (sphinx-doc-field->str (make-sphinx-doc-field :key "rtype"))
                      ":rtype: ")))


(ert-deftest sphinx-doc-test-doc->str ()
  (let ((d1 [cl-struct-sphinx-doc-doc "FIXME! briefly describe function" nil nil
                                      ([cl-struct-sphinx-doc-field "param" nil "name" ""]
                                       [cl-struct-sphinx-doc-field "returns" nil nil ""]
                                       [cl-struct-sphinx-doc-field "rtype" nil nil ""])])
        (d2 [cl-struct-sphinx-doc-doc "Just another function"
                                      "This is some text before the fields section."
                                      "This is some text after the fields section."
                                      ([cl-struct-sphinx-doc-field "param" nil "name" ""]
                                       [cl-struct-sphinx-doc-field "returns" nil nil "constant 42"]
                                       [cl-struct-sphinx-doc-field "rtype" nil nil "integer"])]))
    (cl-assert (string= (sphinx-doc-doc->str d1)
                        "\"\"\"FIXME! briefly describe function\n\n:param name: \n:returns: \n:rtype: \n\n\"\"\""))
    (cl-assert (string= (sphinx-doc-doc->str d2)
                        "\"\"\"Just another function\n\nThis is some text before the fields section.\n\n:param name: \n:returns: constant 42\n:rtype: integer\n\nThis is some text after the fields section.\n\n\"\"\""))))


(ert-deftest sphinx-doc-test-parse ()
  (cl-assert (equal (sphinx-doc-parse "FIXME! briefly describe function\n\n    :param name: \n    :returns: constant 42\n    :rtype: integer\n\n    " 4)
                    (make-sphinx-doc-doc :summary "FIXME! briefly describe function"
                                         :before-fields ""
                                         :after-fields ""
                                         :fields (list (make-sphinx-doc-field :key "param" :arg "name")
                                                       (make-sphinx-doc-field :key "returns" :desc "constant 42")
                                                       (make-sphinx-doc-field :key "rtype" :desc "integer"))))))


(ert-deftest sphinx-doc-test-lines->paras ()
  (cl-assert (equal
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


(ert-deftest sphinx-doc-test-parse-fields ()
  (cl-assert (equal
              (sphinx-doc-parse-fields
               '(":param str sender: email address of the sender"
                 "                   this is the second line of sender param"
                 ":param str recipient: email address of the receiver"
                 ":param str message_body: message to send"
                 ":param priority: priority of message sending which is"
                 "                 is determined by joining date"
                 ":returns: this is a case where the return spans more"
                 "          than two lines. So I need to add a third line"
                 "          here and see if tests pass"
                 ":rtype: None"))
              '([cl-struct-sphinx-doc-field "param" "str" "sender" "email address of the sender\n                   this is the second line of sender param"]
                [cl-struct-sphinx-doc-field "param" "str" "recipient" "email address of the receiver"]
                [cl-struct-sphinx-doc-field "param" "str" "message_body" "message to send"]
                [cl-struct-sphinx-doc-field "param" nil "priority" "priority of message sending which is\n                 is determined by joining date"]
                [cl-struct-sphinx-doc-field "returns" nil nil "this is a case where the return spans more\n          than two lines. So I need to add a third line\n          here and see if tests pass"]
                [cl-struct-sphinx-doc-field "rtype" nil nil "None"])))
  (cl-assert (equal
              (sphinx-doc-parse-fields
               '(":param str sender: email address of the sender"
                 "                   this is the second line of sender param"
                 "                   this is the third line of sender param"
                 ":param int priority: priority"
                 ":returns: "
                 ":rtype: None"))
              '([cl-struct-sphinx-doc-field "param" "str" "sender" "email address of the sender\n                   this is the second line of sender param\n                   this is the third line of sender param"]
                [cl-struct-sphinx-doc-field "param" "int" "priority" "priority"]
                [cl-struct-sphinx-doc-field "returns" nil nil ""]
                [cl-struct-sphinx-doc-field "rtype" nil nil "None"]))))


(ert-deftest sphinx-doc-test-merge-fields ()
  (let ((fs1 '([cl-struct-sphinx-doc-field "param" "str" "name" "This is name"]
               [cl-struct-sphinx-doc-field "returns" nil nil "constant 42"]
               [cl-struct-sphinx-doc-field "rtype" nil nil "integer"]))
        (fs2 '([cl-struct-sphinx-doc-field "param" nil "name" ""]
               [cl-struct-sphinx-doc-field "param" nil "age" ""]
               [cl-struct-sphinx-doc-field "returns" nil nil ""]
               [cl-struct-sphinx-doc-field "rtype" nil nil ""])))
    (cl-assert (equal (sphinx-doc-merge-fields fs1 fs2)
                      (list (make-sphinx-doc-field :key "param" :arg "name" :type "str" :desc "This is name")
                            (make-sphinx-doc-field :key "param" :arg "age" :desc "")
                            (make-sphinx-doc-field :key "returns" :desc "constant 42")
                            (make-sphinx-doc-field :key "rtype" :desc "integer"))))))
