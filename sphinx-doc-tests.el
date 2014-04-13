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


(ert-deftest sphinx-doc-test-lines->paras ()
  (assert (sphinx-doc-lines->paras
           '("Send message to a recipient as per the priority"
             ""
             "This is before comment. "
             ""
             "This is also before the comment but a second para"
             ""
             ":param str sender: email address of the sender"
             "                   this is the second line of sender param"
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
             ":param str recipient: email address of the receiver"
             ":param str message_body: message to send"
             ":param int priority: priority"
             ":returns: nothing"
             ":rtype: None")
            ("This is after comment"))))


(ert-deftest sphinx-doc-test-parse-fields ()
  (assert (sphinx-doc-parse-fields
           '(":param str sender: email address of the sender"
             "                   this is the second line of sender param"
             ":param str recipient: email address of the receiver"
             ":param str message_body: message to send"
             ":param int priority: priority"
             ":returns: "
             ":rtype: None"))
          ([cl-struct-field "param" "str" "sender" "email address of the sender\n                   this is the second line of sender param"]
           [cl-struct-field "param" "str" "recipient" "email address of the receiver"]
           [cl-struct-field "param" "str" "message_body" "message to send"]
           [cl-struct-field "param" "int" "priority" "priority"]
           [cl-struct-field "returns" nil nil nil]
           [cl-struct-field "rtype" nil nil "None"])))
