#|
  This file is a part of takajin84key project.
  Copyright (c) 2015 Satoshi Iwasaki (yanqirenshi@gmail.com)
|#

(in-package :cl-user)
(defpackage takajin84key-test-asd
  (:use :cl :asdf))
(in-package :takajin84key-test-asd)

(defsystem takajin84key-test
  :author "Satoshi Iwasaki"
  :license "LLGPL"
  :depends-on (:takajin84key
               :prove)
  :components ((:module "t"
                :components
                ((:test-file "takajin84key"))))

  :defsystem-depends-on (:prove-asdf)
  :perform (test-op :after (op c)
                    (funcall (intern #.(string :run-test-system) :prove-asdf) c)
                    (asdf:clear-system c)))
