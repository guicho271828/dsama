#|
  This file is a part of dsama project.
  Copyright (c) 2019 Masataro Asai (guicho2.71828@gmail.com)
|#


(in-package :cl-user)
(defpackage dsama.test-asd
  (:use :cl :asdf))
(in-package :dsama.test-asd)


(defsystem dsama.test
  :author "Masataro Asai"
  :mailto "guicho2.71828@gmail.com"
  :description "Test system of dsama"
  :license "LLGPL"
  :depends-on (:dsama
               :fiveam)
  :components ((:module "t"
                :components
                ((:file "package"))))
  :perform (test-op :after (op c) (eval (read-from-string "(5am:run! :dsama)"))
))
