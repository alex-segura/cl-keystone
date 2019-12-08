(defsystem cl-keystone
  :description "Bindings to the keystone assembly engine"
  :version "0.0.1"
  :author "Alex Segura <alex@lispm.dev>"
  :license "BSD"
  :defsystem-depends-on (#:cffi-grovel)
  :depends-on (#:cffi #:alexandria)
  :components
  ((:file "package")
   (:cffi-grovel-file "grovel" :depends-on ("package"))
   (:file "keystone" :depends-on ("grovel"))))
