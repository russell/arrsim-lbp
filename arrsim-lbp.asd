;;; -*- Lisp -*-

(defsystem :arrsim-lbp
  :depends-on (:asdf :inferior-shell :cl-openstack)
  :description "Debian package management scripts"
  :components ((:static-file "arrsim-lbp.asd")
               (:module "src"
                :components
                ((:file "package")
                 (:file "deb" :depends-on ("package"))
                 (:file "build" :depends-on ("package" "deb"))
                 (:file "git" :depends-on ("package"))
                 (:file "nova" :depends-on ("package"))
                 (:file "publish" :depends-on ("package" "deb"))))))
