;;; -*- Lisp -*-

(defsystem :arrsim-lbp
  :depends-on (:asdf :inferior-shell)
  :description "Debian package management scripts"
  :components
  ((:file "package")
   (:file "deb" :depends-on ("package"))
   (:file "build" :depends-on ("package" "deb"))
   (:file "git" :depends-on ("package"))
   (:file "publish" :depends-on ("package" "deb"))))
