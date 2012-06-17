(in-package :cl)

(defpackage :arrsim-lbp

  (:use :cl :inferior-shell)

  (:nicknames :lbp)

  (:export
   ;; Deb
   #:get-release
   #:get-upstream
   #:get-debian
   #:get-current-version

   ;; Dpkg
   #:compare-versions

   ;; Git
   #:git-recent-tag

   #:publish
   #:buildpackage
   #:git-merge-tag))
