;;; -*- Mode: Lisp; Syntax: COMMON-LISP; Base: 10 -*-

;; arrsim-lbp is a tool to automate package building.
;; Copyright (C) 2012 Russell Sim <russell.sim@gmail.com>
;;
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.


(defsystem :arrsim-lbp
  :depends-on (:asdf :inferior-shell :cl-openstack :cl-ppcre :unix-options :trivial-backtrace)
  :defsystem-depends-on (:asdf)
  :description "Debian package management scripts"
  :components ((:static-file "arrsim-lbp.asd")
               (:module "src"
                :components
                ((:file "package")
                 (:file "util" :depends-on ("package"))
                 (:file "deb" :depends-on ("package"))
                 (:file "build" :depends-on ("package" "util" "deb"))
                 (:file "git" :depends-on ("package"))
                 (:file "nova" :depends-on ("package"))
                 (:file "publish" :depends-on ("package" "deb"))
                 (:file "command" :depends-on ("package"))
                 (:module "commands"
                  :depends-on ("package" "util" "command")
                  :components ((:file "push")
                               (:file "merge-tag")
                               (:file "build")
                               (:file "login")
                               (:file "update")
                               (:file "info")
                               (:file "merge-branch")
                               (:file "publish")
                               (:file "dch")))
                 (:file "lbp" :depends-on ("package" "util"
                                                     "command"
                                                     "commands"))))))
