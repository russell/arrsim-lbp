;;; -*- Mode: Lisp; Syntax: COMMON-LISP; Base: 10 -*-

;; arrsim-lbp is a collection of packaging utilities
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

(in-package :cl)

(defpackage :arrsim-lbp

  (:nicknames :lbp)

  (:use :cl :inferior-shell)

  (:import-from #:cl-openstack
                #:server-create
                #:server-list)

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
