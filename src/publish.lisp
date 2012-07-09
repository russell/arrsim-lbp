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

(in-package :arrsim-lbp)

;; ../build-area/rcportal_2.1.5-1_amd64.changes
(defparameter *build-area* "/home/russell/projects/packaging/build-area/")

(defun publish (&optional changes)
  "merge a new version into the upstream and changes it"
  (let ((changes-file (if changes changes
                          (string-concat *build-area* (get-package-source-name) "_"
                                         (get-current-version) "_" *architecture* ".changes"))))
    (run (list 'dupload changes-file))
    (run '(import-new-debs.sh) :host "root@mirrors.melbourne.nectar.org.au")))
