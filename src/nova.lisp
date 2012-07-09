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

(defparameter *os-flavor* "0")
(defparameter *os-image* "d5a4b467-de5e-4042-8ad0-86478c205e01")

;; http://superuser.com/questions/161973/how-can-i-forward-a-gpg-key-via-ssh-agent

(defun create-packaging-host (package &optional (image *os-image*))
  (let ((name (string-concat "ps-" package))
        (user-data (merge-pathnames
                    (make-pathname :name "cloud-config" :type "yml")
                    (component-pathname (component-system (find-system :arrsim-lbp))))))
    (create-server name image *os-flavor* :user-data user-data)))
