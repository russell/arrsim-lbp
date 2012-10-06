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


(in-package :arrsim-lbp)

(defparameter *show-command-output* nil
  "Print the commands being run to *TRACE-OUTPUT* before execution.")

(defun main ()
  (let* ((primary-command (cadr sb-ext:*posix-argv*))
         (command (find primary-command *commands*
                        :key (lambda (i) (string-downcase (symbol-name (car i))))
                        :test #'equal)))
    (if command
        (progn (funcall (caddr command))
               (print command))
        (progn
          (print-error "ERROR: Invalid Command, ~A.~%" primary-command)
          (print-usage)))))
