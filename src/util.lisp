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


(defmacro printf (control-string &rest format-arguments)
  `(format *standard-output* ,control-string ,@format-arguments))

(defmacro print-error (control-string &rest format-arguments)
  `(format *error-output* ,control-string ,@format-arguments))

(defmacro format-string (control-string &rest format-arguments)
  `(with-output-to-string (stream)
     (format stream ,control-string ,@format-arguments)))

(defun string-concat (&rest strings)
  (apply 'concatenate 'string strings))

(defun concat (&rest lists)
  (apply 'concatenate 'list lists))

(defun exit (&optional (code 0))
  (sb-ext:quit :unix-status code))

(defmacro with-environment ((symbol &rest environment-variables) &body body)
  "Add a list of keyword value ENVIRONMENT-VARIABLES to the standard
environment and bind them to the SYMBOL."
  (let ((sym (gensym)))
    `(let* ((,sym (loop :for (key value) :on ,(cons 'list environment-variables) :by #'cddr
                             :collect (format-string "~A=~A" key value)))
            (,symbol (concatenate 'list ,sym (sb-ext:posix-environ))))
       ,@body)))
