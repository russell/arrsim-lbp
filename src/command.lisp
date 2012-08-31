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

(defvar *commands* nil
  "A list of all the possible sub-commands.")

(defvar *default-usage-format-string* "~%~%Commands:~%~@{~A~%~}~%")

(defun exe-name ()
  "The command used to execute this program"
  #+:sbcl (first sb-ext:*posix-argv*)
  #+:ccl (first ccl:*command-line-argument-list*)
  #+:clisp (first ext:*args*)
  #+:lispworks (first system:*line-arguments-list*)
  #+:cmu (first extensions:*command-line-words*)
  #+:ecl (first (ext:command-args))
  )

(defmacro defcommand (command documentation &body body)
  `(let ((com (list (quote ,command)
                    ,documentation
                    (lambda () ,@body))))
     (if (member (quote ,command) *commands* :test (lambda (a b) (equal a (car b))))
         (setf *commands*
               (substitute-if
                com
                (lambda (l) (equal (quote ,command) (car l)))
                *commands*))
         (push com *commands*))))

(defun greater (x y &optional (test #'>))
  (if (funcall test x y)
      x
      y))

(defun greatest (list &optional (test #'>))
  (reduce (lambda (x y)
	    (greater x y test))
	  list))

(defun print-command-summary (description)
  (let* ((max-spec-length (greatest (mapcar
                                     (lambda (c) (length (symbol-name (car c))))
                                     *commands*))))
    (format t "~?" description (mapcar (lambda (spec)
              (command-spec-to-string spec max-spec-length))
            *commands*))))

(defun split-string (string)
    (loop :for i = 0 :then (1+ j)
          :as positions = (remove-if (lambda (l) (eq nil l))
                                     (list (position #\Space string :start i)
                                           (position #\Newline string :start i)))
          :as j = (when positions (apply 'min positions))
          :collect (subseq string i j)
          :while j))

(defun command-spec-to-string (command &optional (desc-offset 50))
  (format nil (format nil "  ~A~~~A,2T~<~@{~A~^ ~:_~}~:>"
		      "~A"
		      (+ desc-offset 4)
		      (split-string (cadr command)))
           (string-downcase (symbol-name (car command)))))

(defun print-usage ()
  (print-command-summary
   (concatenate 'string "Usage: "
                (exe-name)
                " <command> [OPTIONS]..."
                *default-usage-format-string*)))
