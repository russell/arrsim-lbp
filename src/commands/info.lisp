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

(defcommand info
    "Print the variables that will be used for the current execution."
  (with-cli-options ()
    (help verbose)
    (when help
      (print-usage-summary "Usage:~%~@{~A~%~}"
                           '(((#\v "verbose") nil "Verbose output.")
                             ((#\h "help") nil "Print this message.")))
      (exit 1))
    (when verbose
      (setq *show-command-output* t))
    (printf "Debian Branch: ~A~%" (debian-branch-p))
    (printf "Upstream Branch: ~A~%" (car (get-upstream)))
    (printf "Current Version ~A~%" (get-current-version))))
