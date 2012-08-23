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

(defun command-merge-branch ()
  (with-cli-options ()
    (help auto)
    (when help
      (print-usage-summary "Usage:~%~@{~A~%~}"
                           '(((#\a "auto") nil "After merging, commit and tag.")
                             ((#\h "help") nil "Print this message.")))
      (sb-ext:quit :unix-status 1))
    (let ((branch (cdr free)))
      (let ((debian-version (git-merge-branch branch)))
        (if auto
            (git-commit-version)
            (print-error "Please update debian/changelog.~%"))))))
