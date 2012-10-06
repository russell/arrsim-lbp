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

(defcommand merge-tag
    "Merge a tag with the current Debian version and generate a new
changelog entry."
  (with-cli-options ()
    (help recent)
    (when help
      (print-usage-summary "Usage:~%~@{~A~%~}"
                           '(((#\r "recent") nil "Use the most recent tag from a branch.")
                             ((#\h "help") nil "Print this message.")))
      (exit 1))
    (let ((tag (if recent
                   (git-recent-tag "origin/master")
                   (cadr free)))
          (current-version (get-current-version)))
      (git-repository-dirty)
      (if (and tag
               (compare-versions tag current-version :operator ">"))
          (progn
            (print-error "Merging version ~A.~%" tag)
            (git-merge-tag tag))
          (print-error "Version ~A is older then current deb version ~A.~%" tag current-version)))))
