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

(defcommand merge-branch
    "Merge a branch with the current Debian version and generate a new
changelog entry."
  (with-cli-options ()
                    (help auto verbose)
    (when help
      (print-usage-summary "Usage:~%~@{~A~%~}"
                           '(((#\a "auto") nil "After merging, commit and tag.")
                             ((#\h "help") nil "Print this message.")))
      (exit 1))
    (when verbose
      (setq *show-command-output* t))
    (let ((branch (cdr free)))
      (let ((debian-version (git-merge-branch branch)))
        (git-repository-dirty)
        (if auto
            (git-commit-version)
            (print-error "Please update debian/changelog.~%"))))))
