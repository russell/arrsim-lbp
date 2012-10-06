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

(defcommand dch
    "Generate changelog or new version."
  (with-cli-options ()
    (help increment auto verbose)
    (when help
      (print-usage-summary "Usage:~%~@{~A~%~}"
                           '(((#\i "increment") nil "Increment the revision.")
                             ((#\a "auto") nil "After creating new changelog entry, commit and tag.")
                             ((#\v "verbose") nil "Verbose output.")
                             ((#\h "help") nil "Print this message.")))
      (exit 1))
    (when verbose
      (setq *show-command-output* t))
    (increment-revision)
    (if auto
        (git-commit-version)
        (printf "Please update debian/changelog.~%"))))
