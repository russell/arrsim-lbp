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

(defun git-recent-tag (&optional (branch "origin/master"))
  "get the most recent tag on the specified branch"
  (cl-ppcre:register-groups-bind (tag number rev)
      ("^(.+)-(\\d+)-(.+)$"
       (car (run/lines `(git "describe" ,branch) :show t))
       :sharedp t)
    tag))

(defun git-latest-commit-date (&optional (branch "origin/master"))
  "get the latest commit date"
  (cl-ppcre:register-groups-bind (year month day hour minute second timezone)
      ("^CommitDate: (\\d*)-(\\d*)-(\\d*) (\\d*):(\\d*):(\\d*) (.*)$"
       (car (run/lines `(pipe (git "log" "-n" "1" "--pretty=fuller" "--date=iso" ,branch)
                              (grep "CommitDate:")) :show t))
       :sharedp t)
    (list :year year :month month :day day
          :hour hour  :minute minute :second second
          :timezone timezone)))

(defun git-create-version-number (&key (branch "origin/master") (local-version ""))
  "generate a version number based on the latest tag and the commit date"
  (let ((commit-date (git-latest-commit-date branch))
        (latest-tag (git-recent-tag branch)))
    (concatenate 'string latest-tag "+" (getf commit-date :year)
                 (getf commit-date :month) (getf commit-date :day)
                 local-version)))
