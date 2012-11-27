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

(defun git-repository-dirty ()
  "raise a condition if the repository is dirty"
  (run/lines '(git "diff" "--exit-code")
             :show *show-command-output*
             :on-error "There are uncommitted files in the repository."
             ))

(defun git-root ()
  "return the root of the git repository."
  (truename
   (run/ss '(git "rev-parse" "--show-toplevel")
           :show *show-command-output*)))

(defun git-current-branch ()
  "return the current branch."
  (run/ss '(pipe
            ("git" "branch" "--no-color")
            ("sed" "-e" "/^[^*]/d" "-e" "s/* \\(.*\\)/\\1/"))))

(defun git-list-branches ()
  "return a list of the branches in the current repository."
  (run/lines '(pipe
               ("git" "branch" "--no-color")
               ("cut" "-c" "3-"))))

(defun git-recent-tag (&optional (branch "origin/master"))
  "get the most recent tag on the specified branch"
  (let ((latest-tag (car (run/lines `(git "describe" ,branch)
                                    :show *show-command-output*))))
    (or (cl-ppcre:register-groups-bind (tag number rev)
            ("^(.+)-(\\d+)-(.+)$"
             latest-tag
             :sharedp t)
          tag)
        latest-tag)))

(defun git-latest-commit-date (&optional (branch "origin/master"))
  "get the latest commit date"
  (cl-ppcre:register-groups-bind (year month day hour minute second timezone)
      ("^CommitDate: (\\d*)-(\\d*)-(\\d*) (\\d*):(\\d*):(\\d*) (.*)$"
       (car (run/lines `(pipe (git "log" "-n" "1" "--pretty=fuller" "--date=iso" ,branch)
                              (grep "CommitDate:")) :show *show-command-output*))
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

(defun git-commit-all (message)
  (run `(git 'commit "-am" ,message)))
