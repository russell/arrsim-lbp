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

(defparameter *architecture* "amd64")
(defparameter *distribution* "precise")


(defun buildpackage ()
  (let* ((dist (car (remove "UNRELEASED" (get-changelog-distributions) :test #'equal)))
         (upstream-branch (car (get-upstream)))
         (debian-branch (debian-branch-p)))
    (let ((environ (cons
                    (format-string "ARCH=~A" *architecture*)
                    (cons
                     (format-string "DIST=~A" dist)
                     (sb-ext:posix-environ)))))
      (sb-ext:run-program "git-buildpackage"
                          (list "-sa"
                                "--git-ignore-branch"
                                (format-string "--git-upstream-branch=~S" upstream-branch)
                                (format-string "--git-debian-branch=~S" debian-branch))
                          :environment environ
                          :wait t :search t :input t :output t))))

(defun git-new-version (version)
  "create a new change log version and set the target distribution"
  (let* ((dist (car (remove "UNRELEASED" (get-changelog-distributions) :test #'equal)))
         (debian-branch (debian-branch-p)))
    (run (list 'git 'dch
               "--debian-branch" debian-branch
               "-N" version) :show *show-command-output*)
    (run (list 'dch :distribution  dist "") :show *show-command-output*)))

(defun git-commit-version ()
  "Create a new release commit and tag the current commit."
  (let* ((debian-version (get-current-version)))
    (git-commit-all (with-output-to-string (string) (format string "Released version ~A." debian-version)))
    (run (list 'git 'tag (string-concat "debian/" debian-version)) :show *show-command-output*)))

(defun git-merge-revision (ref upstream-version debian-version)
  (let* ((upstream-branch (car (get-upstream)))
         (debian-branch (debian-branch-p)))
    (run (list 'git 'checkout upstream-branch) :show *show-command-output*)
    (run (list 'git 'merge ref) :show *show-command-output*)
    (run (list 'git 'tag (string-concat "upstream/" upstream-version)) :show *show-command-output*)
    (run (list 'git 'checkout debian-branch) :show *show-command-output*)
    (run (list 'git 'merge :no-edit upstream-branch) :show *show-command-output*)
    (git-new-version debian-version)))

(defun git-merge-tag (tag)
  "merge a new version into the upstream and tag it"
  (if (equal (car (run/lines (concat '(git tag "-l") (list tag)) :show *show-command-output*)) tag)
      (progn
        (git-merge-revision tag tag (string-concat tag "-1"))
        (run (list 'dch "-r" "Released new version."))
        (princ "Please update debian/changelog.~%"))))


(defun git-merge-branch (branch)
  "merge the branch into the current package"
  (let* ((latest-version (git-create-version-number :branch branch))
         (current-version (get-current-version))
         (debian-version (string-concat latest-version "-1")))
    (if (compare-versions latest-version current-version :operator ">")
        (progn
          (print-error "Merging version ~A.~%" branch)
          (git-merge-revision branch latest-version debian-version)
          (run (list 'dch "-r" "Released new version."))
          debian-version)
        (print-error "Version ~A is older then current deb version ~A.~%" latest-version current-version))))
