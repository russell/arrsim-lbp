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

(define-condition debian-package-error (error)
  ((text :initarg :text :reader text)))

(defun get-changelog-distributions (&optional (number 4))
  (run/lines `(pipe
               ("dpkg-parsechangelog" ,(string-concat "-c" (write-to-string number)))
               ("sed" "-n" "s/^ .* (.*) \\(.*\\); .*$/\\1/p"))
             :show *show-command-output*))

(defun get-release ()
  "Get the current target distribution from the changelog.  If the
current version is UNRELEASED then next released version will be
returned."
  (let ((branch (git-current-branch))
        (changelog-dist (remove "UNRELEASED" (get-changelog-distributions) :test #'equal)))
    (cond
      ((search "/" branch)
       (subseq branch (1+ (search "/" branch)) (length branch)))
      ((car changelog-dist)
       (car changelog-dist))
      (t "unstable"))))

(defun get-upstream ()
  "Return the upstream of the current branch.  This will look for a
branch with the name upstream/$distibution."
  (let ((dist (get-release)))
    (flet ((upstream-p (item)
             (eq (search "upstream" item) 0))
           (current-dist-p (item)
             (search dist item)))
      (let* ((deb-branch (debian-branch-p))
             (branches (git-list-branches))
             (upstream-branches (remove-if-not #'upstream-p branches))
             (current-dist (remove-if-not #'current-dist-p upstream-branches)))
        (cond
          ;; if there is a specialised branch for the current dist
          ;; then return it.
          (current-dist current-dist)
          ;; if the current branch name is debian and there is a branch
          ;; titled upstream then pass it back.
          ((and (equal deb-branch "debian")
                (member "upstream" branches :test #'equal))
           (member "upstream" branches :test #'equal))
          (t upstream-branches))))))

(defun directory-exists-p (directory)
  "test to see if the directory exists"
  (handler-case
    (let ((directory (truename directory)))
      (when
          (equal (namestring directory)
                 (directory-namestring directory))
        directory))
    (t nil)))

;; XXX This should be renamed or abstracted out to a predicate that
;; doesn't raise a condition.
(defun debian-branch-p ()
  "is the current checked out branch a Debian package.  Return the
branch name if true."
  (if (directory-exists-p
         (merge-pathnames
          (make-pathname :directory '(:relative "debian"))
          (git-root)))
    (git-current-branch)
    (error 'debian-package-error
           :text "The current git repository isn't a Debian package.")))

(defun get-debian (&optional dist)
  "Return the Debian branch based on a distribution.  This will look
for a branch with the name debian/$distribution or return master."
  (flet ((debian-p (item)
           (or (eq (search "debian" item) 0)
               (equal "master" item)))
         (current-dist-p (item)
           (search dist item)))
  (let* ((branches (remove-if-not #'debian-p (git-list-branches)))
         (current-dist (remove-if-not #'current-dist-p branches)))
    (cond
      (current-dist current-dist)
      (t branches)))))

(defun parse-changelog ()
  ;; TODO(RS) this should be run in the root dir of the package.
  (loop :for line :in (run/lines "dpkg-parsechangelog")
     :until (search "Changes:" line)
     :collect (cons (intern (string-upcase (subseq line 0 (search ": " line))) :keyword)
                    (subseq line (+ (search ": " line) 2)))))

(defun get-current-version ()
  (let ((changelog (parse-changelog)))
    (cdr (assoc :version changelog))))

(defun get-package-source-name ()
  (let ((changelog (parse-changelog)))
    (cdr (assoc :source changelog))))

(defun get-package-source-name ()
  (let ((changelog (parse-changelog)))
    (cdr (assoc :source changelog))))

(defun compare-versions (ver1 ver2 &key (operator "="))
  "Compare VER1 with VER2 using dpkg.  Possible operators are < << <=
= >= >> >."
  (when (= (sb-ext:process-exit-code
            (sb-ext:run-program "dpkg"
                                (list "--compare-versions"
                                      ver1 operator ver2)
                                :wait t :search t :input t :output t))
           0)
      t))

(defun increment-revision ()
  "Get the current version from the changelog and increment the
release number."
  (let ((version (cl-ppcre:register-groups-bind (version revision)
                     ("^(.*[^\\d])(\\d*)$" (get-current-version))
                   (concatenate 'string version (write-to-string (1+ (parse-integer revision)))))))
    (git-new-version version)))
