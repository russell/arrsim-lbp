(in-package :arrsim-lbp)

(defparameter *architecture* "amd64")
(defparameter *distribution* "precise")


(defun buildpackage ()
  (let* ((dist (get-release))
         (upstream-branch (car (get-upstream dist)))
         (debian-branch (car (get-debian dist))))
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
  (let* ((dist (get-release))
         (debian-branch (car (get-debian dist))))
    (run (list 'git 'dch
               "--debian-branch" debian-branch
               "-N" version) :show t)
    (run (list 'dch :distribution  *distribution* "") :show t)))

(defun git-commit-version ()
  "create a new release commit and tag the current commit."
  (let* ((debian-version (get-current-version)))
    (git-commit-all (with-output-to-string (string) (format string "Released version ~A." debian-version)))
    (run (list 'git 'tag (string-concat "debian/" debian-version)) :show t)))

(defun git-merge-revision (ref upstream-version debian-version)
  (let* ((dist (get-release))
         (upstream-branch (car (get-upstream dist)))
         (debian-branch (car (get-debian dist))))
    (run (list 'git 'checkout upstream-branch) :show t)
    (run (list 'git 'merge ref) :show t)
    (run (list 'git 'tag (string-concat "upstream/" upstream-version)) :show t)
    (run (list 'git 'checkout debian-branch) :show t)
    (run (list 'git 'merge upstream-branch) :show t)
    (git-new-version debian-version)))

(defun git-merge-tag (tag)
  "merge a new version into the upstream and tag it"
  (if (equal (car (run/lines (concat '(git tag "-l") (list tag)) :show t)) tag)
      (progn
        (git-merge-revision tag tag (string-concat tag "-1"))
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
          debian-version)
        (print-error "Version ~A is older then current deb version ~A.~%" latest-version current-version))))

(defun git-commit-all (message)
  (run `(git 'commit "-am" ,message)))
