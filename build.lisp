(in-package :arrsim-lbp)

(defparameter *architecture* "amd64")


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

(defun git-merge-tag (tag)
  "merge a new version into the upstream and tag it"
  (let* ((dist (get-release))
         (upstream-branch (car (get-upstream dist)))
         (debian-branch (car (get-debian dist))))
    (if (equal (car (run/lines (concat '(git tag "-l") (list tag)) :show t)) tag)
        (progn
          (run (list 'git 'checkout upstream-branch) :show t)
          (run (list 'git 'merge tag))
          (run (list 'git 'tag (string-concat "upstream/" tag)))
          (run (list 'git 'checkout debian-branch))
          (run (list 'git 'merge upstream-branch))
          (run (list 'git 'dch
                     "--debian-branch" debian-branch
                     "-N" (string-concat tag "-1")))
          (print "Please update debian/changelog")))))
