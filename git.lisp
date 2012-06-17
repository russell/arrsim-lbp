(in-package :arrsim-lbp)

(defun git-recent-tag (&optional branch)
  (car
   (if branch
       (run/lines (concat '(git "describe" "--abbrev=0" :tags) (list branch)))
       (run/lines '(git "describe" "--abbrev=0" :tags)))))
