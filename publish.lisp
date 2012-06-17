(in-package :arrsim-lbp)

;; ../build-area/rcportal_2.1.5-1_amd64.changes
(defparameter *build-area* "../build-area/")

(defun publish (&optional changes)
  "merge a new version into the upstream and changes it"
  (let ((changes-file (if changes changes
                          (string-concat *build-area* (get-package-source-name) "_"
                                         (get-current-version) "_" *architecture* ".changes"))))
    (run (list 'dupload changes-file))
    (run '(import-new-debs.sh) :host "root@mirrors.melbourne.nectar.org.au")))
