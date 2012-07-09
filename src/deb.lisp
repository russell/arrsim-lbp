(in-package :arrsim-lbp)

(defmacro format-string (control-string &rest format-arguments)
  `(with-output-to-string (stream)
     (format stream ,control-string ,@format-arguments)))

(defmacro print-error (control-string &rest format-arguments)
  `(format *error-output* ,control-string ,@format-arguments))

(defun string-concat (&rest strings)
  (apply 'concatenate 'string strings))

(defun concat (&rest lists)
  (apply 'concatenate 'list lists))

(defun get-release ()
  (let ((branch (run/ss "git branch --no-color 2> /dev/null | sed -e '/^[^*]/d' -e 's/* \\(.*\\)/\\1/'"))
        (changelog-dist (remove "UNRELEASED" (run/lines "dpkg-parsechangelog -c2 | sed -n 's/^ .* (.*) \\(.*\\); .*$/\\1/p'" :show t) :test #'equal)))
    (cond
      ((search "/" branch)
       (subseq branch (1+ (search "/" branch)) (length branch)))
      ((car changelog-dist)
       (car changelog-dist))
      (t "unstable"))))

(defun get-upstream (&optional dist)
  (flet ((upstream-p (item)
           (eq (search "upstream" item) 0))
         (current-dist-p (item)
           (search dist item)))
  (let* ((branches (remove-if-not #'upstream-p (run/lines "git branch --no-color 2> /dev/null | cut -c 3-")))
         (current-dist (remove-if-not #'current-dist-p branches)))
    (cond
      (current-dist current-dist)
      (t branches)))))

(defun get-debian (&optional dist)
  (flet ((debian-p (item)
           (eq (search "debian" item) 0))
         (current-dist-p (item)
           (search dist item)))
  (let* ((branches (remove-if-not #'debian-p (run/lines "git branch --no-color 2> /dev/null | cut -c 3-")))
         (current-dist (remove-if-not #'current-dist-p branches)))
    (cond
      (current-dist current-dist)
      (t branches)))))

(defun parse-changelog ()
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

(defun compare-versions (ver1 ver2 &key (operator "="))
  "compare VER1 with VER2. possible operators are < << <= = >= >> >"
  (when (= (sb-ext:process-exit-code
            (sb-ext:run-program "dpkg"
                                (list "--compare-versions"
                                      ver1 operator ver2)
                                :wait t :search t :input t :output t))
           0)
      t))

(defun increment-revision ()
  (let ((version (cl-ppcre:register-groups-bind (version revision)
                     ("^(.*[^\\d])(\\d*)$" (get-current-version))
                   (concatenate 'string version (write-to-string (1+ (parse-integer revision)))))))
    (git-new-version version)))
