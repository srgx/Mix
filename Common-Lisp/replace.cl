
(require :asdf)

(defun find-symbols (str symbols)
  (and (not (null symbols))
       (or (let* ((s (car symbols)) (idx (search s str)))
             (and idx (list s idx)))
           (find-symbols str (cdr symbols)))))

(defvar replacements '(("(" "{") (")" "}")))

(defun subst-replace (substrings-to-replace main-string)
  (let ((rslt (find-symbols main-string substrings-to-replace)))
    (if rslt (let ((idx (cadr rslt)) (smb (car rslt)))
               (subst-replace
                 substrings-to-replace
                 ((lambda (main-string newstring from-idx to-idx)
                    (concatenate 'string
                      (subseq main-string 0 from-idx) newstring (subseq main-string to-idx)))
                     main-string (cadr (assoc smb replacements :test #'string=))
                     idx (+ idx (length smb)))))
              main-string)))

(let ((str "((()))"))
  (write (string= "{{{}}}" (subst-replace '("(" ")") str))))

