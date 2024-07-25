
(defun update-assoc (value alist base)
  (if (null alist) base
      (let ((rest (cdr alist)))
        (apply #'cons
          (if (= value (caar alist))
              `((,value ,(+ (cadar alist) 1)) ,rest)
              `(,(car alist) ,(update-assoc value rest base)))))))

(defun update-assoc-fixed (value alist)
  (update-assoc value alist '()))

(defun update-assoc-flex (value alist)
  (update-assoc value alist `((,value 1))))
