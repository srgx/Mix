
(defun unfold (p f g seed)
  (labels ((h (value)
    (if (funcall p value) '()
        (cons (funcall f value)
              (h (funcall g value))))))
    (h seed)))

(defun unfold-right (p f g seed)
  (labels ((h (value acc)
    (if (funcall p value) acc
        (h (funcall g value)
           (cons (funcall f value) acc)))))
    (h seed '())))
