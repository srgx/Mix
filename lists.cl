
(defvar lists (list (make-list 4 :initial-element 0)
                    (make-list 4 :initial-element 0)))

(defvar a1 (list "a1" (car lists)))
(defvar a2 (list "a2" (car lists)))

(setf (nth 0 (nth 0 lists)) 5)
(setf (nth 1 (nth 1 a2)) 6)

(write a1) (terpri)
(write a2) (terpri)

