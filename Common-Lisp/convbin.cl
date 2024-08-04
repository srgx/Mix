
(defun to-num (bits)
  (let ((bv (expt 2 (1- (length bits)))))
    (loop for bit in bits sum (* bit bv) do
      (setq bv (floor bv 2)))))

(defun to-bin (n)
  (labels ((h (n acc)
    (if (zerop n) acc
        (h (floor n 2)
           (cons (rem n 2) acc)))))
  (h n '())))
