
(load "assoc.cl")

(defvar data
  '(2 2 2 3 3 3 3 3 4 4
    4 5 5 5 5 5 5 6 6 7))

(defun occurrences (d)
  (sort
    (labels ((h (d acc)
              (if (null d) acc
                  (h (cdr d) (update-assoc-flex (car d) acc)))))
      (h d '()))
    (lambda (a b) (< (car a) (car b)))))

(defun frequencies (data)
  (let ((ocs (occurrences data))
        (ld (length data)))
    (labels ((h (lst)
            (if (null lst) '()
                (cons (list (caar lst) (* (/ (cadar lst) ld) 100)) (h (cdr lst))))))
      (h ocs))))

(defun cumulative-frequencies (frqs)
  (labels ((h (frqs acc)
    (if (null frqs) '()
        (let ((nv (+ (cadar frqs) acc)))
          (cons (list (caar frqs) nv)
                (h (cdr frqs) nv))))))
  (h frqs 0)))

(defun median (v)
  (let* ((le (length v))
         (h (floor le 2))
         (vh (svref v h)))
    (if (evenp le) (/ (+ vh (svref v (- h 1))) 2) vh)))

(defun percentile (k v)
  (labels ((percentile-index (k v)
     (let* ((va (* k (+ (length v) 1)))
            (qo (floor va 100))
            (sq (- qo 1))
            (re (rem va 100)))
       (if (zerop re) (list sq) (list sq qo)))))
  (percentile-index k v)))

(write
  (let ((d 1e-5)
        (frqs '((2 15) (3 25) (4 15) (5 30) (6 10) (7 5)))
        (ocs '((2 3) (3 5) (4 3) (5 6) (6 2) (7 1)))
        (cfrqs '((2 15) (3 40) (4 55) (5 85) (6 95) (7 100))))
    (and (equal (occurrences data) ocs)
         (equal (frequencies data) frqs)
         (equal (cumulative-frequencies frqs) cfrqs)
         (< (abs (- 7.5 (median (vector 5 6 7 8 9 10)))) d)
         (< (abs (- 8 (median (vector 4 7 8 10 15)))) d))))

(write
  (let ((veo (vector 2 4 8 16 20 30 50 120 130)))
    (list (percentile 50 veo) (percentile 99 veo))))


