(load "assoc.cl")
(load "repeat.cl")

(defun distances (f r) (mapcar (lambda (e) (abs (- f e))) r))

(defun distances-in-list (l)
  (let ((rest (cdr l)))
    (if (null rest) ()
        (append (distances (car l) rest)
                (distances-in-list rest)))))

(defun combinations (lst size)
  (cond
    ((null lst) ())
    ((zerop size ) '(()))
    (t (let ((rest (cdr lst)))
         (append (mapcar (lambda (y) (cons (car lst) y))
                         (combinations rest (1- size)))
                 (combinations rest size))))))

(defun max-in-list (l) (apply #'max l))

(defun bf-pdp (l v)
  (labels ((h (cmbs m l)
    (let ((cand `(0 ,@(car cmbs) ,m)))
      (if (equal l (sort (distances-in-list cand) #'<))
          cand (h (cdr cmbs) m l)))))
  (let ((m (max-in-list l)))
    (h (combinations
         (cond ((= 1 v) ((lambda (start stop)
                           (loop for n from start to stop collect n)) 1 m))
               ((= 2 v) (remove-duplicates l)))
         (- (/ (1+ (sqrt (- 1 (* -8 (length l))))) 2) 2))
        m l))))

(defun bf-pdp-v1 (l) (bf-pdp l 1))
(defun bf-pdp-v2 (l) (bf-pdp l 2))

(defun remove-first (el lst)
  (cond
    ((null lst) ())
    ((= (car lst) el) (cdr lst))
    (t (cons (car lst) (remove-first el (cdr lst))))))

(defun removea (tr l)
  (if (null tr) l
      (removea (cdr tr)
               (remove-first (car tr) l))))

(defun place (l x width)
  (if (null l) x
      (let ((y (max-in-list l)))
        (some
          (lambda (f)
            (let ((dsts (distances f x)))
              (and (subsetp dsts l)
                   (place (removea dsts l) (cons f x) width))))
          (list y (- width y))))))

(defun partial-digest (l)
  (let ((width (max-in-list l)))
    (sort (place (remove-first width l)
                 (list 0 width) width) #'<)))

(defun next-leaf-r (a k)
  (if (null a) ()
      (let ((f (car a)) (r (cdr a)))
        (apply #'cons
          (if (< f k)
              `(,(+ f 1) ,r)
              `(0 ,(next-leaf-r r k)))))))

(defun next-leaf-l (l k)
  (labels ((nexl (l k)
    (labels ((h (cl k nxt)
      (apply #'cons
        (if (< cl k)
            `(,(+ cl 1) ,nxt)
            `(n ,(cons 0 nxt))))))
    (let ((cl (car l)) (rl (cdr l)))
      (if (null rl)
          (h cl k ())
          (let ((nxt (nexl rl k)))
            (if (eq (car nxt) 'n)
                (h cl k (cdr nxt))
                (cons cl nxt))))))))
  (let ((r (nexl l k)))
    (if (eq 'n (car r)) (cdr r) r))))

(defun all-leaves (dir l k)
  (labels ((all-leaves-h (nleaf nxt l k e)
             (let ((n (funcall nleaf l k)))
               (if (equal n e) () (cons n (funcall nxt n k e)))))
           (all-leaves-h-l (l k e)
             (all-leaves-h #'next-leaf-l #'all-leaves-h-l l k e))
           (all-leaves-h-r (l k e)
             (all-leaves-h #'next-leaf-r #'all-leaves-h-r l k e)))
  (let ((s (zeros l)))
    (cons s (funcall (cond ((eq 'l dir) #'all-leaves-h-l)
                           ((eq 'r dir) #'all-leaves-h-r)) s k s)))))

(defun all-leaves-l (l k)
  (all-leaves 'l l k))

(defun all-leaves-r (l k)
  (all-leaves 'r l k))

(defun make-alist (size)
  (loop for n from 0 to (1- size) collect `(,n 0)))

(defun index-of (value alist)
  (labels ((h (index lst)
    (if (= (caar lst) value) index
        (h (+ index 1) (cdr lst)))))
  (h 0 alist)))

(defun occurrences (col)
  (labels ((h (alist lst)
    (if (null lst) alist
        (h (update-assoc-fixed (car lst) alist)
           (cdr lst)))))
  (h (make-alist 4) col)))

(defun max-val (lst)
  (let ((cmax (car lst)))
    (dolist (x (cdr lst))
      (if (> (cadr x) (cadr cmax)) (setq cmax x)))
    cmax))

(defun p-score (col)
  (cadr (max-val (occurrences col))))

(defun trim (s dna pattern-length)
  (loop for ss in s
        for ddna in dna
        collect (subseq (subseq ddna ss) 0 pattern-length)))

(defun score (s dna pattern-length)
  (labels ((h (trimmed)
            (if (null (car trimmed)) 0
                (+ (p-score (mapcar #'car trimmed))
                   (h (mapcar #'cdr trimmed))))))
    (h (trim s dna pattern-length))))

(defun bf-motif-search (dna pattern-length)
  (let ((max-shift (- (length (car dna)) pattern-length)))
    (labels ((h (best-score-and-motif current-leaf dna pattern-length first-leaf)
      (if (equal current-leaf first-leaf)
          best-score-and-motif
          (let ((scr (score current-leaf dna pattern-length)))
            (h (if (> scr (car best-score-and-motif))
                   (list scr current-leaf)
                   best-score-and-motif)
               (next-leaf-l current-leaf max-shift)
               dna pattern-length first-leaf)))))
    (let ((first-leaf (zeros (length dna))))
      (h (list (score first-leaf dna pattern-length) first-leaf)
         (next-leaf-l first-leaf max-shift)
         dna pattern-length first-leaf)))))

(defun next-vertex (vec level max-index max-letter)
  (if (< level max-index)
      (let ((next-level (+ level 1)))
          (setf (svref vec next-level) 1)
          `(,vec ,next-level))
      (labels ((h (vec max-letter c)
        (cond
          ((= c -1) `(,vec 0))
          ((< (svref vec c) max-letter)
           (setf (svref vec c) (+ (svref vec c) 1)) `(,vec ,c))
          (t (h vec max-letter (- c 1))))))
        (h vec max-letter max-index))))

(defun all-vertices (vec)
  (labels ((h (vec level c)
    (if (zerop c) ()
        (let ((result (next-vertex vec level 3 2)))
          (cons (list (coerce (car result) 'list) (cadr result))
                (h (car result) (cadr result) (- c 1)))))))
  (h vec -1 10)))

(write
  (let* ((l '(2 2 3 3 4 5 6 7 8 10))
         (r1 '(0 3 6 8 10))
         (r2 '(0 2 4 7 10))
         (c 9)
         (n '(9 9 9 9))
         (z '(0 0 0 0))
         (l-lvs '((0 0) (0 1) (0 2) (1 0)
                  (1 1) (1 2) (2 0) (2 1) (2 2)))
         (lo-s '(0 4 2 4))
         (hi-s '(6 4 0 6))
         (dna '((0 0 1 1 2 2 3 3 2 2)
                (2 2 1 1 3 3 2 2 1 1)
                (3 3 2 2 0 0 1 1 1 1)
                (1 1 0 0 1 1 3 3 2 2)))
         (dna-len 4)
         (num-dna (length dna))
         (ve (vector 0 0 0 0)))
    (and (equal r1 (partial-digest l))
         (equal r2 (bf-pdp-v1 l))
         (equal r2 (bf-pdp-v2 l))
         (equal z (next-leaf-r n c))
         (equal z (next-leaf-l n c))
         (equal '(0 0 0 1) (next-leaf-l z c))
         (equal '(1 0 0 0) (next-leaf-r z c))
         (equal l-lvs (all-leaves-l 2 2))
         (equal (all-leaves-r 2 2)
                (mapcar (lambda (n) (reverse n)) l-lvs))
         (equal (trim lo-s dna 4) '((0 0 1 1) (3 3 2 2)
                                    (2 2 0 0) (1 1 3 3)))
         (equal (trim hi-s dna 4) '((3 3 2 2) (3 3 2 2)
                                    (3 3 2 2) (3 3 2 2)))
         (equal (score lo-s dna 4) dna-len)
         (equal (score hi-s dna 4) (* dna-len num-dna))
         (equal (bf-motif-search dna 4) `(16 ,hi-s))
         (equal (all-vertices ve)
               '(((1 0 0 0) 0) ((1 1 0 0) 1) ((1 1 1 0) 2)
                 ((1 1 1 1) 3) ((1 1 1 2) 3) ((1 1 2 2) 2)
                 ((1 1 2 1) 3) ((1 1 2 2) 3) ((1 2 2 2) 1)
                 ((1 2 1 2) 2))))))

