
(import (srfi 1) (srfi 95))
(include "try-args.scm")
(include "repeat.scm")
(include "assoc.scm")
(include "print.scm")

(define (range f t)
  (if (= f t) '() (cons f (range (+ f 1) t))))

(define (bf-pdp l v)
  (define (h cmbs m l)
    (let ((cand `(0 ,@(car cmbs) ,m)))
      (if (equal? l (sort (distances-in-list cand) <))
          cand (h (cdr cmbs) m l))))
  (let ((m (apply max l)))
    (h (combinations
         (cond ((= 1 v) (range 1 m))
               ((= 2 v) (delete-duplicates l)))
         (- (/ (+ 1 (sqrt (- 1 (* -8 (length l))))) 2) 2))
        m l)))

(define (bf-pdp-v1 l)
  (bf-pdp l 1))

(define (bf-pdp-v2 l)
  (bf-pdp l 2))

(define (remove-first el lst)
  (cond
    ((null? lst) '())
    ((= (car lst) el) (cdr lst))
    (else (cons (car lst) (remove-first el (cdr lst))))))

(define (subset els lst)
  (or (null? els)
    (and (member (car els) lst)
         (subset (cdr els) lst))))

(define (max-in-list l)
  (apply max l))

(define (partial-digest l)
  (let ((width (max-in-list l)))
    (sort (place (remove-first width l)
                 (list 0 width) width) <)))

(define (place l x width)
  (if (null? l) x
      (let ((y (max-in-list l)))
        (try-args
          (lambda (f)
            (let ((dsts (distances f x)))
              (and (subset dsts l)
                   (place (removea dsts l) (cons f x) width))))
          y (- width y)))))

(define (removea tr l)
  (if (null? tr) l
      (removea (cdr tr)
               (remove-first (car tr) l))))

(define (distances-in-list l)
  (let ((rest (cdr l)))
    (if (null? rest) '()
        (append (distances (car l) rest)
                (distances-in-list rest)))))

(define (distances f r)
  (map (lambda (e) (abs (- f e))) r))

(define (combinations lst size)
  (cond
    ((null? lst) '())
    ((= size 0) '(()))
    (else
      (let ((rest (cdr lst)))
        (append (map (lambda (y) (cons (car lst) y))
                     (combinations rest (- size 1)))
                (combinations rest size))))))

(define (next-leaf-r a k)
  (if (null? a) '()
      (let ((f (car a)) (r (cdr a)))
        (apply cons
          (if (< f k)
              `(,(+ f 1) ,r)
              `(0 ,(next-leaf-r r k)))))))

(define (next-leaf-l l k)
  (define (nexl l k)
    (define (h cl k nxt)
      (apply cons
        (if (< cl k)
            `(,(+ cl 1) ,nxt)
            `(n ,(cons 0 nxt)))))
    (let ((cl (car l)) (rl (cdr l)))
      (if (null? rl)
          (h cl k '())
          (let ((nxt (nexl rl k)))
            (if (eq? (car nxt) 'n)
                (h cl k (cdr nxt))
                (cons cl nxt))))))
  (let ((r (nexl l k)))
    (if (eq? 'n (car r)) (cdr r) r)))

(define (all-leaves t l k)
  (define (all-leaves-h nleaf nxt l k e)
    (let ((n (nleaf l k)))
      (if (equal? n e) '() (cons n (nxt n k e)))))
  (define (all-leaves-h-l l k e)
   (all-leaves-h next-leaf-l all-leaves-h-l l k e))
  (define (all-leaves-h-r l k e)
   (all-leaves-h next-leaf-r all-leaves-h-r l k e))
  (let ((s (zeros l)))
    (cons s ((cond ((eq? 'l t) all-leaves-h-l)
                   ((eq? 'r t) all-leaves-h-r)) s k s))))

(define (all-leaves-l l k)
  (all-leaves 'l l k))

(define (all-leaves-r l k)
  (all-leaves 'r l k))

(define (make-alist size)
  (define (h s)
    (if (> s 3) '()
        (cons (list s 0) (h (+ s 1)))))
  (h 0))

(define (index-of value alist)
  (define (h index lst v)
    (if (= (caar lst) v) index
        (h (+ index 1) (cdr lst) v)))
  (h 0 alist value))

(define (occurrences col)
  (define (h alist lst)
    (if (null? lst) alist
        (h (update-assoc-fixed (car lst) alist)
           (cdr lst))))
  (h (make-alist 4) col))

(define (max-val lst)
  (define (h lsst cmax)
    (if (null? lsst) cmax
        (h (cdr lsst)
           (if (> (cadar lsst) (cadr cmax))
               (car lsst)
               cmax))))
  (h (cdr lst) (car lst)))

(define (p-score col)
  (cadr (max-val (occurrences col))))

(define (score s dna pattern-length)
  (define (h trimmed)
    (if (null? (car trimmed)) 0
        (+ (p-score (map car trimmed))
           (h (map cdr trimmed)))))
  (h (trim s dna pattern-length)))

(define (trim s dna pattern-length)
  (if (null? s) '()
      (cons (take (drop (car dna) (car s)) pattern-length)
            (trim (cdr s) (cdr dna) pattern-length))))

(define (bf-motif-search dna pattern-length)
  (let ((max-shift (- (length (car dna)) pattern-length)))
    (define (h best-score-and-motif current-leaf dna pattern-length first-leaf)
      (if (equal? current-leaf first-leaf)
          best-score-and-motif
          (let ((scr (score current-leaf dna pattern-length)))
            (h (if (> scr (car best-score-and-motif))
                   (list scr current-leaf)
                   best-score-and-motif)
               (next-leaf-l current-leaf max-shift)
               dna pattern-length first-leaf))))
    (let ((first-leaf (zeros (length dna))))
      (h (list (score first-leaf dna pattern-length) first-leaf)
         (next-leaf-l first-leaf max-shift)
         dna pattern-length first-leaf))))

(print
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
         (num-dna (length dna)))
    (and (equal? r1 (partial-digest l))
         (equal? r2 (bf-pdp-v1 l))
         (equal? r2 (bf-pdp-v2 l))
         (equal? z (next-leaf-r n c))
         (equal? z (next-leaf-l n c))
         (equal? '(0 0 0 1) (next-leaf-l z c))
         (equal? '(1 0 0 0) (next-leaf-r z c))
         (equal? l-lvs (all-leaves-l 2 2))
         (equal? (all-leaves-r 2 2)
                 (map (lambda (n) (reverse n)) l-lvs))
         (equal? (trim lo-s dna 4) '((0 0 1 1) (3 3 2 2)
                                     (2 2 0 0) (1 1 3 3)))
         (equal? (trim hi-s dna 4) '((3 3 2 2) (3 3 2 2)
                                     (3 3 2 2) (3 3 2 2)))
         (equal? (score lo-s dna 4) dna-len)
         (equal? (score hi-s dna 4) (* dna-len num-dna))
         (equal? (bf-motif-search dna 4) `(16 ,hi-s)))))


