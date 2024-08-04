(include "print.scm")

(define (to-x l f v)
  (integer->char (f (char->integer l) v)))

(define (to-lower l)
  (to-x l logior #x20))

(define (to-upper l)
  (to-x l logand #xdf))

(define (string-to-int s)
  (chars-to-int (string->list s) 0))

(define (chars-to-int l acc)
  (if (null? l) acc
      (chars-to-int (cdr l)
        (+ (* acc 10)
           (logand (char->integer (car l)) #xf)))))

(define (to-date m d y)
  (logior (ash m 12) (ash d 7) (- y 1900)))

(define (extract-month d)
  (ash d -12))

(define (extract-year d)
  (+ 1900 (logand d #x7f)))

(define (extract-day d)
  (logand (ash d -7) #x1f))

(define (cnt-bits n)
  (define (h n m)
    (if (zero? m) 0
        (+ (if (= m (logand n m)) 1 0)
           (h n (ash m -1)))))
  (h n #x8000))

(define (test-bit n s)
  (and (= s (logand s #xf))
       (let ((m (ash 1 s)))
         (= m (logand m n)))))

(define (test-bits n)
  (let h ((n n) (c 15))
    (if (< c 0) '()
        (cons (if (test-bit n c) 1 0)
              (h n (- c 1))))))

(define (rol n)
  (logior
    (logand (ash n 1) #xff)
    (if (= #x80 (logand #x80 n)) 1 0)))

(define (ror n)
  (logior
    (ash n -1)
    (if (= 1 (logand 1 n)) #x80 0)))

(print
  (and (equal? '(0 1 2 10 16) (map cnt-bits '(0 1 5 #xcdcd #xffff)))
       (equal? (list (to-lower #\a) (to-lower #\A)
                     (to-upper #\a) (to-upper #\A))
               '(#\a #\a #\A #\A))
       (equal? (list (string-to-int "123") (string-to-int "512"))
               '(123 512))
       (let* ((m 2) (d 14) (y 1980) (date (to-date m d y)))
         (equal? (list (extract-month date)
                       (extract-day date)
                       (extract-year date))
                 (list m d y)))
       (equal? (test-bits #xabcd) '(1 0 1 0 1 0 1 1 1 1 0 0 1 1 0 1))
       (equal? (list (rol 0) (rol 2) (rol 128) (rol 148) (rol 255)
                     (ror 0) (ror 4) (ror 1) (ror 41) (ror 255))
                '(0 4 1 41 255 0 2 128 148 255))))

