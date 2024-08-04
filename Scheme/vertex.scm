;(import (rnrs))

(define (next-vertex vec level max-index max-letter)
  (if (< level max-index)
      (let ((next-level (+ level 1)))
        (begin
          (vector-set! vec next-level 1)
          `(,vec ,next-level)))
      (let h ((vec vec) (max-letter max-letter) (c max-index))
        (cond
          ((= c -1) `(,vec 0))
          ((< (vector-ref vec c) max-letter)
           (vector-set! vec c (+ (vector-ref vec c) 1)) `(,vec ,c))
           (else (h vec max-letter (- c 1)))))))

(define (all-vertices vec)
  (let h ((vec vec) (level -1) (c 10))
    (if (zero? c) '()
        (let ((result (next-vertex vec level 3 2)))
          (display result) (newline)
          (h (car result) (cadr result) (- c 1))))))

(define ve (vector 0 0 0 0))

(all-vertices ve)
