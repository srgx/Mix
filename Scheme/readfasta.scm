
(import (srfi 13))

(define file-name "example.fasta")

(define file-lines (with-input-from-file file-name
  (lambda ()
    (let read-lines ()
      (let ((line (read-line)))
        (if (eof-object? line) ()
            (cons line (read-lines))))))))

(define (lines data)
  (define (h d acc)
    (if (or (null? d)
            (eq? #\> (string-ref (car d) 0)))
        (list (list (car data) acc) d)
        (h (cdr d) (string-append acc (car d)))))
  (h (cdr data) ""))

(define (proc-lines data)
  (if (null? data) '()
    (let ((ans (lines data)))
      (cons (car ans) (proc-lines (cadr ans))))))

(define (read-strings)
  (let ((str (read-line)))
    (cond
      ((eof-object? str) "")
      ((eq? #\> (string-ref str 0)) str)
      (else (string-append str (read-strings))))))

(define data (with-input-from-file file-name
  (lambda ()
    (let read-lines ((extra-label ""))
      (define (h lbl)
        (let* ((contents (read-strings))
               (pos (string-index contents #\>)))
          (if pos
              (cons (list lbl (substring contents 0 pos))
                    (read-lines (substring contents pos (string-length contents))))
              (cons (list lbl contents) (read-lines "")))))
      (if (string=? "" extra-label)
          (let ((label (read-line)))
            (if (eof-object? label) '()
                (h label)))
          (h extra-label))))))

(display (equal? (proc-lines file-lines) data)) (newline)
(display data) (newline)

#;((">Rosalind_0808" "CCACCCTCGTGGTATGGCTAGGCATTCAGGAACCGGAGAACGCTTCAGACCAGCCCGGACTGGGAACCTGCGGGCAGTAGGTGGAAT")
   (">Rosalind_5959" "CCATCGGTAGCGCATCCTTAGTCCAATTAAGTCCCTATCCAGGCGCTCCGCCGAAGGTCTATATCCATTTGTCAGCAGACACGC")
   (">Rosalind_6404" "CCTGCGGAAGATCGGCACTAGAATAGCCAGAACCGTTTCTCTGAGGCTTCCGGCCTTCCCTCCCACTAATAATTCTGAGG"))

