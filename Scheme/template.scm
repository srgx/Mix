
(include "print.scm")

(define s "section")

(define data-section
  (string-append s " .data\n\nEXIT_SUCCESS equ 0\nSYS_exit equ 60\n\n"))

(define text-section
  (string-append s " .text\nglobal _start\n_start:\n\n"))

(define end-section
  "last:\n  mov rax, SYS_exit\n  mov rdi, EXIT_SUCCESS\n  syscall")

(define commands '())

(define (logmov dest value)
  (set! commands
    (cons (string-append "mov " dest ", " (number->string value))
          commands)))

(print (string-append data-section text-section end-section))

#;(with-output-to-file "out.txt"
    (lambda () (write-line "Hello")))

