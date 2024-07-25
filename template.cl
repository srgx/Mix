
(defvar s "section")

(defvar data-section
  (concatenate 'string s
    " .data~%~%EXIT_SUCCESS equ 0~%SYS_exit equ 60~%~%"))

(defvar text-section
  (concatenate 'string s
    " .text~%global _start~%_start:~%~%"))

(defvar end-section
  "last:~%  mov rax, SYS_exit~%  mov rdi, EXIT_SUCCESS~%  syscall~%")

(defvar commands '())

(defun logmov (dest value)
  (setq commands
    (cons (concatenate 'string "mov " dest ", " (write-to-string value))
          commands)))

(format t (concatenate 'string data-section text-section end-section))

