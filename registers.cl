
(load "convbin.cl")
(load "repeat.cl")
(load "unfold.cl")

(defvar register-size 64)
(defvar num-registers 4)

(defvar register-names
  '(("rax" "eax" "ax" "al")
    ("rbx" "ebx" "bx" "bl")
    ("rcx" "ecx" "cx" "cl")
    ("rdx" "edx" "dx" "dl")))

(defvar register-sizes
  (unfold (lambda (x) (< x 8)) (lambda (x) x)
          (lambda (x) (floor x 2)) register-size))

(defun fold-right (fn base lst1 &optional lst2)
  (if (null lst1) base
      (if (null lst2)
          (apply fn (list (car lst1) (fold-right fn base (cdr lst1))))
          (apply fn (list (car lst1) (car lst2) (fold-right fn base (cdr lst1) (cdr lst2)))))))

(defvar v-registers
  ((lambda ()
    (labels ((h (v-register-names parent-index)
      (if (null v-register-names) '()
          (append
            (fold-right
              (lambda (name size rnames)
                (cons (list name size parent-index) rnames))
              '() (car v-register-names) register-sizes)
            (h (cdr v-register-names) (+ parent-index 1))))))
    (h register-names 0)))))

(defun get-v-register (v-register-name)
  (find-if (lambda (v-register)
          (equal (car v-register) v-register-name))
        v-registers))

(defvar registers
  (make-array num-registers
    :initial-element (list (zeros register-size)
                           (zeros register-size))))

(defun get-v-register-contents (v-register-name)
  (let ((r (get-v-register v-register-name)))
    (subseq (car (svref registers (caddr r)))
            (- register-size (second r)))))

(defun add-zeros (target-size bin-value)
  (append (zeros (- target-size (length bin-value))) bin-value))

(defun mov (v-register-name decimal-value)
  (let ((vrg (get-v-register v-register-name)))
    (setf (svref registers (caddr vrg))
      (mapcar (lambda (l) (add-zeros register-size l))
           `(,(to-bin decimal-value) ,(ones (second vrg)))))))

(write
  (let* ((e-ones (ones 8)) (twenty (to-bin 20))
         (t-size (length twenty)) (five '(1 0 1))
         (tfn (lambda (sub-size base) (lambda (x) (append (zeros (- x sub-size)) base)))))
    (mov "eax" 255) (mov "rcx" 20)
    (and (equal register-sizes (fold-right (lambda (f r) (cons (length (get-v-register-contents f)) r))
                                  '() (car register-names)))
         (equal `(,(to-bin 5) ,(to-num five)) `(,five 5))
         (equal (mapcar #'get-v-register-contents
                      `(,@(car register-names) ,(caadr register-names) ,@(third register-names)))
                 `(,@(mapcar (funcall tfn 8 e-ones) register-sizes) ,(zeros register-size) ,@(mapcar (funcall tfn t-size twenty) register-sizes))))))
