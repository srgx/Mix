
(defun rec-args (lst a1 a2)
  (if (null lst) ()
      (cons (/ (* (car lst) a1) a2)
            (rec-args (cdr lst) (- a1 1) (+ a2 1)))))

(defmacro foldargs (ftp)
  `(if (null lst) base
      (funcall comber
        (car lst)
        ,(append (if (eq 'lst ftp)
                    '(fold-with-args-list)
                    '(apply #'fold-with-args-var))
                 '(comber updater base (cdr lst) (funcall updater args)))
        args)))

(defun fold-with-args-var
  (comber updater base lst &rest args)
    (foldargs 'var))

(defun fold-with-args-list
  (comber updater base lst args)
    (foldargs 'lst))

(defun cmbn (fst rst args)
  (cons (/ (* fst (car args)) (cadr args)) rst))

(defun updt (args)
  (list (- (car args) 1) (+ (cadr args) 1)))

(let ((lst '(5 6 7 8))
      (rslt '(25 18 14 56/5)))
  (write (and (equal rslt (rec-args lst 10 2))
              (equal rslt (fold-with-args-var #'cmbn #'updt () lst 10 2))
              (equal rslt (fold-with-args-list #'cmbn #'updt () lst '(10 2))))))

