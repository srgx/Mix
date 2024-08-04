(include "print.scm")

(define (example lst a1 a2)
  (if (null? lst) '()
      (cons (/ (* (car lst) a1) a2)
            (example (cdr lst) (- a1 1) (+ a2 1)))))

(define (fold-with-args-list comb-fun updater-fun base lst args)
  (if (null? lst) base
      (comb-fun
        (car lst)
        (fold-with-args-list comb-fun updater-fun base (cdr lst) (updater-fun args))
        args)))

(define (fold-with-args-var comb-fun updater base lst . args)
  (if (null? lst) base
      (comb-fun
        (car lst)
        (apply fold-with-args-var comb-fun updater base (cdr lst) (updater args))
        args)))

(print (example '(5 6 7 8) 10 2))

(print (fold-with-args-list
         (lambda (fst rst args)
           (cons (/ (* fst (car args)) (cadr args)) rst))
         (lambda (args) (list (- (car args) 1) (+ (cadr args) 1)))
         '()
         '(5 6 7 8)
         '(10 2)))

(print (fold-with-args-var
         (lambda (fst rst args)
           (cons (/ (* fst (car args)) (cadr args)) rst))
         (lambda (args) (list (- (car args) 1) (+ (cadr args) 1)))
         '()
         '(5 6 7 8)
         10 2))

