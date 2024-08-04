(import xlib (chicken bitwise) (srfi 1))

(include "repeat.scm")
(include "unfold.scm")

(define register-size 64)
(define num-registers 4)
(define registers-x 20)
(define registers-y 20)
(define rect-width 15)
(define rect-height 15)
(define win-x 10)
(define win-y 10)
(define win-width 640)
(define win-height 480)
(define win-border 1)
(define h-space 16)
(define v-space 16)
(define red #xf7100c)
(define green #x33de1d)
(define black 0)

(define quu #f)
(define mode 'data)

(define instructions '())

(define (to-dec lst)
  (define (h lst value acc)
    (if (null? lst) acc
        (h (cdr lst)
           (quotient value 2)
           (+ acc (* (car lst) value)))))
  (h lst (expt 2 (- (length lst) 1)) 0))

(define (to-bin n)
  (define (h n acc)
    (if (zero? n) acc
        (h (quotient n 2)
           (cons (remainder n 2) acc))))
  (h n '()))

(define registers
  (make-vector num-registers
    ((lambda () `(,(zeros register-size)
                  ,(zeros register-size))))))

(define register-names
  '(("rax" "eax" "ax" "al")
    ("rbx" "ebx" "bx" "bl")
    ("rcx" "ecx" "cx" "cl")
    ("rdx" "edx" "dx" "dl")))

(define register-sizes
  (unfold (lambda (x) (< x 8)) (lambda (x) x)
          (lambda (x) (quotient x 2)) register-size))

(define v-registers
  ((lambda ()
    (define (h v-register-names parent-index)
      (if (null? v-register-names) '()
          (append
            (fold-right
              (lambda (name size rnames)
                (cons (list name size parent-index) rnames))
            '() (car v-register-names) register-sizes)
            (h (cdr v-register-names) (+ parent-index 1)))))
    (h register-names 0))))

(define (get-v-register v-register-name)
  (find (lambda (v-register)
          (equal? (car v-register) v-register-name))
        v-registers))

(define (get-v-register-contents v-register-name)
  (let ((r (get-v-register v-register-name)))
    (drop (car (vector-ref registers (third r)))
          (- register-size (second r)))))

(define (slice lst from size)
  (take (drop lst from) size))

(define (add-zeros target-size bin-value)
  (append (zeros (- target-size (length bin-value))) bin-value))

(define (mov v-register-name decimal-value)
  (let ((vrg (get-v-register v-register-name)))
    (vector-set! registers (third vrg)
      (map (lambda (l) (add-zeros register-size l))
           `(,(to-bin decimal-value) ,(ones (second vrg)))))))

;(define (log-instruction symbol a1 a2)
;  (set! instructions (cons instructions

(print
  (let* ((n 20) (e-ones (ones 8)) (twenty (to-bin 20))
         (t-size (length twenty)) (five '(1 0 1))
         (tfn (lambda (sub-size base) (lambda (x) (append (zeros (- x sub-size)) base)))))
    (mov "eax" 255) (mov "rcx" 20)
    (and (equal? register-sizes (fold-right (lambda (f r) (cons (length (get-v-register-contents f)) r))
                                  '() (car register-names)))
         (equal? `(,(to-bin 5) ,(to-dec five)) `(,five 5))
         (equal? (map get-v-register-contents
                      `(,@(car register-names) ,(caadr register-names) ,@(third register-names)))
                 `(,@(map (tfn 8 e-ones) register-sizes) ,(zeros register-size) ,@(map (tfn t-size twenty) register-sizes))))))

(define (toggle-mode)
  (set! mode (if (eq? 'data mode) 'updated 'data)))


(define (manage-keys keycode)
  (cond
    ((= 9 keycode) (set! quu #t))
    ((= 40 keycode) (toggle-mode))
    (else (display keycode))))

(define (draw-register window gc register y)
  (define (h register x)
    (if (null? register) '()
        (begin
          (x:gc-set! gc x:GC-Foreground (if (zero? (car register)) black red))
          (x:fill-rectangle window gc `(,x ,y) `(,rect-width ,rect-height))
          (h (cdr register) (+ x h-space)))))
  (h register registers-x))

(define (draw-r access)
  (define (h window gc r-idx y)
    (or (= r-idx num-registers)
        (begin
          (draw-register window gc (access (vector-ref registers r-idx)) y)
          (h window gc (+ r-idx 1) (+ y v-space))))) h)

(define (draw-data window gc r-idx y)
  ((draw-r car) window gc r-idx y))

(define (draw-updated window gc r-idx y)
  ((draw-r cadr) window gc r-idx y))

(define (draw-registers window gc registers)
  ((if (eq? mode 'data) draw-data draw-updated) window gc 0 registers-y))

;;; Shows a window and terminates after a button-press
;;; (or after the second exposure event)

(let* ((disp (xopendisplay #f))
       (screen (xdefaultscreen disp))
       (root (xrootwindow disp screen))
       (window (xcreatesimplewindow
                 disp root 100 200 300 50 0
                 (xblackpixel disp screen)
                 (xwhitepixel disp screen)))
      (font (xloadfont disp "10x20"))
      (gc (xcreategc disp window 0 #f))
      (event (make-xevent)))
  (xsetforeground disp gc (xblackpixel disp screen))
  (xsetbackground disp gc (xwhitepixel disp screen))
  (xsetfunction disp gc GXCOPY)
  (xsetfont disp gc font)
  (xselectinput disp window (bitwise-ior EXPOSUREMASK BUTTONPRESSMASK))
  (xmapwindow disp window)
  (xnextevent disp event)
  (xdrawstring disp window gc 100 30 "Hello World!" 12)
  (xflush disp)
  (xnextevent disp event))

(exit)

(let* ((disp (x:open-display #f))
       (screen (x:default-screen disp))
       (window
         (x:create-window (x:root-window disp screen)
                          `(,win-x ,win-y)
                          `(,win-width ,win-height)
                          win-border
                          (x:screen-black disp screen)
                          (x:screen-white disp screen)))
        (gc (x:default-gc window)))
    (x:window-set! window x:CW-Event-Mask
      (logior x:Exposure-Mask x:Key-Press-Mask))
    (x:gc-set! gc x:GC-Function x:G-Xcopy)
    (x:map-window window)
    (let loop ()
      (let ((ev (x:next-event disp)))
        (if (= x:Key-Press (x:event-ref ev X-event:type))
            (manage-keys (x:event-ref ev X-event:keycode))
            (display ev)))
      (draw-registers window gc registers)
      (if quu quu (loop)))
    (x:close disp))

; w 25
; a 38
; d 40
; s 39
; q 24


