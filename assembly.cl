
(load "convbin.cl")

(defvar mask1 '(0 0 1 0 0 0 0 0))
(defvar mask2 '(1 1 0 1 1 1 1 1))

(defun to-lower (char)
  (to-lu char #'logior (to-num mask1)))

(defun to-upper (char)
  (to-lu char #'logand (to-num mask2)))

(defun to-lu (char log-fun mask)
  (code-char (funcall log-fun (char-code char) mask)))

(defun str-to-num (str)
  (let ((chrs (coerce str 'list))
        (acc 0))
    (loop for ch in chrs do
      (setq acc
        (+ (* acc 10)
           (logand (char-code ch) #xf))))
     acc))

(defun to-date (m d y)
  (logior (ash m 12) (ash d 7) (- y 1900)))

(defun extract-month (d) (ash d -12))

(defun extract-day (d)
  (logand (ash d -7) (to-num '(0 0 0 1 1 1 1 1))))

(defun extract-year (d)
  (+ 1900 (logand d (to-num '(0 1 1 1 1 1 1 1)))))

(defun test-bits (n)
  (let ((index 15))
    (loop
      until (< index 0)
      collect
        (if (and (= index (logand index #xf))
                 (let ((mask (ash 1 index)))
                   (= mask (logand mask n))))
             1 0)
      do (incf index -1))))

(defun count-bits (num)
  (let ((mask (to-num '(1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0))) (count 0))
    (loop until (zerop mask) do 
      (incf count (if (= mask (logand num mask)) 1 0))
      (setq mask (ash mask -1))) count))

(defun rol (n)
  (logior
    (logand (ash n 1) #xff)
    (if (= #x80 (logand #x80 n)) 1 0)))

(defun ror (n)
  (logior
    (ash n -1)
    (if (= 1 (logand 1 n)) #x80 0)))

(write (and (equal (list (str-to-num "123") (str-to-num "512")) '(123 512))
            (equal (list (to-lower #\g) (to-lower #\G)
                         (to-upper #\g) (to-upper #\G))
                   '(#\g #\g #\G #\G))
            (let* ((m 2) (d 14) (y 1980) (date (to-date m d y)))
              (equal (list (extract-month date)
                           (extract-day date)
                           (extract-year date))
                     (list m d y)))
            (equal '(0 1 2 10 16) (mapcar #'count-bits '(0 1 5 #xcdcd #xffff)))
            (equal (test-bits #xabcd) '(1 0 1 0 1 0 1 1 1 1 0 0 1 1 0 1))
            (equal (list (rol 0) (rol 2) (rol 128) (rol 148) (rol 255)
                         (ror 0) (ror 4) (ror 1) (ror 41) (ror 255))
                   '(0 4 1 41 255 0 2 128 148 255))))


