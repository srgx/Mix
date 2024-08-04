
(require :asdf)

(defvar file-name "example.fasta")

(defvar file-lines (uiop:read-file-lines file-name))

(defun lines (data)
  (labels ((h (d acc)
    (if (or (null d)
            (eq #\> (elt (car d) 0)))
        (list (list (car data) acc) d)
        (h (cdr d) (concatenate 'string acc (car d))))))
  (h (cdr data) "")))

(defun proc-lines (data)
  (if (null data) ()
    (let ((ans (lines data)))
      (cons (car ans) (proc-lines (cadr ans))))))

(defun read-strings (in)
  (let ((str (read-line in nil nil)))
    (cond
      ((null str) "")
      ((eq #\> (elt str 0)) str)
      (t (concatenate 'string str (read-strings in))))))

(defparameter data (with-open-file (in file-name)
  (labels ((read-lines (extra-label)
    (labels ((h (lbl)
      (let* ((contents (read-strings in))
             (pos (position #\> contents)))
        (if pos
            (cons (list lbl (subseq contents 0 pos))
                  (read-lines (subseq contents pos (length contents))))
            (cons (list lbl contents) (read-lines ""))))))
      (if (string= "" extra-label)
          (let ((label (read-line in nil nil)))
            (if (null label) ()
                (h label)))
          (h extra-label)))))
    (read-lines ""))))

(write (equal (proc-lines file-lines) data)) (terpri)
(write data) (terpri)

