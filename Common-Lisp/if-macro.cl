
(defmacro myif (&rest r)
  `(if (null ',r) nil
       (if ,(car r) ,(cadr r)
           (myif ,@(cddr r)))))

