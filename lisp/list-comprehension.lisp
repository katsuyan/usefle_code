;;;reference from "http://d.hatena.ne.jp/Nobuhisa/20090524/1243175312"

(defmacro list-comprehension (exp-left exp-right)
  (let* ((exp-vec (coerce exp-right 'vector))
         (len (length exp-vec)) )
    (labels ((elt2 (i)
                   (svref exp-vec i))
             (get-var/seq ()
                          (do ((i 0 (1+ i)) acc)
                            ((>= i len) (nreverse acc))
                            (if (eq '<- (elt2 i))
                              (prog1
                               (push (cons (elt2 (1- i)) (elt2 (1+ i))) acc)
                               (incf i 2) ))))
             (get-pred ()
                       (let ((pos (position '<- exp-vec :from-end t)))
                         (if pos
                           (nthcdr (+ 2 pos) exp-right) ))))
            `(loop
               ,@(mapcan #'(lambda (p) `(for ,(car p) in ,(cdr p))) (get-var/seq))
               ,@(mapcan #'(lambda (pred) `(if ,pred)) (get-pred))
               collect ,@exp-left))))

(progn
 (set-macro-character #\] (get-macro-character #\) ))
 (set-macro-character
  #\[
  #'(lambda (stream c)
            (let ((left (read-delimited-list #\| stream t))
                  (right (read-delimited-list #\] stream t)) )
              `(list-comprehension ,left ,right) ))))
