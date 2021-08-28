
(defun slice ()
 (print "Please enter the the desired bit-size:")
  (let ((size (read))) (divisible? (bin-read size))
    (print "Please enter one of the options listed above: ")
    (let ((slices (read)))
      (pixel-sort size slices))))


(defun pixel-sort (size slices)
  (write-bin (flatten
              (sort (group (coerce (bin-read size) 'list) slices)
                            #'> :key #'sumlist))
             size))

(defun random-sort (size slices)
  (write-bin (flatten
              (nshuffle (group (coerce (bin-read size) 'list) slices)))
             size))

(defun write-bin (seq size)
  (with-open-file 
      (s "sorted.data"
         :direction :output 
         :element-type `(unsigned-byte ,size) 
         :if-exists :supersede) 
    (write-sequence seq s))) 

(defun bin-read (size)
  (with-open-file (s "img.data"
                     :direction :input 
                     :element-type `(unsigned-byte ,size)) 
    (let ((seq (make-array (file-length s))))
      (read-sequence seq s)
      seq)))

(defun sumlist (lst) (reduce #'+ lst))

(defun flatten (structure)
  (cond ((null structure) nil)
        ((atom structure) (list structure))
        (t (mapcan #'flatten structure))))

(defun group (seq size)
  (loop for i below (length seq)
        by size
        collect (subseq seq i (+ size i))))

(defun divisible? (seq)
  (let ((lngth (length seq)))
    (format t "~A is divisible by: ~%" lngth)
    (dotimes (n lngth)    
      (if (< (mod lngth (1+ n)) 1)
          (format t "~A, " (1+ n))
            ;(format t "8756 is NOT divisible by ~A!~%" n)
          ))))

;(divisible? (bin-read '1024))

(defun nshuffle (sequence)
  (loop for i from (length sequence) downto 2
        do (rotatef (elt sequence (random i))
                    (elt sequence (1- i))))
  sequence)



(defun move-block (seq)
    (let*
     ((width 6)
      (height 3)
      (block-width 2)
      (block-height 3)
      (offset 4)
      (source 0)
      (destination (+ offset source))
      (start 0)
      (end (length seq)))
      (loop for i from start to (- end width) by width 
          do (dotimes (j block-width)
               (swap-pixels (+ i j) (+ offset i j) seq)))))

(defun swap-pixels (from to lst)
  (rotatef (nth from lst) (nth to lst)))

(defun gen-list (length)
  (loop for i below length collect i))



#|
;;---------
;;Sort a list of lists by the sum of each sublist
(sort '((68 26 95) (16 89 33) (17 51 55)) #'<
      :key #'(lambda (lst)
                   (reduce #'+ lst)))

;;TODO: Cons the remainder to the list to use arbitrary list groupings
;; automate the generation of examples
;; automate conversion with imagemagick
;; shuffle the list 


(loop for i below 9 by 3 do (format nil "~s " (subseq a i (+ 3 i))))

(defun pixel-sort (size)
  (write-bin (sort (bin-read size) #'>) size))

(defun pixel-sort (size)
  (write-bin
   (sort (group (coerce (bin-read size) 'list) 796)
              #'> :key #'sumlist) size))


(defun flatten (lst)
  (let ((out '()))
    (cond
      ((null lst) out)
      ((listp lst) (cons (car lst) out))
      ((atom lst)) (cons lst out))
    (flatten (cdr out))))

|# 
