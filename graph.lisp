
;; for later
(in-package #:pace16)


(defstruct (graph (:conc-name gr-)
                  (:print-function print-graph)
                  (:copier copy-gr))
  (n 0) (m 0) (adj nil))

(defun copy-graph (g)
 (make-graph :n (gr-n g)
             :m (gr-m g)
             :adj (map 'vector #'copy-list (gr-adj g))))

(defmacro get-neigbourhood (g i)
  `(aref (gr-adj ,g) ,i))

(defmacro add-to-neighbourhood (g x y)
  `(ncons (get-neigbourhood ,g ,x) ,y))

(defun print-seq (seq stream)
  (do ((i 0 (1+ i)))
    ((>= i (length seq)) (format stream "~%"))
    (format stream "~A " (elt seq i))))

(defun print-graph (g stream depth)
  (declare (ignore depth))
  (format stream "#g:~A ~A~%" (gr-n g) (gr-m g))
  (do ((i 0 (1+ i)))
    ((>= i (gr-n g)))
    (print-seq (get-neigbourhood g i) stream))
  )

;; (let ((g (read-graph "testgraph.gr"))
;;       (gprime (copy-graph g)))
;;   gprime)

(defun remove-alpha-char (str)
  (remove-if #'alpha-char-p str))

(defun split-by-one-space (str)
  (loop for i = 0 then (1+ j)
        as j = (position #\Space str :start i)
        collect (subseq str i j)
        while j))

(defun empty-str-p (str)
  (if (= 0 (length str)) t nil))

(defun string-to-numbers (str)
  (mapcar #'parse-integer
          (remove-if #'empty-str-p
                     (split-by-one-space (remove-alpha-char str)))))

(defun add-edge (g i j)
  (ncons (aref (gr-adj g) i) j)
  (ncons (aref (gr-adj g) j) i))

(defun read-graph (path)
  (let ((g))
    (with-open-file (str path :direction :input)
      (do ((line (read-line str nil 'eof) (read-line str nil 'eof)))
        ((eql line 'eof))
        (cond ((eql (char line 0) #\c))
              ((eql (char line 0) #\p)
               (let ((vals (string-to-numbers line)))
                 (setf g (make-graph :n (first vals)
                                     :m (second vals)
                                     :adj (make-array
                                            (list (first vals))
                                            :initial-element nil)))))
              (t (let ((vals (string-to-numbers line)))
                   (add-edge g (1- (first vals)) (1- (second vals))))))))
    g))


;;(read-graph "testgraph.gr")

(defstruct (online-graph (:conc-name og-)
                         (:print-function print-online-graph))
  (n 0) (m 0) (mat nil) (adj nil) (non-adj nil) (plc nil))

(defun print-online-graph (og stream depth)
  (declare (ignore depth))
  (format stream "#og: ~A ~A~%" (og-n og) (og-m og))
  (format stream "#adj:" )
  (princ (og-adj og) stream)
  (terpri)
  (format stream "#non-adj:" )
  (princ (og-non-adj og) stream)
  (terpri)
  (format stream "#plc:" )
  (princ (og-plc og)))

(defmacro add-to-dll-in-place (plc e)
  `(setf ,plc (dl-insert ,e ,plc)))

(defmacro add-to-adj (og i j)
  `(setf (aref (og-plc ,og) ,i ,j)
        (add-to-dll-in-place (aref (og-adj ,og) ,i) ,j)))

(defmacro add-to-non-adj (og i j)
  `(setf (aref (og-plc ,og) ,i ,j)
         (add-to-dll-in-place (aref (og-non-adj ,og) ,i) ,j)))

(defmacro adjacent (og i j)
  `(if (= 1 (aref (og-mat ,og) ,i ,j)) t nil))

(defmacro adj-as-lst (og i)
  `(dl->list (aref (og-adj ,og) ,i)))

(defmacro og-neighbourhood (og i)
  `(aref (og-adj ,og ) ,i))

(defmacro og-non-neighbourhood (og i)
  `(aref (og-non-adj ,og ) ,i))

(defmacro og-neighbourhood->list (og i)
  `(dl->list (og-neighbourhood ,og ,i)))

(defmacro og-non-neighbourhood->list (og i)
  `(dl->list (og-non-neighbourhood ,og ,i)))

(defmacro og-neighbourhood-size (og i)
  `(dl-length (og-neighbourhood ,og ,i)))

(defun online-graph-from-graph (g)
  (let* ((n (gr-n g))
         (og (make-online-graph :n n
                                :m (gr-m g)
                                :mat (make-array (list n n))
                                :adj (make-array
                                       (list n)
                                       :initial-element (dl-list))
                                :non-adj (make-array
                                           (list n)
                                           :initial-element (dl-list))
                                :plc (make-array
                                       (list n n)
                                       :initial-element nil)))
         (mat (og-mat og)))
    (do ((i 0 (1+ i)))
      ((>= i n))
      (mapcar #'(lambda (j)
                  (setf (aref mat i j) 1)) (get-neigbourhood g i)))
    (for i 0 (1- n)
      (for j 0 (1- n)
        (if (not (= i j))
         (if (= (aref mat i j) 1)
           (add-to-adj og i j)
           (add-to-non-adj og i j)))))
    og))




(defun opp (x)
  (if (= x 1) 0 1))

(defun toggle (og i j)
  (if (adjacent og i j)
    (progn
      (if (dl-prev (aref (og-plc og) i j))
        (dl-remove (aref (og-plc og) i j))
        (setf (aref (og-adj og) i) (dl-remove (aref (og-plc og) i j))))
      (setf (aref (og-plc og) i j)
            (setf (aref (og-non-adj og) i)
                  (dl-insert j (aref (og-non-adj og) i)))))
    (progn
      (if (dl-prev (aref (og-plc og) i j))
        (dl-remove (aref (og-plc og) i j))
        (setf (aref (og-non-adj og) i) (dl-remove (aref (og-plc og) i j))))
      (setf (aref (og-plc og) i j)
            (setf (aref (og-adj og) i)
                  (dl-insert j (aref (og-adj og) i))))))
  (setf (aref (og-mat og) i j) (opp (aref (og-mat og) i j))))

;; (defun toggle-test ()
;;   (let* ((g (read-graph "instances/ex003.gr"))
;;          (gprime (copy-graph g))
;;          (og (online-graph-from-graph gprime))
;;          (x 0)
;;          (xneigh (og-neighbourhood->list og x))
;;          (y (car xneigh)))
;;     (mvprint (og-neighbourhood og y) (dl-length (og-neighbourhood og y)))
;;     (mvprint (og-neighbourhood og x) (dl-length (og-neighbourhood og x)))
;;     (toggle og y x)
;;     (mvprint (og-neighbourhood og y) (dl-length (og-neighbourhood og y)))
;;     (mvprint (og-neighbourhood og x) (dl-length (og-neighbourhood og x)))))
;;
;; (toggle-test)

(defparameter *cnt* 0)

;;                (assert (< cnt (length lst))
;;                        (*cnt* cnt (length lst))
;;                        "ogs at itr ~S cnt:~S (length lst):~S"
;;                        *cnt* cnt (length lst))



(defun online-graph-subgraph (og lst)
  (assert (<= (length lst) (og-n og)))
  (incf *cnt*)
  (let ((ht (make-hash-table :size (length lst)))
        (cnt -1))
;;     (assert (= 0 (hash-table-size ht)))
    (labels ((getid (x)
               (assert (member x lst))
               (aif (gethash x ht)
                 it
                 (setf (gethash x ht) (incf cnt)))))
      (let* ((n (length lst))
             (adj (map 'vector
                       #'(lambda (x)
                           (let ((nei (og-neighbourhood->list og x)))
                             (assert (null (set-difference nei lst)))
                             (mapcar #'getid nei)))
                       lst))
             (m (/ (reduce #'(lambda (acc l) (+ acc (length l)))
                           adj :initial-value 0) 2)))
        (for x 0 cnt
          (mapcar #'(lambda (y) (assert (< y (length lst))
                                        ((aref adj x))
                                        "Failure ~S"
                                        (aref adj x))) (aref adj x)))
        (make-graph :n n
          :m m
          :adj adj)))))

(defun test-something (lst)
  (let ((ht (make-hash-table))
        (cnt -1))
    (labels ((getid (x)
                    (aif (gethash x ht)
                      it
                      (setf it (incf cnt)))))
      (mapcar #'getid lst))))

(let ((a (test-something '(1 2 3)))
      (b (test-something '(4 5 6))))
  (mvprint a b))



(defun online-graph-subgraph-assert (g)
  (let ((n (gr-n g))
        (lst nil))
    (dolist (x (range (1- n)))
      (dolist (y (get-neigbourhood g x))
        (assert (< y n))
        (unless (member y lst)
          (push y lst))))
    (for i 0 (1- n)
      (assert (member i lst)))
    (mvprint lst)
    (assert (= n (length lst)))))

;;TODO add semi-simpicial part

(defun simplicial (og lst)
  (let ((sim nil)
        (semi nil))
   (dolist (x lst)
     (let* ((prs (pairs (og-neighbourhood->list og x)))
            (cnt (reduce #'(lambda (z y)
                             (if (not (adjacent og (car y) (cdr y))) (1+ z) z))
                         prs
                         :initial-value 0)))
       (cond ((= cnt 0) (push x sim))
;;              ((< cnt 2) (push x semi))
             )))
   (values sim semi)))


;; (let* ((g (read-graph "testgraph.gr"))
;;        (og (online-graph-from-graph g)))
;;   (toggle og 0 1)
;;   (toggle og 0 1)
;;   (print g)
;;   (online-graph-subgraph og '(0 2 4))
;;   )

;;(defparameter g (read-graph "testgraph.gr"))
;;(defparameter og (online-graph-from-graph g))

(defun greedy-min-degree (g)
  (let ((result most-negative-fixnum))
    (do* ((cur (range (1- (gr-n g))) (remove (car mivals) cur))
          (mivals (reduce
                    #'(lambda (x y)
                        (let ((l (length (get-neigbourhood g y))))
                          (if (< (cdr x) l) x (cons y l)))) cur
                    :initial-value (cons -1 most-positive-fixnum))
                  (reduce
                    #'(lambda (x y)
                        (let ((l (length (get-neigbourhood g y))))
                          (if (< (cdr x) l) x (cons y l)))) cur
                    :initial-value (cons -1 most-positive-fixnum))))
      ((< (length cur) result ) (1+ result))
      (let ((mi (car mivals))
            (mil (cdr mivals)))
       (if (> mil result)
         (setf result mil))
       (mapcar #'(lambda (x)
                   (setf (aref (gr-adj g) x)
                         (remove mi (aref (gr-adj g) x))))
               (get-neigbourhood g mi))
       (dolist (p (pairs (get-neigbourhood g mi)))
         (let ((x (car p))
               (y (cdr p)))
           (if (not (member x (get-neigbourhood g y)))
             (progn
               (add-to-neighbourhood g x y)
               (add-to-neighbourhood g y x)))))
       )
      )
    )
  )

;; (let* ((g (read-graph "instances/ex001.gr"))
;;       (gprime (copy-graph g)))
;;   (greedy-min-degree gprime))

;; TODO test over all possible pairs the contraction that creates the
;; largest new neighbourhood

(defun smallest-neigh (g lst)
  (let* ((mins (car (multiple-value-list
                      (mostn #'(lambda (x) (length (get-neigbourhood g x)))
                             lst #'<))))
         (u -1)
         (v -1)
         (mival most-positive-fixnum))
    (dolist (x mins)
     (multiple-value-setq (y val)
       (most #'(lambda (y)
                 (assert (< y (gr-n g)))
                 (length
                   (intersection (get-neigbourhood g x)
                                 (get-neigbourhood g y))))
             (get-neigbourhood g x) #'<))
     (if (eql y nil)
       (setf val -1))
     (when (< val mival)
       (setq u y v x mival val)))
    (values u v mival)))

(defun minor-min-width (g)
  (let ((n (gr-n g)))
;;     (print n)
;;     (print (car (array-dimensions (gr-adj g))))
    (cond ((<= n 1) n)
          (t
           (let ((lb 0)
                 (cur (range (1- n))))
             (while (not (single cur))
               (multiple-value-setq (u v mival) (smallest-neigh g cur))
               (when (not (eql u nil))
                 (let* ((inter (intersection (get-neigbourhood g u)
                                             (get-neigbourhood g v)))
                        (noninter (remove u (set-difference (get-neigbourhood g v) inter)))
                        (un (remove v (remove u (union
                                                  (get-neigbourhood g u)
                                                  (get-neigbourhood g v))))))
                   (setf lb (max lb (length (get-neigbourhood g v))))
                   (dolist (x inter)
                     (setf (get-neigbourhood g x)
                           (remove v (get-neigbourhood g x))))
                   (dolist (x noninter)
                     (setf (get-neigbourhood g x)
                           (substitute u v (get-neigbourhood g x))))
                   (setf (get-neigbourhood g u) un)))
               (setf cur (remove v cur)))
             lb)))))

(defun rm_sim_semi (og lst)
  (mvdo* ((cur -1)
          (sim/semi nil (append sim/semi (append sim semi)))
          (nlst lst (set-difference nlst sim/semi))
          ((sim semi) (simplicial og nlst) (simplicial og nlst)))
         ((and (null sim) (null semi)) (values sim/semi cur))
         (assert (= 0 (length (set-difference sim nlst))))
         (dolist (x sim)
           (let* ((xneigh (og-neighbourhood->list og x))
                  (xlen (length xneigh)))
             (dolist (y xneigh)
               (toggle og y x))
             (when (> xlen cur)
               (setf cur xlen))))
         (dolist (x semi)
           (dolist (y (og-neighbourhood->list og x))
             (toggle og y x)))))

(defun rm_sim_semi-test (rpn)
  (let* ((g (read-graph rpn))
         (gprime (copy-graph g))
         (og (online-graph-from-graph gprime))
         (lst (range (1- (gr-n g)))))
     (multiple-value-bind (sim/semi cur) (rm_sim_semi og lst)
       (mvprint (length sim/semi) cur))))

(rm_sim_semi-test "instances/ex005.gr")

(defun online-graph-assert (og lst)
  (let* ((n (gr-n og))
         (opplst (set-difference (range (1- n)) lst)))
    (dolist (x lst)
      (dolist (y (og-neighbourhood->list og x))
        (assert (member y lst)))
      (dolist (y (og-non-neighbourhood->list og x))
        (assert (member y opplst))))))

(defun bb-assert (og lst)
  (dolist (x lst)
    (dolist (y (og-neighbourhood->list og x))
      (assert (member y lst)))))

(defun bb (og lst cur ub)
  (print (length lst))
  (if (single lst) 1
   (mvlet* (((sim/semi newcur) (rm_sim_semi og lst))
            (ncur (max newcur cur))
            (nub ub))
     (if (> ncur nub) (return-from bb nub))
     (let ((nlst (set-difference lst sim/semi)))
      (dolist (x nlst)
        (let ((xneighs (dl->list (og-neighbourhood og x)))
              (ps (pairs (adj-as-lst og x)))
              (len (og-neighbourhood-size og x))
              (remove_afterwards nil)
              (reduced (remove x nlst)))
          (dolist (neigh xneighs)
            (toggle og neigh x))
          (dolist (p ps)
            (let ((y (car p))
                  (z (cdr p)))
              (unless (adjacent og y z)
                (push p remove_afterwards)
                (toggle og y z)
                (toggle og z y))
              )
            )
          (bb-assert og reduced)
          (assert (> (length reduced) 0))
;;           (online-graph-assert og nlst)
          (let* ((subg (online-graph-subgraph og reduced))
;;                  (toto (online-graph-subgraph-assert subg))
                 (lb (minor-min-width subg)))
            (when (< lb nub)
              (let ((res (bb og reduced (max len ncur) ub)))
                (when (< res nub)
                  (print res)
                  (setf nub res)))))
          (dolist (neigh xneighs)
            (toggle og neigh x))
          (dolist (p remove_afterwards)
            (let ((y (car p))
                  (z (cdr p)))
              (toggle og y z)
              (toggle og z y))
            )
          )
        )
      )
     (dolist (x (reverse sim/semi))
       (dolist (y (og-neighbourhood->list og x))
         (toggle og y x)))
     nub)))


(defun tw (g)
  (let ((ub (greedy-min-degree g))
        (og (online-graph-from-graph g))
        (lst (range (1- (gr-n g)))))
     (bb og lst 0 ub)))

;; (declaim (optimize (debug 3)))
(defparameter g (read-graph "instances/ex001.gr"))
(defparameter gprime (copy-graph g))
(defparameter ub (greedy-min-degree gprime))
;; (defparameter gprime (copy-graph g))
(defparameter og (online-graph-from-graph gprime))
(defparameter lst (range (1- (gr-n g))))
;; (bb og lst ub)
;; (minor-min-width g)

(defun main ()
  (let ((g (read-graph "instances/ex001.gr"))
        (gprime (copy-graph g))
        (ub (greedy-min-degree gprime))
        (og (online-graph-from-graph g))
        (lst (range (1- (gr-n g)))))
    (bb og lst 0 ub)))

(main)

