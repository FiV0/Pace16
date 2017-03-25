;; for later
(in-package #:pace16)

;;some macros are shamelessly copied from "ON LISP"

(defmacro while (test &body body)
  `(do ()
    ((not ,test))
    ,@body))

(defmacro for (var start stop &body body)
  (let ((gstop (gensym)))
    `(do ((,var ,start (1+ ,var))
          (,gstop ,stop))
       ((> ,var ,gstop))
       ,@body)))

(defmacro with-gensyms (syms &body body)
  `(let ,(mapcar #'(lambda (sym) `(,sym (gensym))) syms)
     ,@body))

(define-modify-macro ncons (val)
  (lambda (plc val) (cons val plc)))

(defun pairs (lst)
  "Calculates the pairs of a given list"
  (mapcon #'(lambda (lst1)
              (mapcar #'(lambda (x)
                          (cons (car lst1) x)) (cdr lst1))) lst))

(defun range (n)
  (if (= n 0) (list n)
    (cons n (range (- n 1)))))

(defun single (lst)
  (and (consp lst) (not (cdr lst))))

(defun most (fn lst &optional (comp #'>))
  (if (null lst)
    (values nil nil)
    (let* ((wins (car lst))
           (max (funcall fn wins)))
      (dolist (obj (cdr lst))
        (let ((score (funcall fn obj)))
          (when (funcall comp score max)
            (setq wins obj
                  max score))))
     (values wins max))))

(defun mostn (fn lst &optional (comp #'>))
  "Returns the x of lst for which (fn x) returned the max"
  (if (null lst)
    (values nil nil)
    (let ((result (list (car lst)))
          (max (funcall fn (car lst))))
      (dolist (obj (cdr lst))
        (let ((score (funcall fn obj)))
          (cond ((funcall comp score max) (setq max score
                                     result (list obj)))
                ((= score max) (push obj result)))))
      (values (nreverse result) max))))

(defun forall (p lst)
  (if (null lst) t
    (and (funcall p (car lst)) (forall p (cdr lst)))))

(defun exists (p lst)
  (if (null lst) nil
    (or (funcall p (car lst)) (exists p (cdr lst)))))

(defun contains (x lst)
  (if (null lst) nil
    (or (eql x (car lst)) (contains x (cdr l)))))

(defun subset (x y)
  (null (set-difference x y)))

(defmacro mvdo* (parm-cl test-cl &body body)
  (mvdo-gen parm-cl parm-cl test-cl body))

(defun mvdo-gen (binds rebinds test body)
  (if (null binds)
    (let ((label (gensym)))
      `(prog nil
             ,label
             (if ,(car test)
               (return (progn ,@(cdr test))))
             ,@body
             ,@(mvdo-rebind-gen rebinds)
             (go ,label)))
    (let ((rec (mvdo-gen (cdr binds) rebinds test body)))
      (let ((var/s (caar binds)) (expr (cadar binds)))
        (if (atom var/s)
          `(let ((,var/s ,expr)) ,rec)
          `(multiple-value-bind ,var/s ,expr ,rec))))))

(defun mvdo-rebind-gen (rebinds)
  (cond ((null rebinds) nil)
        ((< (length (car rebinds)) 3)
         (mvdo-rebind-gen (cdr rebinds)))
        (t
         (cons (list (if (atom (caar rebinds))
                       'setq
                       'multiple-value-setq)
                     (caar rebinds)
                     (third (car rebinds)))
               (mvdo-rebind-gen (cdr rebinds))))))

(defmacro mvprint (&rest rest)
  `(progn
     ,@(mapcar #'(lambda (x) `(print ,x)) rest)))

;; (pprint (macroexpand `(mvprint 1 2 3)))


(defmacro mvlet* (binds &body body)
  (mvlet-gen binds body))

(defun mvlet-gen (binds body)
  (if (null binds)
    `(progn ,@body)
    (let ((rec (mvlet-gen (cdr binds) body))
          (var/s (caar binds))
          (expr (cadar binds)))
      (if (atom var/s)
        `(let ((,var/s ,expr)) ,rec)
        `(multiple-value-bind ,var/s ,expr ,rec)))))

;; (mvlet (((x y z) (values 1 2 3)))
;;   (mvprint x y z))

;; (pprint
;;  (macroexpand-1
;;   `(mvlet (((x y) (values 1 2))
;;           ((z v) (values x y)))
;;     (mvprint z v))
;;   )
;;  )

;; anaphoric macros
(defmacro aif (test-form then-form &optional else-form)
  `(let ((it ,test-form))
     (if it ,then-form ,else-form)))

;; TODO get this to work
(let ((ht (make-hash-table))
      (cnt -1))
  (defun getid (x)
    (aif (gethash x ht)
      it
      (setf it (incf cnt)))))

;;generalized variables
(defmacro allf (val &rest args)
  (with-gensyms (gval)
    `(let ((,gval ,val))
       (setf ,@(mapcan #'(lambda (a) (list a gval)))
             args))))

