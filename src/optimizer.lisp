;;; prelude

(in-package :trivia.emilie2006)

(defoptimizer :emilie2006 (type clauses)
  (let ((% clauses))
    (iter (for prev = %)
          (setf % (apply-swapping     % type))
          (setf % (apply-fusion       % type))
          (setf % (apply-interleaving % type))
          (until (equal % prev)))
    %))

;;; Fusion

(defun apply-fusion (clauses &optional (under t))
  (mapcar #'fuse (divide-clauses under clauses #'fusiblep)))

(defun divide-clauses (under clauses fn)
  (iter (for c in clauses)
        (with tmp = nil)
        (if (emptyp tmp)
            (push c tmp)
            (if (apply fn (car tmp) c under)
                (push c tmp)
                (progn (collect (nreverse tmp))
                       (setf tmp (list c)))))))

(defun type-equal (t1 t2 &optional (under t))
  (type= `(and ,t1 ,under)
         `(and ,t2 ,under)))

(defun type-disjointp (t1 t2 &optional (under t))
  (subtypep `(and ,under ,t1 ,t2) 'nil))

(defun gensym* (name)
  (lambda (x)
    (declare (ignore x))
    (gensym name)))

(defun fusiblep (c1 c2 &optional (under t))
  (ematch* (c1 c2)
    (((list ($guard1 s1 (property :type type1) test1 _) _)
      (list ($guard1 s2 (property :type type2) test2 _) _))
     (let ((type1 `(and ,type1 ,(test-type (? s1 test1))))
           (type2 `(and ,type2 ,(test-type (? s2 test2)))))
       (type-equal type1 type2 under)))))

(defun gen-union (&optional x y)
  (union x y :test #'equal))

(defun fuse (clauses)
  ;; assumes all clauses are fusible
  (with-gensyms (fusion)
    (labels ((sym  (c) (ematch c ((list ($guard1 x (property :type _) _ _) _) x)))
             (type (c) (ematch c ((list ($guard1 _ (property :type x) _ _) _) x)))
             (test (c) (ematch c ((list ($guard1 _ (property :type _) x _) _) x)))
             (more (c) (ematch c ((list ($guard1 _ (property :type _) _ x) _) x)))
             (body (c) (ematch c ((list ($guard1 _ (property :type _) _ _) x) x)))
             (generator-alist (x) (plist-alist (subst fusion (sym x) (more x)))))
      (let* ((c (first clauses))
             (more1 (mapcar #'generator-alist clauses))
             (generators (reduce #'gen-union (mapcar #'car more1)))
             (tmps (mapcar (gensym* "TMP") generators))
             (more2 (mapcar #'cons generators tmps)))
        `((guard1 (,fusion :type ,(type c))
                  ,(subst fusion (sym c) (test c))
                  ,@(alist-plist more2))
          (match* ,tmps
            ,@(mapcar (lambda (c m)
                        `(,(mapcar (lambda (gen)
                                     (or (cdr (assoc gen m :test #'equal)) '_))
                                   generators)
                           ,(body c)))
                      clauses more1)))))))
                    
;;; Interleaving

(defun apply-interleaving (clauses &optional (under t))
  ;; be more conservative than Emilie 2006:
  ;; apply only once by each call
  (ematch clauses
    ((list* c1 (and rest1 (list* c2 rest2)))
     (if-let ((c12 (interleave c1 c2 under)))
       (cons c12 rest2)
       (cons c1 (apply-interleaving rest1 under))))))

(defun interleave (c1 c2 &optional (under t))
  (ematch* (c1 c2)
    (((list ($guard1 s1 (and o1 (property :type type1)) test1 more1) body1)
      (list ($guard1 s2 (and o2 (property :type type2)) test2 more2) body2))
     (let ((type1 `(and ,type1 ,(test-type (? s1 test1))))
           (type2 `(and ,type2 ,(test-type (? s2 test2)))))
       (if (type-disjointp type1 type2 under)
           (with-gensyms (il)
             `((and ,il (type ,under))
               (match* ,il
                 ((guard1 ,(list* s1 o1) ,test1 ,@more1) ,body1)
                 ((guard1 ,(list* s2 o2) t ,@more2) ,body2)))))))))

;;; Swapping

(defun apply-swapping (clauses &optional (under t))
  ;; runs swap sort
  (let* ((v (coerce 'vector clauses))
         (len (length v)))
    (iter
      (while
          (iter (for i from 1 below len)
                (for j = (1- i))
                (when (swappable (aref v i) (aref v j) under)
                  (rotatef (aref v i) (aref v j))
                  (leave t)))))
    (coerce 'list v)))


(defun swappable (c1 c2 &optional (under t))
  (ematch* (c1 c2)
    (((list ($guard1 s1 (property :type type1) test1 _) _)
      (list ($guard1 s2 (property :type type2) test2 _) _))
     (let ((type1 `(and ,type1 ,(test-type (? s1 test1))))
           (type2 `(and ,type2 ,(test-type (? s2 test2)))))
       (and (type-disjointp type1 type2 under)
            (< (sxhash type1) (sxhash type2)))))))


