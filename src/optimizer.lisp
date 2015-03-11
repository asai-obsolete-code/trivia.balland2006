;;; prelude

(in-package :trivia.emilie2006)

(defoptimizer :emilie2006 (clauses)
  (let ((% clauses))
    (iter (for prev = %)
          (setf % (apply-or-grounding %))
          (setf % (apply-swapping     %))
          (setf % (apply-fusion       %))
          (setf % (apply-interleaving %))
          (until (equal % prev)))
    %))

;;; or lifting

(defun apply-or-grounding (clauses)
  (mapcar #'ground-or clauses))

(defun ground-or (clause)
  (ematch clause
    ((list (list* 'guard1 *) _) clause)
    ((list (and pattern (list* 'or1 subpatterns)) body)
     ;; overrides the default or1 compilation
     (let* ((vars (variables pattern))
            (syms (mapcar #'first vars)))
       (with-gensyms (it fn)
         `((guard1 ,it t)
           (flet ((,fn ,syms
                    (declare
                     ,@(mapcar (lambda-ematch
                                 ((list* var options)
                                  `(type ,(getf options :type) ,var)))
                               vars))
                    ,body))
             (declare (dynamic-extent (function ,fn)))
             (match2 ,it
               ,@(mapcar (lambda (pattern)
                           `(,pattern (,fn ,@syms)))
                         subpatterns)
               (_ (next))))))))))

;;; Fusion

(defun apply-fusion (clauses)
  (mappend #'fuse (divide-clauses clauses #'fusiblep)))

(defun divide-clauses (clauses fn)
  (ematch clauses
    ((list) nil)
    ((list c) (list (list c)))
    ((list* _ _)
     (iter (for c in clauses)
           (with tmp = nil)
           (with acc = nil)
           (if (emptyp tmp)
               (push c tmp)
               (if (funcall fn (car tmp) c)
                   (push c tmp)
                   (progn (push (nreverse tmp) acc)
                          (setf tmp (list c)))))
           (finally
            (push (nreverse tmp) acc)
            (return (nreverse acc)))))))

(defun type-disjointp (t1 t2)
  (subtypep `(and ,t1 ,t2) 'nil))

(defun gensym* (name)
  (lambda (x)
    (declare (ignore x))
    (gensym name)))

(defun fusiblep (c1 c2)
  (ematch* (c1 c2)
    (((list ($guard1 s1 (property :type type1) test1 _) _)
      (list ($guard1 s2 (property :type type2) test2 _) _))
     (let ((type1 `(and ,type1 ,(test-type (? s1 test1))))
           (type2 `(and ,type2 ,(test-type (? s2 test2)))))
       (type= type1 type2)))))

(defun gen-union (&optional x y)
  (union x y :test #'equal))

(defun fuse (clauses)
  (unless (cdr clauses)
    (return-from fuse clauses))
  ;; assumes all clauses are fusible
  (with-gensyms (fusion)
    (labels ((sym  (c) (ematch c ((list ($guard1 x (property :type _) _ _) _) x)))
             (type (c) (ematch c ((list ($guard1 _ (property :type x) _ _) _) x)))
             (test (c) (ematch c ((list ($guard1 _ (property :type _) x _) _) x)))
             (more (c) (ematch c ((list ($guard1 _ (property :type _) _ x) _) x)))
             (body (c) (ematch c ((list ($guard1 _ (property :type _) _ _) x) x)))
             (generator-alist (x) (plist-alist (subst fusion (sym x) (more x)))))
      (if (every (curry #'eq t) (mapcar #'test clauses))
          ;; then level1 can handle it, and further fusion results in infinite recursion
          clauses
          (let* ((c (first clauses))
                 (more1 (mapcar #'generator-alist clauses))
                 (generators (reduce #'gen-union (mapcar (curry #'mapcar #'car) more1)))
                 (tmps (mapcar (gensym* "TMP") generators))
                 (more2 (mapcar #'cons generators (mapcar #'pattern-expand-all tmps))))
            `(((guard1 (,fusion :type ,(type c))
                       ,(subst fusion (sym c) (test c))
                       ,@(alist-plist more2))
               (match* ,tmps
                 ,@(mapcar (lambda (c m)
                             `(,(mapcar (lambda (gen)
                                          (or (cdr (assoc gen m :test #'equal)) '_))
                                        generators)
                                ,(body c)))
                           clauses more1)))))))))

;;; Interleaving

(defun apply-interleaving (clauses)
  ;; be more conservative than Emilie 2006:
  ;; apply only once by each call
  (ematch clauses
    ((list) nil)
    ((list _) clauses)
    ((list* c1 (and rest1 (list* c2 rest2)))
     (if-let ((c12 (interleave c1 c2)))
       (cons c12 rest2)
       (cons c1 (apply-interleaving rest1))))))

(defun interleave (c1 c2)
  (ematch* (c1 c2)
    (((list ($guard1 s1 (and o1 (property :type type1)) test1 more1) body1)
      (list ($guard1 s2 (and o2 (property :type type2)) test2 more2) body2))
     (let ((type1 `(and ,type1 ,(test-type (? s1 test1))))
           (type2 `(and ,type2 ,(test-type (? s2 test2)))))
       (if (type-disjointp type1 type2)
           (with-gensyms (il)
             `((guard1 ,il t)
               (match* ,il
                 ((guard1 ,(list* s1 o1) ,test1 ,@more1) ,body1)
                 ((guard1 ,(list* s2 o2) t ,@more2) ,body2)))))))))

;;; Swapping

(defun apply-swapping (clauses)
  ;; runs swap sort
  (let* ((v (coerce clauses 'vector))
         (len (length v)))
    (iter
      (while
          (iter (for i from 1 below len)
                (for j = (1- i))
                (when (swappable (aref v i) (aref v j))
                  (rotatef (aref v i) (aref v j))
                  (leave t)))))
    (coerce v 'list)))


(defun swappable (c1 c2)
  (ematch* (c1 c2)
    (((list ($guard1 s1 (property :type type1) test1 _) _)
      (list ($guard1 s2 (property :type type2) test2 _) _))
     (let ((type1 `(and ,type1 ,(test-type (? s1 test1))))
           (type2 `(and ,type2 ,(test-type (? s2 test2)))))
       (and (type-disjointp type1 type2)
            (< (sxhash type1) (sxhash type2)))))))


