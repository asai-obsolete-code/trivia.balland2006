;;; prelude

(in-package :trivia.emilie2006)

(defoptimizer :emilie2006 (clauses &key (type t) &allow-other-keys)
  (let ((*print-length* 3))
    (emilie2006 clauses type)))

(defun emilie2006 (clauses &optional (under t))
  (let ((% clauses))
    (iter (for prev = %)
          (setf % (apply-or-grounding % under))
          (setf % (apply-swapping     % under))
          (setf % (apply-fusion       % under))
          (setf % (apply-interleaving % under))
          (until (equal % prev)))
    %))

;;; or lifting

(defun apply-or-grounding (clauses &optional (under t))
  (mapcar (rcurry #'ground-or under) clauses))

(defun ground-or (clause &optional (under t))
  (declare (ignorable under))
  (ematch clause
    ((list* (list* 'guard1 _) _) clause)
    ((list* (and pattern (list* 'or1 subpatterns)) body)
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
                    ,@body))
             (declare (dynamic-extent (function ,fn)))
             (match2+ ,it ,under
               ,@(mapcar (lambda (pattern)
                                `(,pattern (,fn ,@syms)))
                              subpatterns)
                      ;; 
                      ;;     +-- this (next) makes the failure propagate upwards correctly
                      ;;    / 
               (_ (next))))))))))

;;; Fusion

(defun apply-fusion (clauses &optional (under t))
  (mappend #'fuse (divide-clauses clauses under)))

(defun divide-clauses (clauses &optional (under t))
  (ematch clauses
    ((list) nil)
    ((list c) (list (list c)))
    ((list* _ _)
     (iter (for c in clauses)
           (with tmp = nil)
           (with acc = nil)
           (if (emptyp tmp)
               (push c tmp)
               (if (fusiblep (car tmp) c under)
                   (push c tmp)
                   (progn (push (nreverse tmp) acc)
                          (setf tmp (list c)))))
           (finally
            (push (nreverse tmp) acc)
            (return (nreverse acc)))))))

(defun type-disjointp (t1 t2 &optional (under t))
  (subtypep `(and ,under ,t1 ,t2) nil))

(defun type-exhaustivep (t1 t2 &optional (under t))
  (and (type-disjointp t1 t2 under)
       (subtypep under `(or ,t1 ,t2))))

(defun type-equal (t1 t2 &optional (under t))
  (type= `(and ,under ,t1) `(and ,under ,t2)))

(defun gensym* (name)
  (lambda (x)
    (declare (ignore x))
    (gensym name)))

(defun fusiblep (c1 c2 &optional (under t))
  (ematch* (c1 c2)
    (((list* ($guard1 s1 _ test1 _) _)
      (list* ($guard1 s2 _ test2 _) _))
     (type-equal (test-type (? s1 test1))
                 (test-type (? s2 test2))
                 under))))

(defun gen-union (&optional x y)
  (union x y :test #'equal))

(defun fuse (clauses)
  (unless (cdr clauses)
    (return-from fuse clauses))
  ;; assumes all clauses are fusible
  (with-gensyms (fusion)
    (labels ((sym  (c) (ematch c ((list* ($guard1 x _ _ _) _) x)))
             (type (c) (ematch c ((list* ($guard1 _ (property :type x) _ _) _) x)))
             (test (c) (ematch c ((list* ($guard1 _ _ x _) _) x)))
             (more (c) (ematch c ((list* ($guard1 _ _ _ x) _) x)))
             (body (c) (ematch c ((list* ($guard1 _ _ _ _) x) x)))
             (generator-alist (x) (plist-alist (subst fusion (sym x) (more x)))))
      (if (every (curry #'eq t) (mapcar #'test clauses))
          ;; then level1 can handle it, and further fusion results in infinite recursion
          clauses
          (let* ((c (first clauses))
                 (more1 (mapcar #'generator-alist clauses))
                 (generators (reduce #'gen-union (mapcar (curry #'mapcar #'car) more1)))
                 (tmps (mapcar (gensym* "TMP") generators))
                 (more2 (mapcar #'cons generators (mapcar #'pattern-expand-all tmps))))
            (format t "~&~<; ~@;fusing~_ ~{~4t~A~^, ~_~}~:>" (list clauses))
            `(((guard1 (,fusion :type ,(type c))
                       ,(subst fusion (sym c) (test c))
                       ,@(alist-plist more2))
               (match2* ,tmps
                 ,@(mapcar (lambda (c m)
                             `(,(mapcar (lambda (gen)
                                          (or (cdr (assoc gen m :test #'equal)) '_))
                                        generators)
                                ,@(body c)))
                           clauses more1)
                 ;; 
                 ;;     +-- this (next) makes the failure propagate upwards correctly
                 ;;    / 
                 (_ (next))))))))))

;;; Interleaving

(defun apply-interleaving (clauses &optional (under t))
  ;; be more conservative than Emilie 2006:
  ;; apply only once by each call
  (ematch clauses
    ((list) nil)
    ((list _) clauses)
    ((list* c1 (and rest1 (list* c2 rest2)))
     (if-let ((c12 (interleave c1 c2 under)))
       (progn (format t "~&~<; ~@;interleaving ~_ ~W,~_ ~W~:>" (list c1 c2))
              (cons c12 rest2))
       (cons c1 (apply-interleaving rest1 under))))))

(defun interleave (c1 c2 &optional (under t))
  (ematch* (c1 c2)
    (((list* ($guard1 s1 o1 test1 more1) body1)
      (list* ($guard1 s2 o2 test2 more2) body2))
     (let ((type1 (test-type (? s1 test1)))
           (type2 (test-type (? s2 test2))))
       (cond
         ((type-disjointp type1 under) c2)
         ((type-disjointp type2 under) c1)
         ((type-exhaustivep type1 type2 under)
          ;; exhaustive partition
               (with-gensyms (il)
                 `((guard1 (,il :type ,under) t)
                   (match2+ ,il ,under
                     ((guard1 ,(list* s1 o1) ,test1 ,@more1) ,@body1)
                     ((guard1 ,(list* s2 o2) t ,@more2) ,@body2)
                     ;; 
                     ;;     +-- this (next) makes the failure propagate upwards correctly
                     ;;    / 
                     (_ (next)))))))))))

;;; Swapping

(defun apply-swapping (clauses &optional (under t))
  ;; runs swap sort
  (let* ((v (coerce clauses 'vector))
         (len (length v)))
    (iter
      (while
          (iter (for i from 1 below len)
                (for j = (1- i))
                (when (swappable (aref v i) (aref v j) under)
                  (format t "~&~<; ~@;swapping~_ ~W,~_ ~W~:>"
                          (list (aref v j) (aref v i)))
                  (rotatef (aref v i) (aref v j))
                  (leave t)))))
    (coerce v 'list)))


(defun swappable (c1 c2 &optional (under t))
  (ematch* (c1 c2)
    (((list* ($guard1 s1 _ test1 _) _)
      (list* ($guard1 s2 _ test2 _) _))
     (let ((type1 (test-type (? s1 test1)))
           (type2 (test-type (? s2 test2))))
       (and (type-disjointp type1 type2 under)
            (< (sxhash type1) (sxhash type2)))))))


