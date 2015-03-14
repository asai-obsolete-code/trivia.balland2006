;;; prelude

(in-package :trivia.emilie2006)

(defoptimizer :emilie2006 (clauses &key types &allow-other-keys)
  (let ((*print-length* 3))
    (emilie2006 clauses
                (or types
                    (make-list (reduce #'max
                                       (mapcar
                                        (compose #'length #'first)
                                        clauses))
                               :initial-element t)))))

(defun emilie2006 (clauses types)
  (let ((% clauses))
    (iter (for prev = %)
          (setf % (apply-or-grounding %))
          (setf % (apply-swapping     % types))
          (setf % (apply-fusion       % types))
          (setf % (apply-interleaving % types))
          (until (equal % prev)))
    %))

;;; or lifting

(defun apply-or-grounding (clauses)
  (let ((new (mappend #'ground-or clauses)))
    (if (not (equal clauses new))
        (apply-or-grounding new)
        clauses)))

(defun ground-or (clause)
  (ematch clause
    ((list* (list* (list* 'guard1 _) _) _)
     (list clause))
    ((list* (list* (list* 'or1 subpatterns) rest) body)
     (format t "~&~<; ~@;Grounding~_ ~s~:>" (list clause))
     ;; overrides the default or1 compilation
     (mappend (lambda (x)
                ;; this inflates the code size, but let's ignore it for the sake of speed!
                (if rest
                    (mapcar (lambda-ematch
                              ((list* rest body)
                               (list* (list* x rest) body)))
                            (ground-or (list* rest body)))
                    (list (list* (list x) body))))
              subpatterns))))

;;; Fusion

(defun apply-fusion (clauses types)
  (mappend (rcurry #'fuse types) (divide-clauses clauses types)))

(defun divide-clauses (clauses types &aux (under (first types)))
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
    (((list* (list* ($guard1 s1 _ test1 _) _) _)
      (list* (list* ($guard1 s2 _ test2 _) _) _))
     (multiple-value-bind (type1 ok1) (test-type (? s1 test1))
       (multiple-value-bind (type2 ok2) (test-type (? s2 test2))
         (and ok1 ok2
              (type-equal type1 type2 under)))))))

(defun gen-union (&optional x y)
  (union x y :test #'equal))

(defun fuse (clauses types)
  (unless (cdr clauses)
    (return-from fuse clauses))
  ;; assumes all clauses are fusible
  (with-gensyms (fusion)
    (labels ((sym  (c) (ematch c ((list* (list* ($guard1 x _ _ _) _) _) x)))
             (type (c) (ematch c ((list* (list* ($guard1 _ (property :type x) _ _) _) _) x)))
             (test (c) (ematch c ((list* (list* ($guard1 _ _ x _) _) _) x)))
             (more (c) (ematch c ((list* (list* ($guard1 _ _ _ x) _) _) x)))
             (pat* (c) (ematch c ((list* (list* ($guard1 _ _ _ _) x) _) x)))
             (body (c) (ematch c ((list* (list* ($guard1 _ _ _ _) _) x) x)))
             (generator-alist (x) (plist-alist (subst fusion (sym x) (more x)))))
      (let* ((c (first clauses))
             (more1 (mapcar #'generator-alist clauses))
             (generators (reduce #'gen-union (mapcar (curry #'mapcar #'car) more1)))
             (gen-tmps (mapcar (gensym* "GEN") generators))
             (more2 (mapcar #'cons generators (mapcar #'pattern-expand-all gen-tmps)))
             (pat*-tmps (mapcar (gensym* "PAT") (pat* c)))
             (pat*-pats (mapcar #'pattern-expand-all pat*-tmps))
             (pat** (mapcar (lambda (c) (subst fusion (sym c) (pat* c))) clauses)))
        (format t "~&~<; ~@;fusing~_ ~{~4t~s~^, ~_~}~:>"
                (list (mapcar (compose #'first #'first) clauses)))
        ;; (if (every (curry #'eq t) (mapcar #'test clauses))
        ;;     ;; then level1 can handle it, and further fusion results in infinite recursion
        ;;     clauses

            `((((guard1 (,fusion :type ,(type c))
                        ,(subst fusion (sym c) (test c))
                        ,@(mappend (lambda (c) (list fusion `(guard1 (,(sym c) :type ,(type c)) t)))
                                   (remove-duplicates clauses :key #'sym))
                        ,@(alist-plist more2))
                ,@pat*-pats)
               (match2*+ (,@gen-tmps ,@pat*-tmps)
                   (,@(mapcar (constantly t) gen-tmps) ,@(rest types))
                 ,@(mapcar (lambda (c m pat*)
                             `((,@(mapcar (lambda (gen)
                                            (or (cdr (assoc gen m :test #'equal)) '_))
                                          generators)
                                  ,@pat*)
                               ,@(body c)))
                           clauses more1 pat**)
                 ;; 
                 ;;     +-- this (next) makes the failure propagate upwards correctly
                 ;;    / 
                 (_ (next)))))))));)

;;; Interleaving

(defun apply-interleaving (clauses types &aux (under (first types)))
  ;; be more conservative than Emilie 2006:
  ;; apply only once by each call
  (ematch clauses
    ((list) nil)
    ((list _) clauses)
    ((list* c1 (and rest1 (list* c2 rest2)))
     (if-let ((c12 (interleave c1 c2 types)))
       (progn (format t "~&~<; ~@;interleaving ~_ ~s,~_ ~s~_ under ~s~:>"
                      (list (first (first c1))
                            (first (first c2))
                            under))
              (cons c12 rest2))
       (cons c1 (apply-interleaving rest1 types))))))

(defun interleave (c1 c2 types &aux (under (first types)))
  (ematch* (c1 c2)
    (((list* (list* ($guard1 s1 o1 test1 more1) rest1) body1)
      (list* (list* ($guard1 s2 o2 test2 more2) rest2) body2))
     (multiple-value-bind (type1 ok1) (test-type (? s1 test1))
       (multiple-value-bind (type2 ok2) (test-type (? s2 test2))
         (when (and ok1 ok2)
           (cond
             ((type-disjointp type1 under) c2)
             ((type-disjointp type2 under) c1)
             ((type-exhaustivep type1 type2 under)
              ;; exhaustive partition
              (with-gensyms (il)
                `((guard1 (,il :type ,under) t)
                  (match2*+ ,il ,types
                    (((guard1 ,(list* s1 o1) ,test1 ,@more1) ,rest1) ,@body1)
                    (((guard1 ,(list* s2 o2) t      ,@more2) ,rest2) ,@body2)
                    ;; 
                    ;;     +-- this (next) makes the failure propagate upwards correctly
                    ;;    / 
                    (_ (next)))))))))))))

;;; Swapping

(defun apply-swapping (clauses types &aux (under (first types)))
  ;; runs swap sort
  (let* ((v (coerce clauses 'vector))
         (len (length v)))
    (iter
      (while
          (iter (for i from 1 below len)
                (for j = (1- i))
                (when (swappable (aref v i) (aref v j) under)
                  (format t "~&~<; ~@;swapping~_ ~s,~_ ~s~_ under ~s~:>"
                          (list (first (first (aref v j)))
                                (first (first (aref v i))) under))
                  (rotatef (aref v i) (aref v j))
                  (leave t)))))
    (coerce v 'list)))


(defun swappable (c1 c2 &optional (under t))
  (ematch* (c1 c2)
    (((list* (list* ($guard1 s1 _ test1 _) _) _)
      (list* (list* ($guard1 s2 _ test2 _) _) _))
     (multiple-value-bind (type1 ok1) (test-type (? s1 test1))
       (multiple-value-bind (type2 ok2) (test-type (? s2 test2))
         (and ok1 ok2
              (type-disjointp type1 type2 under)
              (< (sxhash type1) (sxhash type2))))))))


