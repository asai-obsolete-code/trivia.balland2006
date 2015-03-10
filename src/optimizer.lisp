;;; prelude

(in-package :trivia.emilie2006)

(defoptimizer :emilie2006 (types clauses)

  

  )

;;; Fusion

(defun type-equal (t1 t2 &optional (under t))
  (type= `(and ,t1 ,under)
         `(and ,t2 ,under)))

(defun type-disjointp (t1 t2 &optional (under t))
  (subtypep `(and ,under ,t1 ,t2) 'nil))

(defun gensym* (name)
  (lambda (x)
    (declare (ignore x))
    (gensym name)))

(defun if-fusion (c1 c2 &optional (under t))
  (ematch* (c1 c2)
    (((list p1 body1)
      (list p2 body2))
     (%if-fusion
      `(,(pattern-expand-all p1) ,body1)
      `(,(pattern-expand-all p2) ,body2)
      under))))

(defun %if-fusion (c1 c2 under)
  (match* (c1 c2)
    (((list ($guard1 s1 (property :type type1) test1 more1) body1)
      (list ($guard1 s2 (property :type type2) test2 more2) body2))
     (let ((type1 `(and ,type1 ,(test-type (? s1 test1))))
           (type2 `(and ,type2 ,(test-type (? s2 test2)))))
       (if (type-equal type1 type2 under)
           (values
            (with-gensyms (fusion)
              (let* ((more1+ (plist-alist (subst fusion s1 more1)))
                     (more2+ (plist-alist (subst fusion s2 more2)))
                     (generators (mapcar #'car (union more1+ more2+ :key #'car :test #'equal)))
                     (tmps (mapcar (gensym* "TMP") generators))
                     (more3+ (mapcar #'cons generators tmps)))
                `(((guard1 (,fusion :type ,type1)
                           ,(subst fusion s1 test1)
                           ,@(alist-plist more3+))
                   (match* ,tmps
                     (,(mapcar (lambda (gen)
                                 (or (cdr (assoc gen more1+ :test #'equal)) '_))
                               generators) ,body1)
                     (,(mapcar (lambda (gen)
                                 (or (cdr (assoc gen more2+ :test #'equal)) '_))
                               generators) ,body2))))))
            t)
           (list c1 c2))))
    (_ (list c1 c2))))

                    
;;; Interleaving

(defun interleaving (c1 c2 &optional (under t))
  (ematch* (c1 c2)
    (((list p1 body1)
      (list p2 body2))
     (%interleaving
      `(,(pattern-expand-all p1) ,body1)
      `(,(pattern-expand-all p2) ,body2)
      under))))

(defun %interleaving (c1 c2 under)
  (match* (c1 c2)
    (((list ($guard1 s1 (and o1 (property :type type1)) test1 more1) body1)
      (list ($guard1 s2 (and o2 (property :type type2)) test2 more2) body2))
     (let ((type1 `(and ,type1 ,(test-type (? s1 test1))))
           (type2 `(and ,type2 ,(test-type (? s2 test2)))))
       (if (type-disjointp type1 type2 under) ;; under...?
           (values
            (with-gensyms (il)
              `(((and ,il (type ,under))
                 (match* ,il
                   ((guard1 ,(list* s1 o1) ,test1 ,@more1) ,body1)
                   ((guard1 ,(list* s2 o2) t ,@more2) ,body2)))))
            t)
           (list c1 c2))))
    (_ (list c1 c2))))

;;; Swapping


(defun swapping (c1 c2 &optional (under t))
  (ematch* (c1 c2)
    (((list p1 body1)
      (list p2 body2))
     (%swapping
      `(,(pattern-expand-all p1) ,body1)
      `(,(pattern-expand-all p2) ,body2)
      under))))

(defun %swapping (c1 c2 under)
  (match* (c1 c2)
    (((list ($guard1 s1 (property :type type1) test1 _) _)
      (list ($guard1 s2 (property :type type2) test2 _) _))
     (let ((type1 `(and ,type1 ,(test-type (? s1 test1))))
           (type2 `(and ,type2 ,(test-type (? s2 test2)))))
       (if (type-disjointp type1 type2 under) ;; under...?
           (values (list c2 c1) t)
           (list c1 c2))))
    (_ (list c1 c2))))

