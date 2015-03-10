;;; prelude

(in-package :trivia.emilie2006)

(defoptimizer :emilie2006 (types clauses)
  )

;;; Fusion

(defun gensym* (name)
  (lambda (x)
    (declare (ignore x))
    (gensym name)))

(defun if-fusion (c1 c2)
  (match* (c1 c2)
    (((list ($guard1 s1 (property :type type1) test1 more1) body1)
      (list ($guard2 s2 (property :type type2) test2 more2) body2))
     (if (eq (test-type test1)
             (test-type test2))
         (with-gensyms (fusion)
           (let* ((more1+ (plist-alist (subst fusion s1 more1)))
                  (more2+ (plist-alist (subst fusion s2 more2)))
                  (generators (mapcar #'car (union more1+ more2+ :key #'car :test #'equal)))
                  (tmps (mapcar (gensym* "TMP") generators))
                  (more3+ (mapcar #'cons generators tmps)))
             `((guard1 (,fusion :type ,type1)
                       ,(subst fusion s1 test1)
                       ,@(alist-plist more3+))
               (match* ,temps
                 (,(mapcar (lambda (gen)
                             (or (cdr (assoc gen more1+)) '_))
                           generators) ,body1)
                 (,(mapcar (lambda (gen)
                             (or (cdr (assoc gen more2+)) '_))
                           generators) ,body2)))))))))
                 
                    
         
                  
         

;;; Interleaving

;;; Swapping

