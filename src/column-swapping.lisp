(in-package :trivia.balland2006)

;;; Column-swapping

;; Swapping rule in Balland2006 swaps two clauses on each application. This
;; is called a row swapping, in a sense. However, another kind of swapping
;; can be considered, called column-swapping. This is analogous to changing
;; the ordering of variable in Ordered-BDD, which severely affects the
;; quality of the decision tree (= size of the decision tree, number of
;; nodes in a decision tree, or a number of test-forms in a matching
;; tree). It does not increase the number of checks.

;; quote from Rice, Kulhari, "A Survey of Static Variable Ordering
;; Heuristics for Efficient BDD/MDD Construction" :

;;     The problem of finding an optimal variable ordering for Binary
;;     Decision Diagrams (BDD) or Multi-Valued Decision Diagrams (MDD) is
;;     widely known to be NP-Complete. ... in order to minimize the overall
;;     size of the resulting decision diagram.

;;; Dependency Between Patterns

;; One problem in applying these techniques to pattern matching is the
;; dependency between the test forms. For example, A check on a value of a
;; slot of an object should be done after the parent object is
;; checked. However, this itself is largely no problem since the
;; subpatterns of guard1 are considered dependent on the pattern.

;; The remaining degree of freedom is in the ordering of subpatterns, and
;; the ordering of multipatterns. Since subpatterns are compiled into
;; multipatterns when a fusion occurs, this can simply be reduced to a
;; problem of swapping and changing the ordering of multipatterns.

;; User-supplied multipatterns are not safe, since they
;; may contain references to the variables in the earlier patterns, e.g.,
;; 
;; (match* (1 2 3) (((list a b c) (eq a) (eq b)) c)) 

;; We check if the variables in (list a b c) appears in (eq a) and (eq b).
;; Now this only applies to the symbols used in the pattern. Since any
;; implicitly defined symbols are supposed to be created by gensyms, they
;; would not overwrap with each other, and thus, we can safely assume that
;; checking the value returned by `variables' is enough. Note that this
;; does not require any type information.

(defun pattern-dependent (p1 p2)
  "return true if p2 depends on p1"
  (some (lambda (symopt) (find-tree (car symopt) p2))
        (variables p1)))

(defun find-tree (obj tree &key (test #'eql))
  (labels ((rec (tree)
             (match tree
               ((list) nil)
               ((list* (and thing (type list)) rest)
                (or (funcall test obj thing)
                    (some #'rec thing)
                    (rec rest)))
               ((list* thing rest)
                (or (funcall test obj thing)
                    (rec rest))))))
    (rec tree)))

(defun pattern-dependencies (patterns)
  (let ((pv (coerce patterns 'vector)))
    (iter outer
          (for p1 in-vector pv with-index i)
          (iter (for p2 in-vector pv with-index j from (1+ i))
                (when (pattern-dependent p1 p2)
                  (in outer (collect (list i j))))))))


