(in-package :trivia.balland2006)

;;; Column-swapping

;; Swapping rule in Balland2006 swaps two clauses on each application. This
;; is called a row swapping, in a sense. However, another kind of swapping
;; can be considered, called column-swapping. This is analogous to the
;; ordering of variable in Orderd-BDD, which severely affects the quality
;; of the decision tree.  In our case, it is actually a Multi-valued
;; decision diagram (MDD).

;; quote from Rice, Kulhari, "A Survey of Static Variable Ordering
;; Heuristics for Efficient BDD/MDD Construction" :

;;     The problem of finding an optimal variable ordering for Binary
;;     Decision Diagrams (BDD) or Multi-Valued Decision Diagrams (MDD) is
;;     widely known to be NP-Complete. This pa- per presents a survey of
;;     static heuristic techniques applied to ordering the variables of the
;;     BDD/MDD under construction in order to minimize the overall size of
;;     the resulting decision diagram.

;; One problem in applying these techniques to pattern matching is the
;; dependency between the test forms. For example, A check on a value of a
;; slot of an object should be done after the object is checked. However,
;; this is largely no problem since the subpatterns of guard1 are
;; considered dependent on the pattern.

;; thus, the remaining degree of freedom is in the ordering of subpatterns,
;; and the ordering of multipatterns. Since subpatterns are compiled into
;; multipatterns when a fusion occurs, this can simply be reduced to a
;; problem of swapping and changing the ordering of multipatterns.

;; However, user-supplied multipatterns are not safe, since they
;; may contain references to the variables in the earlier patterns, e.g.,
;; 
;; (match* (a b c)
;;   (((list a b c) (eq a) (eq b))
;;    c))
;; 
;; now this only applies to the interned, explicit patterns. Therefore, we can check
;; the variables in (list a b c) appears in (eq a) and (eq b).
;; Note that this does not require any type information.

(defun pattern-swappable (p1 p2)
  (notany (lambda (symopt) (find-tree (car symopt) p2))
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

