;;;;;;;;;;;;;;;;;
(defDifferentiation Differentiation-of-Constant
  ((|$Derivative| SIMP) ?t ?var) 
  :TEST (not (occurs-in? ?var ?t))
  :RESULT (* 0 ?var))  
;;;;;;;;;;;;;;;;;

(defDifferentiation Differentiation-of-Self
  ((|$Derivative| SIMP)  ?exp ?exp)
  :RESULT 1)  

;;;;;;;;;;;;;;;;
  ;;in case constant arrives before variable, (* $a $x) for x
(defDifferentiation Move-Constant-outside
  ((|$Derivative| SIMP) (*  ?const ?nonconst) ?var)
  :TEST (and (occurs-in? ?var ?const)
	     (not (occurs-in? ?var ?nonconst)))
  :SUBPROBLEMS ((?int ((|$Differentiate| SIMP)  ((|$Derivative| SIMP) ?const ?var))))
  :RESULT (* ?nonconst ?int)) 
  
     
    ;;in case constant arrives after variable, (* $x $z) for x
(defDifferentiation Move-Constant-outside-1
  ((|$Derivative| SIMP) (*  ?const ?nonconst) ?var)
  :TEST (and (not (occurs-in? ?var ?const))
	          (occurs-in? ?var ?nonconst))
  :SUBPROBLEMS ((?int ((|$Differentiate| SIMP)  ((|$Derivative| SIMP) ?nonconst ?var))))
  :RESULT (* ?const ?int))  
 
;;;;;;;;;;;;;;;;  
(defDifferentiation Differentiation-of-Product
  ((|$Derivative| SIMP) (* ?const ?nonconst) ?var)
   :TEST (and (occurs-in? ?var ?const)
	      (occurs-in? ?var ?nonconst))
  :SUBPROBLEMS ((?int1 ((|$Differentiate| SIMP) ((|$Derivative| SIMP) ?const ?var)))
		(?int2 ((|$Differentiate| SIMP) ((|$Derivative| SIMP) ?nonconst ?var))))
  :RESULT (+ (* ?nonconst ?int1) (* ?const ?int2)))  

;;;;;;;;;;;;;;;
(defDifferentiation Differentiation-of-Sum
  ((|$Derivative| SIMP) (+ ?t1 ?t2) ?var)
  :SUBPROBLEMS ((?int1 ((|$Differentiate| SIMP) ((|$Derivative| SIMP) ?t1 ?var)))
		(?int2 ((|$Differentiate| SIMP) ((|$Derivative| SIMP) ?t2 ?var))))
  :RESULT (+ ?int1 ?int2))  
  
;;;;;;;;;;;;;;;  

  
;;;;;;;;;;;;;;;  

(defIntegration Integral-of-Nary-sum
  (Integral (+ ?t1 ?t2 . ?trest) ?var)
  :SUBPROBLEMS ((?int1 (Integrate (Integral ?t1 ?var)))
		(?int2 (Integrate (Integral ?t2 ?var)))
		(?intr (Integrate (Integral (+ . ?trest) ?var))))
  :TEST (not (null ?trest))
  :RESULT (+ ?int1 ?int2 ?intr))
  
;;;;;;;;;;;;;;;


(defDifferentiation Differentiation-of-Nary-sum
  ((|$Derivative| SIMP) (+ ?t1 ?t2 . ?trest) ?var)
  :SUBPROBLEMS ((?int1 ((|$Differentiate| SIMP) ((|$Derivative| SIMP) ?t1 ?var)))
		(?int2 ((|$Differentiate| SIMP) ((|$Derivative| SIMP) ?t2 ?var)))
		(?intr ((|$Differentiate| SIMP) ((|$Derivative| SIMP) (+ . ?trest) ?var))))
  :TEST (not (null ?trest))
  :RESULT (+ ?int1 ?int2 ?intr))
  
;;;;;;;;;;;;;;;    

(defIntegration Integral-of-uminus
  (Integral (- ?term) ?var)
  :SUBPROBLEMS ((?int (Integrate (Integral ?term ?var))))
  :RESULT (- ?int))
  
;;;;;;;;;;;;;;;
(defDifferentiation Differentiation-of-uminus
  ((|$Derivative| SIMP) (* -1 ?term) ?var)
  :SUBPROBLEMS ((?int ((|$Differentiate| SIMP) ((|$Derivative| SIMP) ?term ?var))))
  :RESULT (* -1 ?int))
;;;;;;;;;;;;;;    

(defIntegration Integral-of-minus
  (Integral (- ?t1 ?t2) ?var)
  :SUBPROBLEMS ((?int1 (Integrate (Integral ?t1 ?var)))
		(?int2 (Integrate (Integral ?t2 ?var))))
  :RESULT (- ?int1 ?int2))
  
;;;;;;;;;;;;;;
(defDifferentiation Differentiation-of-minus
  ((|$Derivative| SIMP) (- ?t1 ?t2) ?var)
  :SUBPROBLEMS ((?int1 ((|$Differentiate| SIMP) ((|$Derivative| SIMP) ?t1 ?var)))
		(?int2 ((|$Differentiate| SIMP) ((|$Derivative| SIMP) ?t2 ?var))))
  :RESULT (- ?int1 ?int2))
;;;;;;;;;;;;;;    

(defIntegration Integral-of-polyterm
  (Integral (expt ?var ?n) ?var)
  :TEST (not (same-constant? ?n -1))
  :RESULT (/ (expt ?var (+ 1 ?n)) (+ 1 ?n)))

  
(defDifferentiation Differentiation-of-nonconstant-polyterm
  ((|$Derivative| SIMP) (expt ?var ?n) ?var)
  :TEST (not (occurs-in? ?n ?var))
  :RESULT (* (expt ?var (+ -1 ?n)) ?n))
  
(defDifferentiation Differentiation-of-constant-polyterm
  ((|$Derivative| SIMP) (expt ?r ?n) ?var)
  :TEST (not (occurs-in? ?r ?var))
  :RESULT (* (expt ?r ?n) (log ?r)))      
  
  
(defDifferentiation Differentiation-of-polyterm
  ((|$Derivative| SIMP) (expt ?var ?n) ?var)
  :TEST (not (same-constant? ?n -1))
  :RESULT (* (expt ?var (- ?n 1)) ?n))       

;;;; Some exponentials and trig functions

(defDifferentiation Differentiate-Simple-e-integral
  ((|$Derivative| SIMP) (expt $%e ?var) ?var)
  :RESULT (expt $%e ?var))

(defDifferentiation Differentiation-complex-e-integral
  ((|$Derivative| SIMP) (expt $%e (* ?a ?var)) ?var)
  :SUBPROBLEMS ((?int1 ((|$Differentiate| SIMP) ((|$Derivative| SIMP) ?a ?var))))
  :TEST (occurs-in? ?var ?a)
  :RESULT (* (expt $%e (* ?a ?var)) ?int1))
  
(defDifferentiation Differentiation-e-constant-power
  ((|$Derivative| SIMP) (expt $%e (* ?a ?var)) ?var)
  :TEST (not (occurs-in? ?var ?a))
  :RESULT (* (expt $%e (* ?a ?var)) ?a))  

(defIntegration non-e-power-integral
  (Integral (expt ?b (* ?a ?var)) ?var)
  :TEST (and (not (occurs-in? ?var ?a))
	     (not (occurs-in? ?var ?b)))
  :RESULT (/ (expt ?b (* ?a ?var)) (* ?a (log ?b %e))))
  
(defDifferentiation Log-Differentiation
  ((|$Derivative| SIMP)  (log ?var) ?var)
  :RESULT (/ 1 ?var)) 
  
(defDifferentiation Log-power-Differentiation
  ((|$Derivative| SIMP)  (expt (log ?r) ?n) ?var)
  :SUBPROBLEMS ((?int1 ((|$Differentiate| SIMP) ((|$Derivative| SIMP) ?r ?var)))
               (?int2 ((|$Differentiate| SIMP) ((|$Derivative| SIMP) ?n ?var))))
  :RESULT (* (expt (log ?r) ?n) (+ (* ?int2 (log (log ?r))) (/ (* ?n ?int1) (* ?r (log ?r))))))  
  

(defIntegration sin-integral
  (Integral (sin (* ?a ?var)) ?var)
  :TEST (not (occurs-in? ?var ?a))
  :RESULT (- (/ (cos (* ?a ?var)) ?a)))

(defIntegration cos-integral
  (Integral (cos (* ?a ?var)) ?var)
  :TEST (not (occurs-in? ?var ?a))
  :RESULT (/ (sin (* ?a ?var)) ?a))

(defIntegration sin-sqr-integral
  (Integral (sqr (sin ?var)) ?var)
  :RESULT (- (/ ?var 2) (/ (sin (* 2 ?var)) 4)))

(defIntegration cos-sqr-integral
  (Integral (sqr (cos ?var)) ?var)
  :RESULT (+ (/ ?var 2) (/ (sin (* 2 ?var)) 4)))

;;;; Some not-so-clever operators

(defIntegration SinToCosSqrSub
  (Integral ?exp ?var)
  :TEST (and (occurs-in? ?var ?exp)
	     (occurs-in? `(sin ,?var) ?exp))
  :SUBPROBLEMS
  ((?Int (Integrate (Integral
		     (:EVAL (subst `(sqrt (- 1 (expt (cos ,?var) 2)))
				   `(sin ,?var)
				   ?exp :TEST 'equal)) ?var))))
  :RESULT ?Int)

(defIntegration CosToSinSqrSub
  (Integral ?exp ?var)
  :TEST (and (occurs-in? ?var ?exp)
	     (occurs-in? `(cos ,?var) ?exp))
  :SUBPROBLEMS
  ((?Int (Integrate (Integral
		     (:EVAL (subst `(sqrt (- 1 (expt (sin ,?var) 2)))
				   `(cos ,?var)
				   ?exp :TEST 'equal)) ?var))))
  :RESULT ?Int)

(defIntegration SinSqrToTanCosSub
  (Integral ?exp ?var)
  :TEST (and (occurs-in? ?var ?exp)
	     (occurs-in? `(sin ,?var) ?exp))
  :SUBPROBLEMS ((?int (Integrate (Integral
				  (:EVAL (subst `(* (sqr (tan ,?var))
						    (sqr (cos ,?var)))
						`(sin ,?var)
						?exp :TEST 'equal))
				  ?var))))
  :RESULT ?Int)
  
(defDifferentiation Differentiation-of-SQR
  (Derivative (sqr ?var) ?var)
  :RESULT (* ?var 2))  
  
  
 ;;;;;;;;;;;;;;;;;;;;
 
  
(defDifferentiation Differentiation-of-plus-Bracketed
  ((|$Derivative| SIMP) (+ ?t1) ?var)
  :SUBPROBLEMS ((?int1 ((|$Differentiate| SIMP) ((|$Derivative| SIMP) ?t1 ?var))))		
  :RESULT ?int1)    
  
(defDifferentiation Differentiation-of-plus-Bracketed
  ((|$Derivative| SIMP) ((MPLUS SIMP) ?t1) ?var)
  :SUBPROBLEMS ((?int1 ((|$Differentiate| SIMP) ((|$Derivative| SIMP) ?t1 ?var))))		
  :RESULT ?int1)    
  

(defDifferentiation Differentiation-of-times-Bracketed
  ((|$Derivative| SIMP) (* ?t1) ?var)
  :SUBPROBLEMS ((?int1 ((|$Differentiate| SIMP) ((|$Derivative| SIMP) ?t1 ?var))))		
  :RESULT ?int1)  
      
(defDifferentiation Differentiation-of-times-Bracketed
  ((|$Derivative| SIMP) ((MTIMES SIMP) ?t1) ?var)
  :SUBPROBLEMS ((?int1 ((|$Differentiate| SIMP) ((|$Derivative| SIMP) ?t1 ?var))))		
  :RESULT ?int1)    
