;;;;;;;;;;;;;;;;;
(defDifferentiation Differentiation-of-Constant
  ((|$Derivative| SIMP) ?t ?var) 
  :TEST (not (occurs-in? ?var ?t))
  :RESULT (* 0 ?var))  
;;;;;;;;;;;;;;;;;

(defDifferentiation Differentiation-of-Self
  ((|$Derivative| SIMP)  ?exp ?exp)
  :RESULT 1)  
  

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
  ((|$Derivative| SIMP) (* ?var1 ?var2) ?var)
  :SUBPROBLEMS ((?int1 ((|$Differentiate| SIMP) ((|$Derivative| SIMP) ?var1 ?var)))
		(?int2 ((|$Differentiate| SIMP) ((|$Derivative| SIMP) ?var2 ?var))))
  :RESULT (+ (* ?var2 ?int1) (* ?var1 ?int2)))  

;;;;;;;;;;;;;;;

(defDifferentiation Differentiation-of-Sum
  ((|$Derivative| SIMP) (+ ?t1 ?t2) ?var)
  :SUBPROBLEMS ((?int1 ((|$Differentiate| SIMP) ((|$Derivative| SIMP) ?t1 ?var)))
		(?int2 ((|$Differentiate| SIMP) ((|$Derivative| SIMP) ?t2 ?var))))
  :RESULT (+ ?int1 ?int2))  
  
;;;;;;;;;;;;;;;  

(defDifferentiation Differentiation-of-Nary-Product
  ((|$Derivative| SIMP) (* ?t1 ?t2 . ?trest) ?var)
  :SUBPROBLEMS ((?int1 ((|$Differentiate| SIMP) ((|$Derivative| SIMP) ?t1 ?var)))
		(?intr ((|$Differentiate| SIMP) ((|$Derivative| SIMP) (* ?t2 . ?trest) ?var))))
  :TEST (not (null ?trest))
  :RESULT (+ (* ?int1 (* ?t2 (car ?trest))) (* ?intr ?t1)))
  
;;;;;;;;;;;;;;;  

(defDifferentiation Differentiation-of-Nary-sum
  ((|$Derivative| SIMP) (+ ?t1 ?t2 . ?trest) ?var)
  :SUBPROBLEMS ((?int1 ((|$Differentiate| SIMP) ((|$Derivative| SIMP) ?t1 ?var)))
		(?int2 ((|$Differentiate| SIMP) ((|$Derivative| SIMP) ?t2 ?var)))
		(?intr ((|$Differentiate| SIMP) ((|$Derivative| SIMP) (+ . ?trest) ?var))))
  :TEST (not (null ?trest))
  :RESULT (+ ?int1 ?int2 ?intr))
  
;;;;;;;;;;;;;;;    

(defDifferentiation Differentiation-of-uminus
  ((|$Derivative| SIMP) (* -1 ?term) ?var)
  :SUBPROBLEMS ((?int ((|$Differentiate| SIMP) ((|$Derivative| SIMP) ?term ?var))))
  :RESULT (* -1 ?int))
;;;;;;;;;;;;;;    

(defDifferentiation Differentiation-of-minus
  ((|$Derivative| SIMP) (- ?t1 ?t2) ?var)
  :SUBPROBLEMS ((?int1 ((|$Differentiate| SIMP) ((|$Derivative| SIMP) ?t1 ?var)))
		(?int2 ((|$Differentiate| SIMP) ((|$Derivative| SIMP) ?t2 ?var))))
  :RESULT (- ?int1 ?int2))
;;;;;;;;;;;;;;    

(defDifferentiation Differentiation-of-log-base-power
  ((|$Derivative| SIMP) (expt (log ?v) ?n) ?var)     ;;;?n * log(?v)
  :SUBPROBLEMS ((?int1 ((|$Differentiate| SIMP)  ((|$Derivative| SIMP) (:EVAL `(log ,?v)) ?var)))
                (?int2 ((|$Differentiate| SIMP)  ((|$Derivative| SIMP) ?n ?var))))
  :RESULT (+ (* (log ?v) ?int2) (+ ?n ?int1)))

(defDifferentiation Differentiation-of-different-base-power
  ((|$Derivative| SIMP) (expt ?v ?n) ?var)     ;;;(expt ?v ?n)*?n * log(?v)
  :SUBPROBLEMS ((?int1 ((|$Differentiate| SIMP)  ((|$Derivative| SIMP) (:EVAL `(log ,?v)) ?var)))
                (?int2 ((|$Differentiate| SIMP)  ((|$Derivative| SIMP) ?n ?var))))
   :RESULT (* (expt ?v ?n) (+ (* ?int1 ?n) (* (log ?v) ?int2 ))))          

  
(defDifferentiation Differentiation-e
  ((|$Derivative| SIMP) (exp ?a) ?var)
  :SUBPROBLEMS ((?int1 ((|$Differentiate| SIMP) ((|$Derivative| SIMP) ?a ?var))))
  :RESULT (* (exp ?a) ?int1))  
   

(defDifferentiation Log-of-self-Differentiation
  ((|$Derivative| SIMP)  (log ?v) ?var)
  :SUBPROBLEMS ((?int1 ((|$Differentiate| SIMP) ((|$Derivative| SIMP) ?v ?var))))
  :RESULT (* (/ 1 ?v) ?int1 )) 
  
(defDifferentiation Differentiation-of-multiply-Bracketed
  ((|$Derivative| SIMP) (* ?t1) ?var)
  :SUBPROBLEMS ((?int1 ((|$Differentiate| SIMP) ((|$Derivative| SIMP) ?t1 ?var))))		
  :RESULT ?int1)  
  
  
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
  
;;;;;;;;;;;;;;;;;;;
;;Trignometric functions

(defDifferentiation sin-differentiation
  ((|$Derivative| SIMP) (sin ?a) ?var)
  :SUBPROBLEMS ((?int1 ((|$Differentiate| SIMP) ((|$Derivative| SIMP) ?a ?var))))		
  :RESULT (* (cos ?a) ?int1)) 
  
(defDifferentiation csc-differentiation
  ((|$Derivative| SIMP) (csc ?a) ?var)
  :SUBPROBLEMS ((?int1 ((|$Differentiate| SIMP) ((|$Derivative| SIMP) ?a ?var))))		
  :RESULT (* -1 (csc ?a) (cot ?a) ?int1)) 
  
(defDifferentiation asin-differentiation
  ((|$Derivative| SIMP) (asin ?a) ?var)
  :SUBPROBLEMS ((?int1 ((|$Differentiate| SIMP) ((|$Derivative| SIMP) ?a ?var))))		
  :RESULT (* (/ 1 (expt (+ 1 (* -1 (expt ?a 2))) (/ 1 2))) ?int1))   
  
(defDifferentiation cos-differentiation
  ((|$Derivative| SIMP) (cos ?a) ?var)
  :SUBPROBLEMS ((?int1 ((|$Differentiate| SIMP) ((|$Derivative| SIMP) ?a ?var))))		
  :RESULT (* -1 (sin ?a) ?int1))   
  
(defDifferentiation sec-differentiation
  ((|$Derivative| SIMP) (sec ?a) ?var)
  :SUBPROBLEMS ((?int1 ((|$Differentiate| SIMP) ((|$Derivative| SIMP) ?a ?var))))		
  :RESULT (* (sec ?a) (tan ?a) ?int1)) 
  
(defDifferentiation acos-differentiation
  ((|$Derivative| SIMP) (acos ?a) ?var)
  :SUBPROBLEMS ((?int1 ((|$Differentiate| SIMP) ((|$Derivative| SIMP) ?a ?var))))		
  :RESULT (* -1 (/ 1 (expt (+ 1 (* -1 (expt ?a 2))) (/ 1 2))) ?int1))     
  
(defDifferentiation tan-differentiation
  ((|$Derivative| SIMP) (tan ?a) ?var)
  :SUBPROBLEMS ((?int1 ((|$Differentiate| SIMP) ((|$Derivative| SIMP) ?a ?var))))		
  :RESULT (* (expt (sec ?a) 2) ?int1))     
  
(defDifferentiation cot-differentiation
  ((|$Derivative| SIMP) (cot ?a) ?var)
  :SUBPROBLEMS ((?int1 ((|$Differentiate| SIMP) ((|$Derivative| SIMP) ?a ?var))))		
  :RESULT (* -1 (expt (csc ?a) 2) ?int1))
  
(defDifferentiation atan-differentiation
  ((|$Derivative| SIMP) (atan ?a) ?var)
  :SUBPROBLEMS ((?int1 ((|$Differentiate| SIMP) ((|$Derivative| SIMP) ?a ?var))))		
  :RESULT (* (/ 1 (+ 1 (expt ?a 2))) ?int1))       
  
(defDifferentiation acot-differentiation
  ((|$Derivative| SIMP) (acot ?a) ?var)
  :SUBPROBLEMS ((?int1 ((|$Differentiate| SIMP) ((|$Derivative| SIMP) ?a ?var))))		
  :RESULT (* -1 (/ 1 (+ 1 (expt ?a 2))) ?int1))    
  
(defDifferentiation acsc-differentiation
  ((|$Derivative| SIMP) (acsc ?a) ?var)
  :SUBPROBLEMS ((?int1 ((|$Differentiate| SIMP) ((|$Derivative| SIMP) ?a ?var))))		
  :RESULT (* -1 (/ 1 (* (expt ?a 2) (expt (+ 1 (/ 1 (* -1 (expt ?a 2)))) (/ 1 2)))) ?int1))   
  
(defDifferentiation asec-differentiation
  ((|$Derivative| SIMP) (asec ?a) ?var)
  :SUBPROBLEMS ((?int1 ((|$Differentiate| SIMP) ((|$Derivative| SIMP) ?a ?var))))		
  :RESULT (* (/ 1 (* (expt ?a 2) (expt (+ 1 (/ 1 (* -1 (expt ?a 2)))) (/ 1 2)))) ?int1))   
  
(defDifferentiation sinh-differentiation
  ((|$Derivative| SIMP) (sinh ?a) ?var)
  :SUBPROBLEMS ((?int1 ((|$Differentiate| SIMP) ((|$Derivative| SIMP) ?a ?var))))		
  :RESULT (* (cosh ?a) ?int1))    
  
(defDifferentiation cosh-differentiation
  ((|$Derivative| SIMP) (cosh ?a) ?var)
  :SUBPROBLEMS ((?int1 ((|$Differentiate| SIMP) ((|$Derivative| SIMP) ?a ?var))))		
  :RESULT (* (sinh ?a) ?int1))    
  
(defDifferentiation tanh-differentiation
  ((|$Derivative| SIMP) (tanh ?a) ?var)
  :SUBPROBLEMS ((?int1 ((|$Differentiate| SIMP) ((|$Derivative| SIMP) ?a ?var))))		
  :RESULT (* (expt (sech ?a) 2) ?int1))  
  
(defDifferentiation sech-differentiation
  ((|$Derivative| SIMP) (sech ?a) ?var)
  :SUBPROBLEMS ((?int1 ((|$Differentiate| SIMP) ((|$Derivative| SIMP) ?a ?var))))		
  :RESULT (* -1 (sech ?a) (tanh ?a) ?int1)) 
  
(defDifferentiation coth-differentiation
  ((|$Derivative| SIMP) (coth ?a) ?var)
  :SUBPROBLEMS ((?int1 ((|$Differentiate| SIMP) ((|$Derivative| SIMP) ?a ?var))))		
  :RESULT (* -1 (expt (csch ?a) 2) ?int1))  
  
(defDifferentiation csch-differentiation
  ((|$Derivative| SIMP) (csch ?a) ?var)
  :SUBPROBLEMS ((?int1 ((|$Differentiate| SIMP) ((|$Derivative| SIMP) ?a ?var))))		
  :RESULT (* -1 (csch ?a) (coth ?a) ?int1))     
