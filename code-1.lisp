#|Copyright (c) 1986-1993 Kenneth D. Forbus, Johan de Kleer and Xerox
Corporation.  All Rights Reserved.

Use, reproduction, and preparation of derivative works are permitted.
Any copy of this software or of any derivative work must include the
above copyright notice and this paragraph.  Any distribution of this
software or derivative works must comply with all applicable United
States export control laws.  This software is made available as is, and
Kenneth D. Forbus, Johan de Kleer and Xerox Corporation disclaim all
warranties, express or implied, including without limitation the implied
warranties of merchantability and fitness for a particular purpose, and
notwithstanding any other provision contained herein, any liability for
damages resulting from the software or its use is expressly disclaimed,
whether arising in contract, tort (including negligence) or strict
liability, even if Kenneth D. Forbus, Johan de Kleer or Xerox
Corporation is advised of the possibility of such damages.
|#

;;; Justification-based Truth Maintenence System (JTMS)

;(in-package :COMMON-LISP-USER)

(defstruct (jtms (:PRINT-FUNCTION print-jtms))
  (title nil)					
  (node-counter 0)             ;; unique namer for nodes.
  (just-counter 0)             ;; unique namer for justifications.
  (nodes nil)                  ;; list of all tms nodes.
  (justs nil)                  ;; list of all justifications
  (debugging nil)              ;; debugging flag
  (contradictions nil)         ;; list of contradiction nodes.
  (assumptions nil)            ;; list of assumption nodes.
  (checking-contradictions T)  ;; For external systems
  (node-string nil)
  (contradiction-handler nil)
  (enqueue-procedure nil))

(defun print-jtms (jtms stream ignore)
  (declare (ignore ignore))
  (format stream "#<JTMS: ~A>" (jtms-title jtms)))

(defstruct (tms-node (:PRINT-FUNCTION print-tms-node))
  (index 0)
  (datum nil)           ;; pointer to external problem solver
  (label :OUT)          ;; :IN means believed, :OUT means disbelieved
  (support nil)         ;; Current justification or premise marker
  (justs nil)           ;; Possible justifications
  (consequences nil)    ;; Justifications in which it is an antecedent
  (mark nil)            ;; Marker for sweep algorithms
  (contradictory? nil)  ;; Flag marking it as contradictory
  (assumption? nil)     ;; Flag marking it as an assumption.
  (in-rules nil)	;; Rules that should be triggered when node goes in
  (out-rules nil)	;; Rules that should be triggered when node goes out
  (jtms nil))           ;; The JTMS in which this node appears.

(defun print-tms-node (node stream ignore)
  (declare (ignore ignore))
  (format stream "#<Node: ~A>" (node-string node)))

(defstruct (just (:PRINT-FUNCTION print-just))
  (index 0)
  informant
  consequence
  antecedents)

(defun print-just (just stream ignore)
  (declare (ignore ignore))
  (format stream "#<Just ~D>" (just-index just)))

(defun tms-node-premise? (node &aux support)
  (and (setq support (tms-node-support node))
       (not (eq support :ENABLED-ASSUMPTION))
       (null (just-antecedents support))))

;;; Simple utilities:

(defun node-string (node)
  (funcall (jtms-node-string (tms-node-jtms node)) node))

(defmacro debugging-jtms (jtms msg &optional node &rest args)
  `(when (jtms-debugging ,jtms)
     (format *trace-output* ,msg (if ,node (node-string ,node)) ,@args)))

(defun tms-error (string node) (error string (node-string node)))

(defun default-node-string (n) (format nil "~A" (tms-node-datum n)))

(defun create-jtms (title &key (node-string 'default-node-string)
                               debugging
                               (checking-contradictions t)
                               (contradiction-handler 'ask-user-handler)
                               enqueue-procedure)
  (make-jtms :TITLE title
	     :NODE-STRING node-string
	     :DEBUGGING debugging
	     :CHECKING-CONTRADICTIONS checking-contradictions
	     :CONTRADICTION-HANDLER contradiction-handler
	     :ENQUEUE-PROCEDURE enqueue-procedure
	     ))
	     
(defun change-jtms (jtms &key contradiction-handler node-string
		              enqueue-procedure debugging
                              checking-contradictions)
  (if node-string (setf (jtms-node-string jtms) node-string))
  (if debugging (setf (jtms-debugging jtms) debugging))
  (if checking-contradictions
      (setf (jtms-checking-contradictions jtms)
	    checking-contradictions))
  (if contradiction-handler
      (setf (jtms-contradiction-handler jtms) contradiction-handler))
  (if enqueue-procedure
      (setf (jtms-enqueue-procedure jtms) enqueue-procedure)))

;;; Basic inference-engine interface.

(defun in-node? (node) (eq (tms-node-label node) :IN))

(defun out-node? (node) (eq (tms-node-label node) :OUT))

(defun tms-create-node (jtms datum &key assumptionp contradictoryp)
  (let ((node (make-tms-node :INDEX (incf (jtms-node-counter jtms))
			     :DATUM datum
			     :ASSUMPTION? assumptionp
			     :CONTRADICTORY? contradictoryp
			     :JTMS jtms)))
    (if assumptionp (push node (jtms-assumptions jtms)))
    (if contradictoryp (push node (jtms-contradictions jtms)))
    (push node (jtms-nodes jtms))
    node))

;;; Converts a regular node to an assumption and enables it.
(defun assume-node (node &aux (jtms (tms-node-jtms node)))
  (unless (tms-node-assumption? node)
    (debugging-jtms jtms "~%Converting ~A into an assumption" node)
    (setf (tms-node-assumption? node) t))
  (enable-assumption node))

(defun make-contradiction (node &aux (jtms (tms-node-jtms node)))
  (unless (tms-node-contradictory? node)
    (setf (tms-node-contradictory? node) t)
    (push node (jtms-contradictions jtms))
    (check-for-contradictions jtms)))

(defun justify-node (informant consequence antecedents &aux just jtms)
  (setq jtms (tms-node-jtms consequence)
	just (make-just :INDEX (incf (jtms-just-counter jtms))
			:INFORMANT informant
			:CONSEQUENCE consequence
			:ANTECEDENTS antecedents))
  (push just (tms-node-justs consequence))
  (dolist (node antecedents) (push just (tms-node-consequences node)))
  (push just (jtms-justs jtms))
  (debugging-jtms jtms
		  "~%Justifying ~A by ~A using ~A."
		  consequence
		  informant
		  (mapcar #'node-string antecedents))
  (if (or antecedents (out-node? consequence))
      (if (check-justification just) (install-support consequence just))
      (setf (tms-node-support consequence) just))
  (check-for-contradictions jtms))

;;;; Support for adding justifications

(defun check-justification (just)
  (and (out-node? (just-consequence just))
       (justification-satisfied? just)))

(defun justification-satisfied? (just) 
  (every #'in-node? (just-antecedents just)))

(defun install-support (conseq just)
  (make-node-in conseq just)
  (propagate-inness conseq))

(defun propagate-inness (node &aux (jtms (tms-node-jtms node)) (q (list node)))
  (do () ((null (setq node (pop q))))
    (debugging-jtms jtms "~%   Propagating belief in ~A." node)
    (dolist (justification (tms-node-consequences node))
      (when (check-justification justification)
	(make-node-in (just-consequence justification) justification)
	(push (just-consequence justification) q)))))

(defun make-node-in (conseq reason &aux jtms enqueuef)
  (setq jtms (tms-node-jtms conseq)
	enqueuef (jtms-enqueue-procedure jtms))
  (debugging-jtms jtms "~%     Making ~A in via ~A."
	     conseq
	     (if (symbolp reason)
		 reason
		 (cons (just-informant reason)
		       (mapcar (jtms-node-string jtms)
			       (just-antecedents reason)))))
  (setf (tms-node-label conseq) :IN)
  (setf (tms-node-support conseq) reason)
  (when enqueuef
    (dolist (in-rule (tms-node-in-rules conseq))
      (funcall enqueuef in-rule))
    (setf (tms-node-in-rules conseq) nil)))

;;; Assumption Manipulation
(defun retract-assumption (node &aux jtms)
  (when (eq (tms-node-support node) :ENABLED-ASSUMPTION)
    (setq jtms (tms-node-jtms node))
    (debugging-jtms jtms "~%  Retracting assumption ~A." node)
    (make-node-out node)
    (find-alternative-support jtms (cons node (propagate-outness node jtms)))))

(defun enable-assumption (node &aux (jtms (tms-node-jtms node)))
  (unless (tms-node-assumption? node) 
    (tms-error "Can't enable the non-assumption ~A" node))
  (debugging-jtms jtms "~%  Enabling assumption ~A." node)
  (cond ((out-node? node) (make-node-in node :ENABLED-ASSUMPTION)
	                  (propagate-inness node))
	((or (eq (tms-node-support node) :ENABLED-ASSUMPTION)
	     (null (just-antecedents (tms-node-support node)))))
	(t (setf (tms-node-support node) :ENABLED-ASSUMPTION)))
  (check-for-contradictions jtms))

(defun make-node-out (node &aux jtms enqueuef)
  (setq jtms (tms-node-jtms node)
	enqueuef (jtms-enqueue-procedure jtms))
  (debugging-jtms jtms "~%     retracting belief in ~a." node)
  (setf (tms-node-support node) nil)
  (setf (tms-node-label node) :OUT)
  (if enqueuef (dolist (out-rule (tms-node-out-rules node)) 
		 (funcall enqueuef out-rule)))
  (setf (tms-node-out-rules node) nil))

(defun propagate-outness (node jtms &aux out-queue)
  (debugging-jtms jtms "~%   Propagating disbelief in ~A." node)
  (do ((js (tms-node-consequences node) (append (cdr js) new))
       (new nil nil)
       (conseq nil))
      ((null js) out-queue)
    ;;For each justification using the node, check to see if
    ;;it supports some other node.  If so, forget that node,
    ;;queue up the node to look for other support, and recurse
    (setq conseq (just-consequence (car js)))
    (when (eq (tms-node-support conseq) (car js)) 
      (make-node-out conseq)
      (push conseq out-queue)
      (setq new (tms-node-consequences conseq)))))

(defun find-alternative-support (jtms out-queue)
  (debugging-jtms jtms "~%   Looking for alternative supports.")
  (dolist (node out-queue)
    (unless (in-node? node)
      (dolist (just (tms-node-justs node))
	(when (check-justification just)
	  (install-support (just-consequence just)
				 just)
	  (return just))))))

;;; Contradiction handling interface
(defun check-for-contradictions (jtms &aux contradictions)
  (when (jtms-checking-contradictions jtms)
    (dolist (cnode (jtms-contradictions jtms))
      (if (in-node? cnode) (push cnode contradictions)))
    (if contradictions
	(funcall (jtms-contradiction-handler jtms) jtms contradictions))))

(defmacro without-contradiction-check (jtms &body body)
  (contradiction-check jtms nil body))

(defmacro with-contradiction-check (jtms &body body)
  (contradiction-check jtms t body))

(defun contradiction-check (jtms flag body)
  (let ((jtmsv (gensym)) (old-value (gensym)))
    `(let* ((,jtmsv ,jtms)
	    (,old-value (jtms-checking-contradictions ,jtmsv)))
       (unwind-protect
	   (progn (setf (jtms-checking-contradictions ,jtmsv) ,flag) ,@body)
	 (setf (jtms-checking-contradictions ,jtmsv) ,old-value)))))

(defmacro with-contradiction-handler (jtms handler &body body)
  (let ((jtmsv (gensym)) (old-handler (gensym)))
    `(let* ((,jtmsv ,jtms)
	    (,old-handler (jtms-contradiction-handler ,jtmsv)))
     (unwind-protect
	 (progn (setf (jtms-contradiction-handler ,jtmsv) ,handler) ,@body)
       (setf (jtms-contradiction-handler ,jtmsv) ,old-handler)))))

(defun default-assumptions (jtms)
  (with-contradiction-check jtms
    (with-contradiction-handler jtms #'(lambda (&rest ignore)
					 (declare (ignore ignore)) 
					 (throw 'CONTRADICTION t))
      (dolist (assumption (jtms-assumptions jtms))
	(cond ((eq (tms-node-support assumption) :ENABLED-ASSUMPTION))
	      ((not (eq :DEFAULT (tms-node-assumption? assumption))))
	      ((catch 'CONTRADICTION (enable-assumption assumption))
	       (retract-assumption assumption)))))))

;;; Well-founded support inqueries
(defun supporting-justification-for-node (node) (tms-node-support node))

(defun assumptions-of-node (node &aux assumptions (marker (list :MARK)))
  (do ((nodes (list node) (append (cdr nodes) new))
       (new nil nil))
      ((null nodes) assumptions)
    (let ((node (car nodes)))
      (cond ((eq (tms-node-mark node) marker))
	    ((eq (tms-node-support node) :ENABLED-ASSUMPTION)
	     (push node assumptions))
	    ((in-node? node)
	     (setq new (just-antecedents (tms-node-support node)))))
      (setf (tms-node-mark node) marker))))

(defun enabled-assumptions (jtms &aux result)
  (dolist (assumption (jtms-assumptions jtms) result)
    (if (eq (tms-node-support assumption) :ENABLED-ASSUMPTION)
	(push assumption result))))

;;; Inference engine stub to allow this JTMS to be used stand alone
(defun why-node (node &aux justification) 
  (setq justification (tms-node-support node))
  (cond ((eq justification :ENABLED-ASSUMPTION)
	 (format t "~%~A is an enabled assumption"
		 (node-string node)))
	(justification (mapcar #'displa (cons (replac-to-maxima (cadr (node-string node))) 
				(replac-to-maxima (cddr (node-string node))))) (princ '=>) 
	 (dolist (anode (just-antecedents justification))
	    (progn (terpri)(mapcar #'displa (cons (replac-to-maxima (cadr (node-string anode))) 
				(replac-to-maxima (cddr (node-string anode))))))
				))
	(T (format t "~%~A is OUT." (node-string node))))
  node)

(defun why-nodes (jtms)
  (dolist (node (jtms-nodes jtms)) (why-node node)))

(proclaim '(special *contra-assumptions*))

(defun ask-user-handler (jtms contradictions)
  (handle-one-contradiction (car contradictions))
  (check-for-contradictions jtms))

(defun handle-one-contradiction (contra-node
				 &aux the-answer *contra-assumptions*)
  (setq *contra-assumptions* (assumptions-of-node contra-node))
  (unless *contra-assumptions*
    (tms-error "~%There is a flaw in the universe...~A" contra-node))
  (format t "~%Contradiction found: ~A" (node-string contra-node))
  (print-contra-list *contra-assumptions*)
  (format t "~%Call (TMS-ANSWER <number>) to retract assumption.")
  (setq the-answer
	(catch 'tms-contradiction-handler
	  (break "JTMS contradiction break")))
  (if (and (integerp the-answer)
	   (> the-answer 0)
	   (not (> the-answer (length *contra-assumptions*))))
      (retract-assumption (nth (1- the-answer)
			       *contra-assumptions*))))

(defun print-contra-list (nodes)
  (do ((counter 1 (1+ counter))
       (nn nodes (cdr nn)))
      ((null nn))
    (format t "~%~A ~A" counter
	    (node-string (car nn)))))

(defun tms-answer (num)
  (if (integerp num)
      (if (> num 0)
	  (if (not (> num (length *contra-assumptions*)))
	      (throw 'tms-contradiction-handler num)
	      (format t "~%Ignoring answer, too big."))
	  (format t "~%Ignoring answer, too small"))
      (format t "~%Ignoring answer, must be an integer.")))


(defun explore-network (node) 
  (unless (in-node? node)
	  (format t "~% Sorry, ~A not believed." (node-string node))
	  (return-from explore-network node))
  (do ((stack nil)
       (current node)
       (options nil)
       (olen 0)
       (done? nil))
      (done? current)
      (why-node current)
      (setq options (if (typep (tms-node-support current) 'just)
			(just-antecedents (tms-node-support current))))
      (setq olen (length options))
      (do ((good? nil)
	   (choice 0))
	  (good? (case good?
		       (q (return-from explore-network current))
		       (0 (if stack
			      (setq current (pop stack))
			      (return-from explore-network current)))
		       (t (push current stack)
			  (setq current (nth (1- good?) options)))))
	  ;(format t "~%>>>")
          (setq choice 0)      ;(setq choice (read))
	  (cond ((or (eq choice 'q)
		     (and (integerp choice)
			  (not (> choice olen))
			  (not (< choice 0))))
		 (setq good? choice))
		(t (format t
		    "~% Must be q or an integer from 0 to ~D."
		    olen))))))
    		    
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; JTRE definitions  : jinter.lisp

;(in-package :COMMON-LISP-USER) 

(defstruct (jtre (:PRINT-FUNCTION jtre-printer))
  title                   ; Pretty name
  jtms                    ; Pointer to its JTMS
  (dbclass-table nil)       ; Table of dbclasses
  (datum-counter 0)       ; Unique ID for asserts
  (rule-counter 0)        ; Unique ID for rules
  (debugging nil)         ; If non-NIL, show basic operations
  (queue nil)             ; Rule queue
  (rules-run 0))          ; Statistic

(defun jtre-printer (j st ignore)
  (format st "<JTRE: ~A>" (jtre-title j)))

(defvar *JTRE* nil)

(defmacro With-Jtre (jtre &rest forms); (print `(with-jtre ,jtre forms))
  `(let ((*JTRE* ,jtre)) ,@ forms))

(defun In-Jtre (jtre) (setq *JTRE* jtre))

(defmacro debugging-jtre (msg &rest args)
  `(when (jtre-debugging *JTRE*) (format t ,msg  ,@args)))

(defun create-jtre (title &key debugging)
 (let ((j (make-jtre
	   :TITLE title 
	   :JTMS (create-jtms (list :JTMS-OF title) 
			      :NODE-STRING 'view-node)
	   :DBCLASS-TABLE (make-hash-table :TEST #'eq)
	   :DEBUGGING debugging)))
   (change-jtms (jtre-jtms j)
		:ENQUEUE-PROCEDURE
		#'(lambda (rule) (enqueue rule j)))
   j))

(defun change-jtre (jtre &key (debugging :NADA))
  (unless (eq debugging :NADA)
	  (setf (jtre-debugging jtre) debugging)))

;;;; Running JTRE

(defun uassert! (fact &optional (just 'user))
  (assert! fact just) ;; Do internal operation
  (run-rules *JTRE*))        ;; Run the rules

(defun uassume! (fact reason) ;; Similar to UASSERT!
  (assume! fact reason *JTRE*)
  (run-rules *JTRE*))

(defun run-forms (forms &optional (*JTRE* *JTRE*))
  (dolist (form forms) (eval form) (run-rules *JTRE*)))

(defun run (&optional (*JTRE* *JTRE*)) ;; Toplevel driver function
    (format T "~%>>")
    (do ((form (read) (read)))
        ((member form '(quit stop exit abort)) nil)
        (format t "~%~A" (eval form))
        (run-rules)
        (format t "~%>>")))

(defun show (&optional (*JTRE* *JTRE*) (stream *standard-output*))
  (show-data *JTRE* stream) (show-rules *JTRE* stream))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;jdata.lisp

;; -*- Mode: Lisp; -*-

;;;; Database for Tiny Rule Engine using JTMS

;(in-package :COMMON-LISP-USER)

;;;; Database structure and contents

(defstruct (dbclass (:PRINT-FUNCTION jtre-dbclass-printer))
  name    ; Corresponding symbol
  jtre    ; JTRE it is part of.
  facts   ; Associated facts
  rules)  ; Associated rules

 (defun jtre-dbclass-printer (r st ignore)
   (declare (ignore ignore))
   (format st "<Dbclass ~A>" (dbclass-name r)))

(defstruct (datum (:PRINT-FUNCTION jtre-datum-printer))
  id                   ; Unique ID for easy lookup
  lisp-form            ; Expression for pattern-matching
  (tms-node nil)       ; Pointer into TMS
  dbclass              ; Dbclass of the corresponding pattern
  (assumption? nil)    ; if non-nil, indicates informant
  (plist nil))         ; local property list

(defun jtre-datum-printer (d st ignore)
  (declare (ignore ignore))
  (format st "<Datum ~D>" (datum-id d)))

;;;; Making statements

(defun assert! (fact just &optional (*JTRE* *JTRE*) &aux datum node)
  (setq datum (referent fact t)
	node (datum-tms-node datum))
  (unless (listp just) (setq just (list just)))
  (debugging-jtre "~%    Asserting ~A via ~A." fact just)
  (justify-node (car just) node
		(mapcar #'(lambda (f) (datum-tms-node (referent f t)))
			(cdr just)))
  datum)

(defmacro rassert! (fact just)
  `(assert! ,(quotize fact) ,(quotize just)))

(defun quiet-assert! (fact just &optional (*JTRE* *JTRE*))
  (without-contradiction-check (jtre-jtms *JTRE*) (assert! fact just)))

(defun assume! (fact reason &optional (*JTRE* *JTRE*) &aux datum node)
  (setq datum (referent fact t)
	node (datum-tms-node datum))
  (cond	((not (datum-assumption? datum))
	 (setf (datum-assumption? datum) reason)
	 (debugging-jtre "~%    Assuming ~A via ~A." fact reason)
	 (assume-node node)
	 (enable-assumption node))
	((eq reason (datum-assumption? datum)))
	(t (error
	    "Fact ~A assumed because of ~A assumed again because of ~A"
	    (show-datum datum) (datum-assumption? datum) reason)))
  datum)

(defun already-assumed? (fact) (datum-assumption? (referent fact t)))

;;;; Retraction

(defun retract! (fact &optional (just 'user) (quiet? nil)
		      (*JTRE* *JTRE*) &aux datum node)
  (setq datum (referent fact t)
	node (datum-tms-node datum))
  (cond ((not (tms-node-assumption? node))
	 (unless quiet?
	   (format t "~%~A isn't an assumption."
		   (show-datum datum))))
	((not (in-node? node))
	 (unless quiet?
	   (format T
	     "~%The assumption ~A is not currently in."
	     fact)))
	((eq just (datum-assumption? datum))
	 (debugging-jtre "~%    Retracting ~A via ~A."
			 fact just)
	 (setf (datum-assumption? datum) nil)
	 (retract-assumption node))
	((not quiet?)
	 (format t "~%~A not source of assumption for ~A"
		 just fact)))
  node)

(defmacro rretract! (fact &optional (just 'USER))
  `(retract! ,(quotize fact) ,(quotize just)))

(defun contradiction (fact &optional (*JTRE* *JTRE*))
  (make-contradiction (datum-tms-node (referent fact t))))

;;;; Interface and display of data

(defun in? (fact &optional (*JTRE* *JTRE*) &aux r)
  (when (setq r (referent fact))
	(in-node? (datum-tms-node r))))

(defun out? (fact &optional (*JTRE* *JTRE*) &aux r)
  (when (setq r (referent fact))
	(out-node? (datum-tms-node r))))
    
(defun why? (fact &optional (*JTRE* *JTRE*) &aux r)
  (when (setq r (referent fact))
	(why-node (datum-tms-node r))))

(defun assumptions-of (fact &optional (*JTRE* *JTRE*))
  (mapcar #'view-node 
	  (assumptions-of-node
	   (datum-tms-node (referent fact *jtre* t)))))

(defun fetch (pattern &optional (*JTRE* *JTRE*) &aux bindings unifiers)
  (dolist (candidate (get-candidates pattern) unifiers)
    (setq bindings (unify pattern (datum-lisp-form candidate)))
    (unless (eq bindings :FAIL)
      (push (sublis bindings pattern) unifiers))))

;;;; More display-intensive procedures

(defun wfs (fact &optional (*JTRE* *JTRE*))
  ;; Displays well-founded support for a fact
  (cond ((out? fact) (format t "~% ~A is OUT." fact))
	(t (do ((queue (list (get-tms-node fact))
		       (nconc (cdr queue) new-antes))
		(so-far (list (get-tms-node fact)))
		(new-antes nil nil))
	       ((null queue) (format t "~%--------") fact)
	     (why-node (car queue))
	     (unless (or (out-node? (car queue))
			 (tms-node-assumption? (car queue)))
	       ;; Go down the support
	       (dolist (ante (just-antecedents
			      (tms-node-support (car queue))))
		 (unless (member ante so-far)
		   (push ante so-far)
		   (push ante new-antes))))))))

(defun say-datum-belief (pr &optional (*jtre* *jtre*)
			    (indent ""))
  (format t "~%~A~A: ~A" indent pr
	  (if (in-node? (get-tms-node pr *jtre*))
	      "IN" "OUT")))

(defun show-justifications (fact &optional (*jtre* *jtre*))
  (format t "~% ~A::" fact)
  (let* ((node (get-tms-node fact *jtre*))
	 (justs (tms-node-justs node)))
    (unless justs
	    (format t " No justifications.")
	    (return-from show-justifications node))
    (dolist (j justs)
	    (format t "~% ~A" (just-informant j))
	    (cond ((just-antecedents j) 
		   (format t ", on:")
		   (dolist (ante (just-antecedents j))
			   (say-datum-belief
			    (view-node ante) *jtre* "  "))
		   (format t "."))
		  (t (format t "."))))))

(defun show-data (&optional (*JTRE* *JTRE*)
			    (stream *standard-output*))
  (format stream 
	  "~%~D facts total." (jtre-datum-counter *JTRE*))
  (map-dbclass
   #'(lambda (dbclass)
       (dolist (datum (dbclass-facts dbclass))
	       (format stream "~%~A: ~A" (show-datum datum)
		       (if (in-node? (datum-tms-node datum))
			   "IN" "OUT"))))))

;;;; Database system

(defun get-dbclass (fact &optional (*JTRE* *JTRE*)
			 &aux dbclass)
  (cond ((null fact) (error "~% NIL can't be a dbclass."))
	((listp fact) (get-dbclass (car fact) *JTRE*))
	((variable? fact)
	 (cond ((boundp fact)
		(get-dbclass (symbol-value fact) *JTRE*))
	       (t (error "~%Dbclass unbound: ~A" fact))))
	((symbolp fact)
	 (cond ((setq dbclass
		      (gethash fact
			       (jtre-dbclass-table *JTRE*)))
		dbclass)
	       (t (setq dbclass
			(make-dbclass :NAME fact :FACTS nil
				    :RULES nil :JTRE *JTRE*))
		  (setf (gethash fact
			 (jtre-dbclass-table *JTRE*))
			dbclass)
		  dbclass)))
	(t (error "Bad dbclass type: ~A" fact))))

(defun referent (fact &optional (virtual? nil)
		      (*JTRE* *JTRE*))
  (if virtual? (insert fact) (referent1 fact)))

(defun referent1 (fact)
  (dolist (candidate (dbclass-facts (get-dbclass fact)))
	  (when (equal (datum-lisp-form candidate) fact)
		(return candidate))))

(defun insert (fact &aux datum)
  (setq datum (referent1 fact))
  (cond (datum (values datum t))
	(t (setq datum
		 (make-datum
		  :ID (incf (jtre-datum-counter *JTRE*))
		  :LISP-FORM fact
		  :DBCLASS (get-dbclass fact)))
	   (setf (datum-tms-node datum)
		 (tms-create-node (jtre-jtms *JTRE*) datum))
	   (push datum (dbclass-facts (datum-dbclass datum)))
	   (try-rules datum)
	   (values datum nil))))

(defun get-candidates (pattern)
  (dbclass-facts (get-dbclass pattern)))

(defun map-dbclass (proc &optional (*JTRE* *JTRE*))
  (maphash #'(lambda (name dbclass) (declare (ignore name))
	       (funcall proc dbclass))
	   (jtre-dbclass-table *JTRE*)))

(defun get-tms-node (fact &optional (*JTRE* *JTRE*))
  (datum-tms-node (referent fact t)))

(defun view-node (node)
  (datum-lisp-form (tms-node-datum node)))

;;;; More query routines

(defun show-datum (datum)
  (format nil "~A" (datum-lisp-form datum)))

(defun get-datum (num &optional (*JTRE* *JTRE*))
  (map-dbclass
   #'(lambda (dbclass)
       (dolist (datum (dbclass-facts dbclass))
	       (when (= (datum-id datum) num)
		     (return-from GET-DATUM datum))))))

(defun get-just (num &optional (*JTRE* *JTRE*))
  (dolist (just (jtms-justs (jtre-jtms *JTRE*)))
    (when (= (just-index just) num)
	  (return-from GET-just just))))
	  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; This file is jrules.lisp

;(in-package :COMMON-LISP-USER)

(proclaim '(special *JTRE* *bound-vars* *rule-procedures*))

(defstruct (rule (:PRINT-FUNCTION jtre-rule-printer))
  id           ; Unique ID for easy lookup
  jtre         ; The JTRE it is part of
  dbclass      ; Dbclass of associated pattern
  matcher      ; Procedure that performs the match.
  body)        ; Procedure that does the work.

(defun jtre-rule-printer (r st ignore)
  (declare (ignore ignore))
  (format st "<Rule ~D>" (rule-id r)))

(defvar *file-counter* 0)
(defvar *file-prefix* "")

(defmacro Rule-File (prefix)
  `(eval-when (compile load eval)
     (setq *file-counter* 0)
     (setq *file-prefix* ,prefix)))

;;;; Building and installing rules

(defmacro rule (triggers &rest body) (do-rule triggers body))

(defun my-rule(triggers &rest body)(do-rule triggers body))

(defun do-rule (triggers body)
  (let ((*rule-procedures* nil)
	(*bound-vars* nil)
	(index-form nil))
    (setq index-form
	  (build-rule (car triggers)
		      (subst 'internal-rule
			     'rule
			     (make-nested-rule
			      (cdr triggers) body))))
  `(progn ,@ *rule-procedures* ,index-form)))

(defmacro internal-rule (triggers &rest body)
  `(add-internal-rule ,(car triggers)
     ,(make-nested-rule (cdr triggers) body)))

(defun make-nested-rule (triggers body)
  (cond ((null triggers) body)
	(t `((add-internal-rule ,(car triggers)
	       ,(make-nested-rule (cdr triggers) body))))))

(defmacro add-internal-rule (trigger body)
  (build-rule trigger body))

;;;; Details of rule-building

(defun build-rule (trigger body &aux match-procedure body-procedure)
  (multiple-value-bind (pattern condition var test)
		       (parse-rule-trigger trigger)
   (setq match-procedure
	 (generate-match-procedure pattern var test condition))   
   (setq body-procedure
	 (generate-body-procedure pattern condition var body))
   (push match-procedure *rule-procedures*)
   (push body-procedure *rule-procedures*)
   `(insert-rule
     (get-dbclass ,(get-trigger-dbclass pattern))
     ;return form to index rule
     (function ;the match function for rule
       ,(if *bound-vars*
	    `(lambda (p)
	       (,(cadr match-procedure) p ,@ *bound-vars*))
	  (cadr match-procedure)))
     (function ;;the body function for rule
       ,(if (or *bound-vars*
		(not (eq condition :INTERN)))
	    (let ((tv (nreverse
			(pattern-free-variables trigger))))
	      (unless (eq condition :INTERN)
		      (push 'TRIGGER-NODE tv))
	      `(lambda ,tv
		 (,(cadr body-procedure) ,@ tv
		  ;(fn-name parameters)
		  ,@ (scratchout tv *bound-vars*))))
	      (cadr body-procedure))))))

(defun parse-rule-trigger (trigger)
  (values (cadr trigger)
	  (cond ((member (car trigger) '(:INTERN :IN :OUT))
		 (car trigger))
		(t (error
		    "~% Unknown belief condition ~A in trigger ~A."
			  (car trigger) trigger)))
	  (cadr (member :VAR (cddr trigger)))
	  (cadr (member :TEST (cddr trigger)))))

(defun get-trigger-dbclass (trigger)
  (cond ((variable? trigger)
	 (if (member trigger *bound-vars*)  trigger
	     (error "~%Trigger dbclass is unbound -- ~A."
		    trigger)))
	((atom trigger)  (list 'QUOTE trigger))
	(t (get-trigger-dbclass (car trigger)))))

;;;; Generating the body function

(defmacro with-pushed-variable-bindings (new-bindings
					  &rest body)
  `(let ((*bound-vars* (append ,new-bindings
			       (scratchout ,new-bindings
					   *bound-vars*))))
     ,@ body))

(defun generate-body-procedure (pattern condition var body
					&aux newly-bound env fname)
  (setq newly-bound (pattern-free-variables pattern))
  (if var (push var newly-bound))
  (setq body (with-pushed-variable-bindings
	       newly-bound (fully-expand-body body)))
  (setq env (append newly-bound
		    (scratchout newly-bound *bound-vars*)))
  (unless (eq condition :INTERN) (push 'trigger-node env))
  (setq fname (generate-rule-procedure-name pattern))
  `(defun ,fname ,env
     ,@ (cond ((eq condition :INTERN) body) ;; Just do it
	      (t ;; Must check and see if the node's belief state
	         ;; matches the rule's requirements
	       `((cond ((,(case condition
				(:IN 'in-node?)(:OUT 'out-node?)
				(t (error "~A bad condition -- GBF"
						 condition)))
			 TRIGGER-NODE) ,@ body)
		       (t (push (list ',fname ,@ env)
				,(ecase condition
				       (:IN `(tms-node-in-rules TRIGGER-NODE))
				       (:OUT `(tms-node-out-rules TRIGGER-NODE)
					     ))))))))))

(defun generate-match-procedure (pattern var test condition)
  (multiple-value-bind (tests binding-specs)
   (generate-match-body
    pattern (pattern-free-variables pattern) test)
   `(defun ,(generate-rule-procedure-name pattern)
      (P ,@ *bound-vars*)
       ;;first arg, P, is the pattern
       (if (and ,@ tests)
	   (values T (list ,@ (if var '(P))
			   ,@ (reverse binding-specs))
		   ,(unless (eq condition :INTERN) t))))))

(defun scratchout (l1 l2)  ;non-destructive and order-preserving
  (dolist (el1 l1 l2) (setq l2 (remove el1 l2))))

(defun generate-rule-procedure-name (pattern)
  (intern (format nil "~A-~A-~A" *file-prefix* pattern (incf *file-counter*))))

;;;; Recursive macroexpansion

(defvar *macros-to-expand*
  '(rule rlet rassert! rretract!
    internal-rule add-internal-rule with-pushed-variable-bindings
    without-contradiction-check with-contradiction-check
    with-contradiction-handler with-JTRE))

(defun fully-expand-body (body)
  (cond ((null body) nil)
	((not (listp body)) body)
	((symbolp (car body))
	 (cond ((member (car body) *macros-to-expand*)
		(fully-expand-body (macroexpand body)))
	       (t (cons (car body)
			(fully-expand-body (cdr body))))))
	(t (cons (fully-expand-body (car body))
		 (fully-expand-body (cdr body))))))

;;;; Running rules

(defun insert-rule (dbclass matcher body &aux rule)
  (let ((*JTRE* (dbclass-jtre dbclass)))
    (setq rule (make-rule :MATCHER matcher
			  :BODY body
			  :DBCLASS dbclass
			  :ID (incf (jtre-rule-counter *JTRE*))))
    (push rule (dbclass-rules dbclass))
    (dolist (candidate (dbclass-facts dbclass))
	    (try-rule-on rule candidate))))

(defun try-rules (datum)
  (dolist (rule (dbclass-rules (datum-dbclass datum)))
    (try-rule-on rule datum)))

(defun try-rule-on (rule datum)
  (let ((*JTRE* (dbclass-jtre (datum-dbclass datum))))
    (multiple-value-bind (okay? bindings node?)
     (funcall (rule-matcher rule)
	      (datum-lisp-form datum))
     (when okay?
	   (when node?
		 (push (datum-tms-node datum) bindings))
	   (enqueue (cons (rule-body rule) bindings) *JTRE*)))))

(defun run-rules (&optional (*JTRE* *JTRE*))
  (do ((form (dequeue *JTRE*) (dequeue *JTRE*))
       (counter 0 (1+ counter)))
      ((null form)
       (debugging-jtre "~%    ~A rules run."  counter)
       (incf (jtre-rules-run *JTRE*) counter))
    (apply (car form) (cdr form))))

(defun rules-waiting? (jtre) (jtre-queue jtre))

(defun enqueue (new j) (push new (jtre-queue j)))

(defun dequeue (jtre) (pop (jtre-queue jtre)))

;;;; Display routines

(defun show-rules (&optional (*JTRE* *JTRE*) (stream *standard-output*))
  (format t "~%There are ~D rules in ~A:" 
	  (jtre-rule-counter *JTRE*) (jtre-title *JTRE*))
  (format stream "~% ~A queued." (if (null (jtre-queue *JTRE*)) "None"
				   (length (jtre-queue *JTRE*))))
  (map-dbclass #'(lambda (dbclass)
		 (dolist (rule (dbclass-rules dbclass))
			 (print-rule rule stream)))))

(defun print-rule (rule &optional (stream *standard-output*))
  (format stream "~% ~A: ~A, ~A" rule
	  (rule-matcher rule) (rule-body rule)))

(defun test-rule-expansion ()
 (pprint (macroexpand
	  '(rule ((:IN (implies ?p ?q) :VAR ?f1)
		  (:IN ?p)) (rassert! ?q (:CE ?f1 ?p))))))

(defun get-rule (num &optional (*JTRE* *JTRE*))
  (map-dbclass #'(lambda (dbclass)
		 (dolist (rule (dbclass-rules dbclass))
			 (when (= (rule-id rule) num)
			       (return-from GET-RULE rule))))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -*- unify.lisp; -*-

;;;; Variables and unification

;(in-package :COMMON-LISP-USER)

(defun variable? (x)
  (and (symbolp x)	;A symbol whose first character is "?"
       (char= #\? (elt (symbol-name x) 0))))

(defun unify (a b &optional (bindings nil))
   (cond ((equal a b) bindings)
	 ((variable? a) (unify-variable a b bindings))
	 ((variable? b) (unify-variable b a bindings))
	 ((or (not (listp a)) (not (listp b))) :FAIL)
	 ((not (eq :FAIL (setq bindings
			       (unify (car a) (car b) bindings))))
	  (unify (cdr a) (cdr b) bindings))
	 (t :FAIL)))

(defun unify-variable (var exp bindings &aux val)
  ;; Must distinguish no value from value of nil
  (setq val (assoc var bindings))
  (cond (val (unify (cdr val) exp bindings))
	;; If safe, bind <var> to <exp>
	((free-in? var exp bindings) (cons (cons var exp) bindings))
	(t :FAIL)))

(defun free-in? (var exp bindings)
  ;; Returns nil if <var> occurs in <exp>, assuming <bindings>.
  (cond ((null exp) t)
	((equal var exp) nil)
	((variable? exp)
	 (let ((val (assoc exp bindings)))
	   (if val 
	       (free-in? var (cdr val) bindings)
	     t)))
	((not (listp exp)) t)
	((free-in? var (car exp) bindings)
	 (free-in? var (cdr exp) bindings))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -*- funify.lisp; -*- 

;;; Extra pattern-matching facilities for FTRE

;(in-package :COMMON-LISP-USER)

(proclaim '(special *bound-vars*))

(defun quotize (pattern)
  (cond ((null pattern) nil)
	((variable? pattern) pattern)
	((not (listp pattern)) (list 'QUOTE pattern))
	((eq (car pattern) :EVAL) (cadr pattern))
	(t `(cons ,(quotize (car pattern))
		  ,(quotize (cdr pattern))))))

(defmacro rlet (var-specs &rest body)
  ;; Provides means for lisp code in body to
  ;; add information to the rule's environment.
  (let ((*bound-vars*
	 (append (mapcar #'car var-specs) *bound-vars*)))
    `(let ,(mapcar
	    #'(lambda (let-clause)
		(list (car let-clause)
		      (if (and (listp (cadr let-clause))
			       (eq (car (cadr let-clause))
				   :EVAL))
			  (cadr (cadr let-clause))
			(quotize (cadr let-clause)))))
			    var-specs)
       ,@ (fully-expand-body body))))

;;; Finding free variables in a pattern

(defun pattern-free-variables (pattern)
  (pattern-free-vars1 pattern nil))

(defun pattern-free-vars1 (pattern vars)
  (cond ((null pattern) vars)
	((variable? pattern)
	 (if (or (member pattern vars)
		 (member pattern *bound-vars*))
	     vars
	     (cons pattern vars)))
	((atom pattern) vars)
	(t (pattern-free-vars1
	     (cdr pattern)
	     (pattern-free-vars1 (car pattern) vars)))))

;;;; Open-coding unification

(defun generate-match-body (pattern vars extra-test
				    &aux structure-tests var-alist
				    equal-tests binding-specs)
  (dolist (test (generate-unify-tests pattern vars nil 'P))
    (cond ((variable? (car test))
	   ;test looks like (?x (nth p) (nth p) ...)
	   (setq equal-tests
		 (append (generate-pairwise-tests (cdr test))
			 equal-tests))
	   (if extra-test 
	       (push (cons (car test) (car (last test)))
		     var-alist))
	   (push (car (last test)) binding-specs))
	  (t (push test structure-tests))))
  (setq extra-test (sublis var-alist extra-test))
  (when (pattern-free-variables extra-test)
    (error "Rule test includes free variable: ~A"
	   extra-test))
  (values (append structure-tests equal-tests
		  (if extra-test (list extra-test)))
	  binding-specs))

(defun generate-pairwise-tests (tests)
  (cond ((or (null tests) (null (cdr tests))) nil)
	(t (cons (list 'EQUAL (car tests) (cadr tests))
		 (generate-pairwise-tests (cdr tests))))))

;;; Generate a list of explicit tests for matching 
;;; the given pattern. Assumes that the pattern
;;;    to be tested will be in variable "P".
;;; Tests are returned in backward order.
;;; (generate-unify-tests '(foo ?x) nil nil 'P)
;;;     returns:    '((NULL (CDR (CDR P)))
;;;                   (EQUAL ?X (CAR (CDR P)))
;;;                   (CONSP (CDR P))
;;;                   (EQUAL (QUOTE FOO) (CAR P))
;;;                   (CONSP P))

(defun generate-unify-tests (pattern vars tests path)
  (cond ((null pattern)
	 	;this is the end
	 (cons `(null ,path) tests))
	((member pattern vars)	                  
         ;; must see if the pattern has been bound elsewhere,
	 ;; and if it has, test to see if the element here is
         ;; consistent with that earlier binding.
	 (let ((previous (assoc pattern tests)))
	   (cond (previous ;add this position to test it
		  (push path (cdr previous))
		  tests)
		 (t (cons (list pattern path) tests)))))
	;; if variable, it must be bound so test
	;; against the current value.
	((variable? pattern) (cons `(equal ,pattern ,path)
				   tests))
	;; if not a list, then see if equal
	((numberp pattern)
	 (cons `(and (numberp ,path) (= ,pattern ,path))
	       tests))
	((atom pattern) (cons `(equal ',pattern ,path) tests))
	;; recurse on a list
	(t (generate-unify-tests (cdr pattern) vars
		 (generate-unify-tests (car pattern) vars
				       ;avoid lisp errors
				       (cons `(consp ,path)
					     tests)
				       	    ;extend the path
				       (list 'car path))
		 ;extend path in other direction
		 (list 'cdr path)))))		
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -*- jtest.lisp; -*-

;;;; Simple shakedown procedure for JTRE 

;(in-package :COMMON-LISP-USER)

(defun shakedown-jtre ()
  (in-jtre (create-jtre "Test One"))
  (dolist (form '((rule ((:INTERN (foo ?x) :VAR ?f :TEST (numberp ?x))
			 (:INTERN (bar ?y) :VAR ?g :TEST (numberp ?y)))
			(rassert! (mumble ?x ?y) (Test-intern ?f ?g)))
		  (format t "~% :INTERN rule defined okay.")
		  (rule ((:IN (foo ?x) :VAR ?f
			      :TEST (not (numberp ?x)))
			 (:IN (bar ?y) :VAR ?g
			      :TEST (not (numberp ?y))))
			(rassert! (grumble ?x ?y)
			 (:TEST-in ?f ?g)))
		  (format t "~% :IN rule defined okay.")
		  (referent '(foo 1) t)
		  (cond ((fetch '(foo 1))
			 (format t "~% Referent worked okay."))
			(t (error "Referent failed.")))
		  (referent '(bar 1) t)
		  (run-rules)
		  (format t "~% No errors during attempted rule execution.")
		  (cond ((fetch '(mumble 1 1))
			 (format t "~%:INTERN rule fired okay."))
			(t (error "~% :INTERN rule failed to fire.")))
		  (referent '(foo a) t)
		  (referent '(bar a) t)
		  (run-rules)
		  (when (some #'(lambda (fact) (in? fact))
			      (fetch '(grumble ?p ?q)))
			(format t "~%Premature triggering of :IN rule."))
		  (uassume! '(foo a) :USER)
		  (uassume! '(bar a) :USER)
		  (cond ((in? '(grumble a a))
			 (format t "~% :IN rule worked okay."))
			(t (format t "~%:IN rule failed to fire.")))
		  (uassume! '(foo 1) :USER)
		  (uassume! '(bar 1) :USER)
		  (unless (in? '(mumble 1 1))
			  (format t "~% Reference or JTMS failure.")))
		:OKAY)
	  (print (eval form))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -*-jqueens.lisp;

;;;; Example of dependency-directed search using JTRE

;(in-package :COMMON-LISP-USER)

;;; Statistics
(defvar *n-assumptions* 0)
(defvar *placements* nil)

(proclaim '(special *JTRE*))

(defvar *queen-rules-file* "jqrule.lisp")
;  #+ILS "/u/bps/code/jtms/jqrule.rbin"
;  #+PARC "virgo:/virgo/dekleer/bps/code/jtms/jqrule.lisp"
;  #+MCL "Macintosh HD:BPS:jtms:jqrule.fasl")

(defun test-queens (from to)
  (do ((n from (1+ n)))
      ((> n to))
      (gc)
      (time (n-queens n))
      (format t "~% For n=~D, ~D solutions, ~D assumptions."
	     n (length *placements*) *n-assumptions*)))

(defun n-queens (n &optional (debugging? nil))
  (setup-queens-puzzle n debugging?)
  (solve-queens-puzzle (make-queens-choice-sets n))
  (length *placements*))

;;;; Setup and search

(defun setup-queens-puzzle (n &optional (debugging? nil))
  (in-JTRE (create-jtre (format nil "~D-Queens JTRE" n) 
			:DEBUGGING debugging?))
  (setq *placements* nil
	*n-assumptions* 0)
  (load *queen-rules-file*))

(defun make-queens-choice-sets (n)
  (do ((column 1 (1+ column))
       (column-queens nil nil)
       (choice-sets nil))
      ((> column n) (nreverse choice-sets))
    (dotimes (row n)
     (push `(Queen ,column ,(1+ row)) column-queens))
    (push (nreverse column-queens) choice-sets)))

(defun solve-queens-puzzle (choice-sets)
  (cond ((null choice-sets) (gather-queens-solution))
	(t (dolist (choice (car choice-sets))
	    (unless (in? `(not ,choice) *jtre*)
	     ;respect nogood information
     (multiple-value-bind (nogood? asns)
      (try-in-context choice
       `(solve-queens-puzzle ',(cdr choice-sets))
       *jtre*)
      (incf *n-assumptions*)
      (when nogood?
	    ;;This assumption lost, so justify the negation
	    ;; based on the other relevant assumptions.
	    (assert! `(not ,choice)
		     `(Nogood ,@ 
			      (remove choice asns))))))))))

;;;; JTMS approximation to try-in-context

(defun try-in-context (asn thunk jtre &aux try-marker result)
  (setq try-marker (cons 'TRY asn))
  (with-contradiction-handler (jtre-jtms jtre)
        #'(lambda (jtms contras)
	    (try-contradiction-handler
	     contras jtms asn try-marker jtre))
        (unwind-protect
	  (progn (unless (in? asn jtre)
		   (setq result (catch 'TRY-CONTRADICTION-FOUND
				  (assume! asn try-marker jtre)))
		   (when (and (listp result) (eq (car result) :ASNS))
		     (return-from TRY-IN-CONTEXT
				  (values t (mapcar #'view-node
						    (cdr result)))))
		   (setq result (catch 'TRY-CONTRADICTION-FOUND
				  (run-rules jtre)))
		   (when (and (listp result)  (eq (car result) :ASNS))
		     (return-from TRY-IN-CONTEXT
				  (values t (mapcar #'view-node
						    (cdr result)))))
		   (eval thunk) ;; use the thunk
		   (progn (retract! asn try-marker t)
			  (return-from TRY-IN-CONTEXT
				       (values nil nil))))))))

(defun try-contradiction-handler (contras jtms asn marker *JTRE*
					  &aux node)
  (unless (eq jtms (jtre-jtms *JTRE*))
    (error "~%High Contradiction Weirdness: ~A not jtms for ~A!"
	   jtms *JTRE*))
  (unless contras (return-from TRY-CONTRADICTION-HANDLER nil))
  (unless asn (return-from TRY-CONTRADICTION-HANDLER nil))
  (setq node (get-tms-node asn))
  (dolist (cnode contras)
    (let ((asns (assumptions-of-node cnode)))
      (when (member node asns)
	(retract! asn marker)
	(throw 'TRY-CONTRADICTION-FOUND (cons :ASNS asns))))))

;;; Other helpers

(defun queens-okay? (x1 y1 x2 y2)
  (not (or (= y1 y2) (= (abs (- x1 x2)) (abs (- y1 y2))))))

(defun gather-queens-solution ()
  (push (remove-if #'(lambda (q) (out? q *jtre*))
		   (fetch `(Queen ?c ?r) *jtre*))
	*placements*))

(defun show-queens-solution (solution &aux n)
  (setq n (length solution))
  (dotimes (i n)
    (terpri)
    (dotimes (j n)
      (format t "~A"
	      (if (member `(queen ,i ,j) solution
			  :TEST #'equal) "Q" "-")))))
;;++++++++++++++++++++++++++++++++==
;; -*- Mode: Lisp; -*-

;;;; Algebraic simplifier

;(in-package :COMMON-LISP-USER)

;;; This version is inspired by one of G.J. Sussman's scheme matchers.

(defvar *simplify-cache* (make-hash-table :TEST #'equal))

(defun simplify (exp)
  (or (gethash exp *simplify-cache*)
      (setf (gethash exp *simplify-cache*) 
	    (simplify-it exp *algebra-rules*))))

(defun clear-simplify-cache ()
  (clrhash *simplify-cache*))

(defun simplify-it (exp rules &aux result)
  (setq result
	(try-matcher-rules
	 (if (listp exp) (mapcar #'simplify exp)
	   exp)
	 rules))
  (if (equal result exp) result
    (simplify-it result rules)))

(defun try-matcher-rules (exp rules)
  (dolist (rule rules exp) ;; Return the original expression by default
    (let ((bindings (match (rule-pattern rule) exp nil)))
;;      FOR DEBUGGING
;;      (unless (eq bindings :FAIL)
;;      (format t "~% Matched ~A on ~A." exp rule)
;;      (dolist (binding bindings)
;;      (format t "~% ~A: ~A." (car binding)
;;                (var-value (list '? (car binding)) bindings))))
      (unless (eq bindings :FAIL)
	(when (check-predicate (rule-predicate rule) bindings)
	  (return-from try-matcher-rules
		       (substitute-in (rule-skeleton rule) bindings)))))))

(defun check-predicate (proc bindings)
  (unless proc (return-from check-predicate T))
  (eval (substitute-in proc bindings)))

(defun rule-pattern (rule) (car rule))
(defun rule-predicate (rule) (cadr rule))
(defun rule-skeleton (rule) (caddr rule))

;;;; Algebra utilities

(defun alg< (e1 e2) ;; Sort predicate for algebraic expressions
  (cond ((equal? e1 e2) nil)
	((consp e1)
	 (if (consp e2)
	     (if (equal? (car e1) (car e2))
		 (alg< (cdr e1) (cdr e2))
		 (alg< (car e1) (car e2)))
	     nil))
	((consp e2) t)
	((symbolp e1)
	 (if (symbolp e2)
	     (string< (symbol-name e1) (symbol-name e2))
	     nil))
	((symbolp e2) t)
	((and (numberp e1) (numberp e2)) (< e1 e2))
	(t (error "alg< cannot compare these: ~A, ~A." e1 e2))))

(defun alg= (e1 e2) (not (or (alg< e1 e2) (alg< e2 e1))))
 
(defun sorted? (list pred)
  (cond ((or (null list) (null (cdr list))) t)
	((funcall pred (cadr list) (car list)) nil)
	(t (sorted? (cdr list) pred))))

(defun +/*? (exp) (or (eq exp '+) (eq exp '*)))

(defun same-constant? (exp constant)
  (and (numberp exp)
       (if (floatp exp) (equal? exp (float constant))
	   (= exp constant))))

(defun zero? (exp) (same-constant? exp 0))
(defun one? (exp) (same-constant? exp 1))

;;;; Extra utilities

(defun occurs-in? (exp1 exp2) 
  (cond ((equal exp1 exp2) t)
	((null exp2) nil)
	((listp exp2)
	 (or (occurs-in? exp1 (car exp2))
	     (occurs-in? exp1 (cdr exp2))))))


;;;; Rules for algebraic simplification

(setq *algebra-rules* `(
;; Flush degenerate cases
(((? op +/*?) (? e)) nil (? e))
((+ (? zero zero?) (?? e)) nil (+ (?? e)))
((- (? zero zero?) (? e)) nil (- (? e)))
((- (? e) (? zero zero?)) nil (? e))
((- (? e) (? e)) nil 0)
((* (? one one?) (?? e)) nil (* (?? e)))
((* (? zero zero?) (?? e)) nil 0)
((expt (? e) (? zero zero?)) nil 1)
((expt (? e) (? one one?)) nil (? e))
((log (? one one?) (? base)) nil 0)
((log (? base) (? base)) nil 1)
((log (expt (? base) (? val)) (? base)) nil (? val))
((expt (? base) (log (? val) (? base))) nil (? val))
;; Equivalences involving powers
((* (? e) (? e)) nil (sqr (? e)))
((expt (? e) (? two ,#'(lambda (exp) (same-constant? exp 2))))
 nil (sqr (? e)))
((sqrt (sqr (? e))) nil (abs (? e)))
((sqr (sqrt (? e))) nil (? e))

;; Combine numerical constants
(((? op +/*?) (? e1 numberp) (? e2 numberp) (?? e3))
 nil
 ((? op) (:EVAL ((? op) (? e1) (? e2))) (?? e3)))
((- (- (? e1) (? e2))) nil (- (? e2) (? e1))) ;; strip
((- (? e1 numberp) (? e2 numberp)) nil (:EVAL (- (? e1) (? e2))))
((- (? e1 numberp)) nil (:EVAL (- (? e1))))
((- (? e1) (? e2 numberp)) nil (+ (- (? e2)) (? e1)))
((- (? e1 numberp) (+ (? e2 numberp) (?? e3)))
 nil (- (:EVAL (- (? e1) (? e2))) (+ (?? e3))))
((- (? e1 numberp) (- (? e2 numberp) (?? e3)))
 nil (+ (:EVAL (- (? e1) (? e2))) (?? e3)))
((+ (? e1 numberp) (- (? e2 numberp) (?? e3)))
 nil (- (:EVAL (+ (? e1) (? e2))) (?? e3)))
((sqr (? e1 numberp)) nil (:EVAL (* (? e1)  (? e1))))
((sqrt (? e1 numberp)) nil (:EVAL (sqrt (? e1))))
((expt (? e1 numberp) (? e2 numberp)) nil (:EVAL (expt (? e1) (? e2))))
((/ (? e1 numberp) (? e2 numberp)) nil (:EVAL (/ (? e1) (? e2))))
((* (? e1 numberp) (/ (? e2) (? e3 numberp))) nil
 (* (:EVAL (/ (? e1) (? e3))) (? e2)))
((/ (* (? e1 numberp) (? e2)) (? e3 numberp)) nil
 (* (:EVAL (/ (? e1 numberp) (? e3 numberp))) (? e2)))
((* (?? pre) (- (? term)) (?? post)) nil
 (* (?? pre) (* -1 (? term)) (?? post)))
((abs (? e numberp)) nil (:EVAL (abs (? e))))
((log (? x numberp) (? base numberp)) 
 nil (:EVAL (/ (log (? x)) (log (? base)))))
;; Flatten +,*
(((? op +/*?) (?? e1) ((? op) (?? e2) (?? e3)))
 nil
 ((? op) (?? e1) (?? e2) (?? e3)))
;; Combine like terms
((+ (?? pre) (* (? f1) (? thing)) (* (? f2) (? thing)) (?? post)) nil
 (+ (?? pre) (* (* (? f1) (? f2)) (? thing)) (?? post)))
((+ (?? pre) (* (? f1) (? thing)) (?? mid) (? thing) (?? post)) nil
 (+ (?? pre) (* (+ 1 (? f1)) (? thing)) (?? mid) (?? post)))
;; Canonicalize +,*
(((? op +/*?) (?? terms))
 (not (sorted? (quote (? terms)) #'alg<))
 ((? op) (:SPLICE (sort (quote (? terms)) #'alg<))))))	
;;++++++++++++++++++++++==
;; -*- Mode: Lisp; -*-

;;;; Pattern matcher for algebraic manipulation systems

;(in-package :COMMON-LISP-USER)

;;; There are two kinds of variables.
;;; Element variables match a single element of a list.
;;; Segment variables match a (perhaps empty) piece of a list.
;;; Element variables have the form (? <var name> <optional restriction>)
;;;  where <var name> is a symbol, and the restriction is a one-place
;;;  procedure which returns non-nil if the potential binding satisfies it.
;;; Segment variables are like element variables, but start with ??.

;;; The basic entry point is MATCH, which takes a pattern, a datum expression,
;;; and an alist of bindings.

(defun match (pat dat &optional (dict nil))
  (cond ((eq dict :FAIL) :FAIL) ;; Propagate lossage
	((eq pat dat) dict) ;; Easy win
	((element-var? pat)
	 (match-element-var pat dat dict))
	((not (consp pat))
	 (if (equal? pat dat) dict :FAIL))
	((segment-var? (car pat))
	 (match-segment-var pat dat dict))
	((not (consp dat)) :FAIL)
	(t (match (cdr pat) (cdr dat)
		  (match (car pat) (car dat) dict)))))

(defun match-element-var (pat dat dict &aux entry pred)
  (setq entry (lookup-var pat dict))
  (cond (entry 
	 (if (equal? (cadr entry) dat) dict :FAIL))
	(t (setq pred (var-restriction pat))
	   (cond ((or (not pred)
		      (funcall pred dat))
		  (bind-element-var (var-name pat) dat dict))
		 (t :FAIL)))))

(defvar *tol* 1.0e-6)

(defun equal? (a b)
  (cond ((and (floatp a) (floatp b)) (< (abs (- a b)) *tol*))
	(t (equal a b))))

;;;; Finding matches for segment variables
;; This is non-deterministic, hence requires iteration.

(defun match-segment-var (pat dat dict &aux entry pred end rest)
  (setq entry (lookup-var (car pat) dict))
  (cond (entry ;; check for match
         (setq rest 
	       (check-segment dat (segment-beg entry)
			      (segment-end entry)))
	 (if (eq rest :FAIL) :FAIL
	     (match (cdr pat) rest dict)))
	(t ;; Search for alternate segment bindings
	 (try-segment-bindings (car pat) (cdr pat) dat dict))))

(defun check-segment (dat beg end)
  (cond ((eq beg end) dat)
	((not (consp dat)) :FAIL)
	((equal? (car dat) (car beg))
	 (check-segment (cdr dat) (cdr beg) end))
	(t :FAIL)))

(defun try-segment-bindings (var pat dat dict &aux name pred beg)
  (setq name (var-name var)
	pred (var-restriction var)
	beg dat)
  (do ((end dat (cdr end))
       (ndict nil))
      ((null end)
       (cond ((or (null pred)
		  (funcall pred (segment->list beg nil)))
	      (match pat nil ;; Try the very end
		     (bind-segment-var name beg nil dict)))
	     (t :FAIL)))
    (when (or (null pred)
	      (funcall pred (segment->list beg end)))
      (setq ndict (match pat end 
			 (bind-segment-var name beg end dict)))
      (unless (eq ndict :FAIL)
	      (return-from TRY-SEGMENT-BINDINGS ndict)))))

;;;; Defining variables

(defun pattern-variable? (x) (or (element-var? x) (segment-var? x)))
(defun element-var? (x) (and (consp x) (eq (car x) '?)))
(defun segment-var? (x) (and (consp x) (eq (car x) '??)))
(defun var-name (x) (cadr x))
(defun var-restriction (x) (caddr x))

;; Dictionary entries take the form
;; (<name> <position> <value>), where <position> is NIL if an element
;;   variable, and (<beg> . <end>) if a segment variable.

;; Accessing entries
(defun lookup-var (var dict) (assoc (var-name var) dict))

(defun var-value (var dict &aux entry)
  (setq entry (lookup-var var dict))
  (unless entry (error "Not bound variable: ~A, ~A." var dict))
  (cond ((= (length entry) 2) (cadr entry)) ;; element variable
	(t (segment->list (cadr entry) (caddr entry)))))

(defun segment-beg (entry) (cadr entry))
(defun segment-end (entry) (caddr entry))

(defun segment->list (start end)
  (do ((point start (cdr point))
       (l nil))
      ((eq point end) (nreverse l))
    (push (car point) l)))

;; Updating dictionaries
(defun bind-element-var (name dat dict)
  (cons (list name dat) dict))
(defun bind-segment-var (name beg end dict)
  (cons (list name beg end) dict))

;; Performing substitutions
(defun substitute-in (exp dict)
  (cond ((null exp) nil)
	((element-var? exp) (var-value exp dict))
	((consp exp)
	 (cond ((segment-var? (car exp))
		(append (var-value (car exp) dict)
			(substitute-in (cdr exp) dict)))
	       ((eq (car exp) :EVAL)
		(eval (substitute-in (cadr exp) dict)))
	       ((and (consp (car exp)) (eq (caar exp) :SPLICE))
		(append (eval (substitute-in (cadar exp) dict))
			(substitute-in (cdr exp) dict)))
	       (t (cons (substitute-in (car exp) dict)
			(substitute-in (cdr exp) dict)))))
	(t exp)))
		  
;;----------------------------

;;;; JSAINT: A rational reconstruction of Slagel's SAINT program

;(in-package :COMMON-LISP-USER)

(defstruct (Jsaint 
		   (:PRINT-FUNCTION (lambda (a st ignore)
				      (format st "<Agenda ~A>"
					      (jsaint-title a)))))
  (title "")           ;; Name for printing
  (jtre nil)           ;; Associated JTRE
  (agenda nil)         ;; List of queued subproblems
  (problem nil)        ;; When solved, we are done.
  (solution nil)       ;; Cached answer.
  (n-subproblems 0)    ;; Statistic
  (max-tasks 20)       ;; resource bound
  (debugging nil))     ;; Debugging flag

;; Start with the usual encapsulation

(proclaim '(special *jsaint*))

(defvar *jsaint* nil)

(defun create-jsaint (title problem &key (debugging nil)
			    (max-tasks nil))
  (let ((ag (make-jsaint
	      :TITLE title
	      :PROBLEM problem
	      :JTRE (create-jtre 
		      (concatenate 'string "JTRE of " title))
	      :DEBUGGING debugging
	      :MAX-TASKS (if (integerp max-tasks) max-tasks 20))))
    (in-jtre (jsaint-jtre ag))
    (change-jtms (jtre-jtms (jsaint-jtre ag))
		 :CONTRADICTION-HANDLER #'jsaint-contradiction-handler)
    (use-jsaint ag)))

(defmacro debugging-jsaint (js msg &rest args)
  `(when (jsaint-debugging ,js) (format t ,msg ,@ args)))

(defun change-jsaint (js &key (debugging :NADA) (problem :NADA)
		       (max-tasks :NADA))
  (unless (eq debugging :NADA) (setf (jsaint-debugging js) debugging))
  (unless (eq problem :NADA) (setf (jsaint-problem js) problem))
  (unless (eq max-tasks :NADA) (setf (jsaint-max-tasks js) max-tasks)))

(defun use-jsaint (js) (setq *jsaint* js))

(defmacro with-jsaint (js &rest forms) `(let ((*ag* ,js)) ,@ forms))

;;;; User entry point

(defvar *jsaint-rules*  "jsrules.lisp")
(defvar *jsaint-operators*  "jsops.lisp")


(defun solve-integral (integral
		       &key (title (symbol-name (gensym)))
		       (debugging nil)
		       (max-tasks 20))
  ;; Remove redudancies and canonicalize input. 
  (setq integral (eval (quotize (simplifying-form-of integral))))
  (use-jsaint (create-jsaint title integral
			     :DEBUGGING debugging
			     :MAX-TASKS max-tasks))
  (queue-problem (jsaint-problem *jsaint*) nil)
  (with-JTRE (jsaint-jtre *jsaint*) 
	     (load *jsaint-rules*)
	     (load *jsaint-operators*))	     
  (run-jsaint *jsaint*))

(defun $explainresult (&optional (*jsaint* *jsaint*))
  (cond ((null (jsaint-solution *jsaint*))
	 (format t "~% Problem not solved yet."))
	((eq (jsaint-solution *jsaint*) :FAILED-PROBLEM)
	 (explore-network (get-tms-node `(failed ,(jsaint-problem *jsaint*))
				    (jsaint-jtre *jsaint*)))
	 (format t "~% Failed to find a solution."))
	((eq (jsaint-solution *jsaint*) :FAILED-EMPTY)
	 (format t "~% Ran out of things to do.")
	 (explore-network (get-tms-node `(failed ,(jsaint-problem *jsaint*))
				    (jsaint-jtre *jsaint*))))
	(t (format t "~% Solved the problem:")
	   (explore-network (get-tms-node
			 `(solution-of ,(jsaint-problem *jsaint*)
				       ,(jsaint-solution *jsaint*))
			 (jsaint-jtre *jsaint*))))))

;;;; Basic algorithm 

(defun run-jsaint (*jsaint*)
  (when (jsaint-solution *jsaint*)
    (return-from run-jsaint ;; Don't re-solve
      (values (jsaint-solution *jsaint*) *jsaint*)))
  (when (> (jsaint-n-subproblems *jsaint*)
	   (jsaint-max-tasks *jsaint*))
    (return-from run-jsaint ;; Respect resource limits
      (values :TIME-OUT *jsaint*)))
  (do ((done? nil)
       (solution (fetch-solution (jsaint-problem *jsaint*) *jsaint*)
		 (fetch-solution (jsaint-problem *jsaint*) *jsaint*))
       (failure-signal `(Failed (Integrate ,(jsaint-problem *jsaint*)))))
      (done? (values (jsaint-solution *jsaint*) *jsaint*))
    (cond (solution (setf (jsaint-solution *jsaint*) solution)
            (debugging-jsaint *jsaint*
             "~% ~A: Solved original problem." (jsaint-title *jsaint*))
	    (setq done? t))
	  ((in? failure-signal (jsaint-jtre *jsaint*))
	   (debugging-jsaint *jsaint*
	     "~% ~A: Failed on original problem."
	     (jsaint-title *jsaint*)) 
	   (setf (jsaint-solution *jsaint*) :FAILED-PROBLEM)
	   (setq done? t))
	  ((null (jsaint-agenda *jsaint*))
	   (debugging-jsaint *jsaint* "~% ~A: Agenda empty."
			     (jsaint-title *jsaint*))
	   (setf (jsaint-solution *jsaint*) :FAILED-EMPTY)
	   (setq done? t))
	  (t (process-subproblem (cdr (pop (jsaint-agenda *jsaint*))))))))

(defun process-subproblem (item &aux (jtre (jsaint-jtre *jsaint*))
			   (suggestions nil))

  (debugging-jsaint *jsaint* "~%  Trying to solve ~A." item)
  (open-subproblem item)
  (when (fetch-solution item *jsaint*)
	;; Bookkeeping is done by pdis rules
	(debugging-jsaint *jsaint* "~%    ..already solved.")
	(return-from process-subproblem T))
  (when (some #'(lambda (f) (in? f jtre)) ;; Already expanded
	      (fetch `(AND-SUBGOALS ,item ?subproblems) jtre))
	(debugging-jsaint *jsaint* "~%   ..already expanded.")
    (return-from process-subproblem T))
  (dolist (suggestion (fetch `(SUGGEST-FOR ,item ?operator) jtre))
    (when (in? suggestion jtre)
      (queue-problem `(try ,(third suggestion)) item)
      (push `(try ,(third suggestion)) suggestions)))
  ;; Presume extra subgoals don't come along.
  (assert! `(OR-SUBGOALS ,item ,suggestions) :OR-SUBGOALS jtre)
  (run-rules jtre))

(defun open-subproblem (item &aux (jtre (jsaint-jtre *jsaint*)))
  (assert! `(expanded ,item) :EXPAND-AGENDA-ITEM jtre)
  (assume! `(open ,item) :EXPAND-AGENDA-ITEM jtre)
  ;; Look for quick win, extra consequences.
  (run-rules jtre))

;;;; Queuing problems
;; Queue entries take the form (<difficulty> . <subproblem>)
;; Difficulty estimates are based on the form of the subproblem
;; alone, since there could be multiple parents for a subproblem.

(defun queue-problem (problem parent &aux entry)
  (setq entry (cons (estimate-difficulty problem) problem))
  (debugging-jsaint *jsaint* "~%   Queueing ~A, difficulty = ~D"
		    problem (car entry))
  (setf (jsaint-agenda *jsaint*)
	(merge 'list (list entry)
	       (jsaint-agenda *jsaint*)
	       #'(lambda (a b) (< (car a) (car b))))))

(defun estimate-difficulty (problem)
  (+ (max-depth problem) (count-symbols problem)))

(defun count-symbols (pr)
  (cond ((null pr) 0)
	((listp pr)
	 (reduce #'+ (mapcar #'count-symbols pr)
		 :INITIAL-VALUE 0))
	(t 1)))

(defun max-depth (pr)
  (cond ((not (listp pr)) 1)
	(t (1+ (reduce #'max (mapcar #'max-depth pr)
		       :INITIAL-VALUE 0)))))

;;;; Auxiliary routines

(defun fetch-solution (problem &optional (*jsaint* *jsaint*)
		       &aux (jtre (jsaint-jtre *jsaint*)))
  (dolist (solution (fetch `(SOLUTION-OF ,problem ?answer) jtre))
    (when (in? solution jtre)
      (return-from fetch-solution (third solution)))))

(defun jsaint-contradiction-handler (contradictions jtms)
  (ask-user-hander contradictions jtms)) ;; default

;;;; Defining operators

(defun delete-me (name trigger &rest keyed-items
			  &aux subproblems result test) (print `(trigger-> ,trigger))
  (setq subproblems (cadr (member :SUBPROBLEMS keyed-items)))(print `(subproblems-> ,subproblems))
  (setq result (cadr (member :RESULT keyed-items)))(print `(result-> ,result))
  (setq test (cadr (member :TEST keyed-items)))(print `(test-> ,test)))

(defmacro defIntegration (name trigger &rest keyed-items
			  &aux subproblems result test) ;(print `(trigger-> ,trigger))
  (setq subproblems (cadr (member :SUBPROBLEMS keyed-items)));(print `(subproblems-> ,subproblems))
  (setq result (cadr (member :RESULT keyed-items)));(print `(result-> ,result))
  (setq test (cadr (member :TEST keyed-items)));(print `(test-> ,test))
  (unless result 
    (error "Integration operator must have result form"))
  `(rule ((:IN (expanded (Integrate ,trigger)) :VAR ?starter
	  ,@ (if test `(:TEST ,test) nil)))
	 (rlet ((?integral ,trigger)
		(?problem (Integrate ,trigger)))
	       (rlet ((?op-instance (,name ?integral)))
	     (rassert! (Operator-Instance ?op-instance)
		       :OP-INSTANCE-DEFINITION)
	     ;; If no subproblems, just create solution
     ,@ (cond ((null subproblems)
	       `((rlet ((?solution
			 (:EVAL (simplify ,(quotize result)))))
		  (rassert! (solution-of ?problem ?solution)
		   (,(keywordize name)
		     (Operator-Instance ?op-instance))))))
	      (t ;; Usual case
	       (let ((subs (calculate-subproblem-list subproblems))) 
		 `((rassert! (suggest-for ?problem ?op-instance)
		    (:INTOPEXPANDER ?starter))
   (rule ((:IN (expanded (try ?op-instance)) :VAR ?try))
	   (rlet ,subs
		 ,@ (mapcar #'(lambda (sub)
				`(queue-problem ,(car sub) ?problem))
			    subs)
		 (rassert! (AND-SUBGOALS (try ?op-instance)
					 ,(mapcar #'car subs))
			   (,(keywordize (format nil "~A-DEF" name))
			    ?try))
		 ;; Solution detector
		 ,(multiple-value-bind (triggers antes)
		      (calculate-solution-rule-parts subs subproblems)
		    `(rule (,@ triggers)
			   (rlet ((?solution
				    (:EVAL (simplify ,(quotize result)))))
				 (rassert! (solution-of ?problem ?solution)
					   (,(keywordize name)
					     ,@ antes)))))))))))))))
					     
(defmacro defDifferentiation (name trigger &rest keyed-items
			  &aux subproblems result test) ;(print `(trigger-> ,trigger))
  (setq subproblems (cadr (member :SUBPROBLEMS keyed-items)));(print `(subproblems-> ,subproblems))
  (setq result (cadr (member :RESULT keyed-items)));(print `(result-> ,result))
  (setq test (cadr (member :TEST keyed-items)));(print `(test-> ,test))
  (unless result 
    (error "Integration operator must have result form"))
  `(rule ((:IN (expanded ((|$Differentiate| SIMP) ,trigger)) :VAR ?starter
	  ,@ (if test `(:TEST ,test) nil)))
	 (rlet ((?derivative ,trigger)
		(?problem ((|$Differentiate| SIMP) ,trigger)))
	       (rlet ((?op-instance (,name ?derivative)))
	     (rassert! (Operator-Instance ?op-instance)
		       :OP-INSTANCE-DEFINITION)
	     ;; If no subproblems, just create solution
     ,@ (cond ((null subproblems)
	       `((rlet ((?solution
			 (:EVAL (simplify ,(quotize result)))))
		  (rassert! (solution-of ?problem ?solution)
		   (,(keywordize name)
		     (Operator-Instance ?op-instance))))))
	      (t ;; Usual case
	       (let ((subs (calculate-subproblem-list subproblems))) 
		 `((rassert! (suggest-for ?problem ?op-instance)
		    (:INTOPEXPANDER ?starter))
   (rule ((:IN (expanded (try ?op-instance)) :VAR ?try))
	   (rlet ,subs
		 ,@ (mapcar #'(lambda (sub)
				`(queue-problem ,(car sub) ?problem))
			    subs)
		 (rassert! (AND-SUBGOALS (try ?op-instance)
					 ,(mapcar #'car subs))
			   (,(keywordize (format nil "~A-DEF" name))
			    ?try))
		 ;; Solution detector
		 ,(multiple-value-bind (triggers antes)
		      (calculate-solution-rule-parts subs subproblems)
		    `(rule (,@ triggers)
			   (rlet ((?solution
				    (:EVAL (simplify ,(quotize result)))))
				 (rassert! (solution-of ?problem ?solution)
					   (,(keywordize name)
					     ,@ antes)))))))))))))))					     

(defvar *test-operator*
	'(defIntegration Integral-of-Sum
	   (Integral (+ ?t1 ?t2) ?var)
	   :SUBPROBLEMS ((?int1 (Integrate (Integral ?t1 ?var)))
			 (?int2 (Integrate (Integral ?t2 ?var))))
	   :RESULT (+ ?int1 ?int2)))

;;;; Helpers for operator definition

(defun calculate-subproblem-list (subproblems &aux (counter -1))
  ;; Takes list of entries whose form is (?result-var ?form)
  ;; and returns a list of (?goal-var ?form)
  (mapcar #'(lambda (pair)
	      (incf counter)
	      (list (intern (format nil "?GOAL~D" counter)) 
		    (simplifying-form-of (cadr pair))))
	  subproblems))

(defun simplifying-form-of (alg-goal)
  ;; Run simplifier on subgoals, just in case.
  (cond ((null alg-goal) nil)
	((not (listp alg-goal)) alg-goal)
	((eq (car alg-goal) 'INTEGRAL) ;; Simplify as needed
	 `(INTEGRAL (:EVAL (SIMPLIFY ,(quotize (cadr alg-goal))))
	   ,(caddr alg-goal)))
	(t (cons (simplifying-form-of (car alg-goal))
		 (simplifying-form-of (cdr alg-goal))))))

(defun calculate-solution-rule-parts (sub-pairs res-pairs
				      &aux (counter -1)
				      (antes nil)
				      (triggers nil))
  (setq triggers
	(mapcar #'(lambda (subpair respair)
		    (incf counter)
		    (let ((rvar (intern (format nil "?RESULT~D" counter))))
		      (push rvar antes)
		      `(:in (solution-of ,(car subpair) ,(car respair))
			:VAR ,rvar)))
		sub-pairs res-pairs))
  (values triggers (nreverse antes)))

(defun keywordize (stuff)
  (cond ((null stuff) (error "Can't keywordize nothing."))
	((listp stuff) (keywordize (car stuff)))
	(t (intern (format nil "~A" stuff) 'keyword))))

;;;; Interrogatives

;;; SHOW-PROBLEM highlights the assertions relevant to the given problem.

(defun show-problem (level pr &optional (*jsaint* *jsaint*)
			&aux stuff ands ors)
  ;(print `(problem++++> ,pr))	
  (if (equal 'try (car pr)) 
     (if (or (equal '|$Derivative|  (caar (cadadr pr)))
              (equal '|$Differentiate|  (caar (cadadr pr))))
            (setf pr (cadadr pr))
        (setf pr (cdr pr))))
        
        ;(print `(new--pr ,pr --->  ,(replac-to-maxima pr)))		
  (mapcar #'displa `(,(replac-to-maxima pr))); "   DifficultyLevel="  ,(estimate-difficulty pr)))			
  ;(format t "~%~A:: (~D)" pr (estimate-difficulty pr))
  (with-JTRE (jsaint-jtre *jsaint*)
  (setq stuff (fetch `(parent-of ,pr ?x ?type)))
  (cond (stuff ;(format t "~% Parent(s): ")
  (dolist (p stuff)
	  (if (in? p)
	     (progn  
	           (if (equal 'try (car p)) 
	              (progn (mapcar #'displa `(,(replac-to-maxima (third p)) ,(replac-to-maxima (fourth p)))))))
	           
	    (format t "~%    BUG: Should be in: ~A" p))))
	(t ;(format t "~% No parents found.")
	))
  
  (cond ((setq stuff (fetch-solution pr))
              (get-spaces 4) (princ 'Solution=) (get-spaces 2)(displa(replac-to-maxima stuff)))
	((and (setq stuff (car (fetch `(failed ,pr))))
	      (in? stuff)) (format t "~%  Failed."))
	((not (equal (car pr) 'try))
	 (format t "~%    Solved in following steps")))
	 
  (setq ands (fetch `(and-subgoals ,pr ?ands)))
  (when ands (get-spaces 4)(format t "~% And subgoals:")
	(dolist (subg (third (car ands))) 
	       (progn  
		(displa(replac-to-maxima (car (cdr subg)))))))

  (setq ors (fetch `(or-subgoals ,pr ?ors)))
  (when ors (get-spaces 4) (format t "~%    Or subgoals:")
	   (dolist (subg (third (car ors)))  (get-spaces 4)
	        (displa(replac-to-maxima (cons '(mlist) (cdar (cdr subg)))))))))

(defun get-spaces(n)(loop for i from 1 to n do (princ #\Space)))

;;;; Textual display of an AND/OR graph

(defun $showaograph (&optional (*jsaint* *jsaint*))
  (let* ((problems (get-problems))
	 (depth-table (update-ao-depth-table 
		       (jsaint-problem *jsaint*)
		       0 (list (cons (jsaint-problem *jsaint*) 0))
		       (list (jsaint-problem *jsaint*)))))
    (setq depth-table
	  (sort depth-table #'(lambda (x y) (< (cdr x) (cdr y)))))
    (dolist (pair depth-table)
	    (progn (terpri)(get-spaces (* 3 (cdr pair)))(princ 'Level) (princ '=) (princ (cdr pair))
	    (show-problem (cdr pair)(car pair))))))

(defun update-ao-depth-table (now depth depths path)
  (incf depth)
  (dolist (child (get-children now) depths)
   (unless (member child path :TEST 'equal) ;; Yes, can loop!
    (let ((entry (assoc child depths :TEST 'equal)))
      (unless entry 
	      (push (setq entry (cons child 0)) depths))
      (when (> depth (cdr entry))
	    (setf (cdr entry) depth)
	    (setq depths (update-ao-depth-table
			child depth depths (cons child path))))))))

(defun get-children (gp &optional (*jsaint* *jsaint*)
			&aux children)
  (dolist (maybe-kid (fetch `(parent-of ?x ,gp ?type)
			    (jsaint-jtre *jsaint*))
		     children)
	  (if (in? maybe-kid (jsaint-jtre *jsaint*))
	      (push (cadr maybe-kid) children))))

(defun get-problems (&optional (*jsaint* *jsaint*))
  (mapcar 'cadr (fetch '(expanded ?x) (jsaint-jtre *jsaint*))))

;;;; Debugging

;;Convert from maxima code to lisp code
(defun convert-to-lisp(lis)  
    (cond ((null lis) nil)
         ((atom lis) lis)
         ((listp lis)         
         (cond ((equal '(mplus simp) (car lis)) (cons '+ (cdr lis)))
               ((equal '(mtimes simp) (car lis))(cons '* (cdr lis)))
               ((equal '(mexpt simp) (car lis)) (cons 'expt (cdr lis)))
               ((equal '(%log simp) (car lis)) (cons 'log (cdr lis)))
               
               ))))  
    
(defun replac (lis)
 (let ((lis (convert-to-lisp lis)))
  (cond ((null lis) nil) ((atom lis) lis)
   (t
    (mapcar
     #'(lambda (x)
        (cond ((atom x) x) (t (replac x))))
     lis)))))
;;;----     
(defun lisp-to-maxima(lis) 
    (cond ((null lis) nil)
         ((atom lis) lis)
         ((listp lis)         
         (cond ((equal '+ (car lis)) (cons '(mplus simp) (cdr lis)))
               ((equal '/ (car lis)) (list '(mtimes simp) (cadr lis) `((mexpt simp) ,(caddr lis) ,(- 1))))
               ((equal '* (car lis))(cons '(mtimes simp) (cdr lis)))
               ((equal 'expt (car lis))(cons '(mexpt simp) (cdr lis)))
               ((equal 'sqr (car lis))(cons '(mexpt simp) (list (cadr lis) 2)))
               ((equal 'log (car lis))(cons '(%log simp) (cdr lis)))
               ((equal 'derivative (car lis))(cons '(d simp) (cdr lis)))
               (t lis)
               ))))  
    
(defun replac-to-maxima (lis) ;(print lis)
 (let ((lis (lisp-to-maxima lis)))
  (cond ((null lis) nil) ((atom lis) lis)
   (t
    (mapcar
     #'(lambda (x)
        (cond ((atom x) x) (t (replac-to-maxima x))))
    lis)))))     


  
(defun $differentiate (problem var) ;(print `(problem+> ,problem)) 
   (setf problem (replac problem)) ;(print `(problem-> ,problem))
   (replac-to-maxima ($tryjsaint `((|$Differentiate| simp) ((|$Derivative| simp) ,problem ,var)))))

(defun $tryjsaint (problem &optional (title "JSAINT Test")) 
  (solve-integral problem :DEBUGGING nil :TITLE title))

(defun jfetch (pattern) (fetch pattern (jsaint-jtre *jsaint*)))


(defvar problem1-d '(Differentiate (Derivative 1 x)))

(defvar problem2-d '(Differentiate (Derivative (+ x 5) x)))

;;;;;;;;-------------


		 
