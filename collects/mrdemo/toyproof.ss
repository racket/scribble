;; Toy proof checkers implemented with MrEd and units
;; Kathi Fisler
;; September 3, 1996

;; This code demonstrates the combined use of MrEd and units to
;; implement a collection of simple proof checkers.

;; Proof Checker Architecture:
;;
;; A proof checker consists of two main components: a list of
;; operations supported by the checker and the interface through which
;; the user invokes the checker.  Each of these components is
;; implemented as a unit in the code below.
;;
;; The "operations" unit, (signature prover-core^) provides a list
;; containing the names of the operations, a list containing the
;; operations corresponding to the names, and a default operation.
;;
;; The "interface" unit (signature interface^) is a compound unit with
;; subunits corresponding to the various pieces of the interface.
;; Subunits exist for how the user inputs data to the checker, how the
;; checker displays results to the user, how the user selects from
;; among the possible operations, how the user instructs the checker
;; to check the current data against the selected operation, and how
;; the user exits the checker.  The interface unit combines these
;; subunits into a main interface and provides a function
;; (create-interface) that can be invoked to create a checker with the
;; specified interface components and the specified operations from
;; the operations unit.

;; The Collection of Checkers
;;
;; Each of the implemented checkers contains three objects into which
;; the user can enter data and one object through which the checker
;; displays results.  Two operations units are provided:
;; proof-core-calculator@ and proof-core-tester@.  The former computes
;; simple arithmetic functions over the three inputs and the latter
;; tests boolean-valued relationships between the three inputs.  The
;; checker displays the values returned by either type of operation.
;;
;; Two types of interfaces are possible: one in which the operations
;; are selected through radio boxes and the other in which the
;; operations are selected through menu options.
;;
;; Three checkers are defined explicitly at the end of the code:
;; make-calculator/radio, make-checker/radio, and make-checker/menu,
;; each taking the appropriate combination of the described interface
;; and operations units.

;;;;; The Implementation ;;;;;

(require-library "trigger.ss")

;;;;; Signature Definitions ;;;;;

(define-signature prover-core^
  (proof-rule-names proof-rule-checkers default-rule-checker))

(define-signature num-entry-interface^
  (create-num-entry-interface get-numbers))

(define-signature result-display-interface^
  (create-result-display-interface show-result))

(define-signature rule-select-interface^
  (create-rule-select-interface
   current-proof-rule set-current-proof-rule))

(define-signature compute-interface^
  (create-compute-interface))

(define-signature quit-interface^
  (create-quit-interface))

(define-signature main-interface^
  (create-interface quit-prover))

(define-signature interface^
  ((open main-interface^)))

;;;;; Compound Unit Definitions ;;;;;

(define interface@
  (lambda (numentry resdisplay ruleselect compute quit main)
    (compound-unit/sig
     (import (MRED : mred^) (CORE : prover-core^))
     (link
      (NUMENTRY : num-entry-interface^ (numentry MRED))
      (RESDISPLAY : result-display-interface^ (resdisplay MRED))
      (RULESELECT : rule-select-interface^ (ruleselect CORE MRED))
      (COMPUTE : compute-interface^ (compute RESDISPLAY RULESELECT
				      NUMENTRY MRED))
      (QUIT : quit-interface^ (quit MAIN MRED))
      (MAIN : main-interface^ (main NUMENTRY RESDISPLAY
				    RULESELECT COMPUTE QUIT MRED))
      )
     (export (open MAIN)))))

(define prover@
  (lambda (core interface)
    (compound-unit/sig
     (import (MRED : mred^))
     (link
      (CORE : prover-core^ (core))
      (INTERFACE : interface^ (interface MRED CORE)))
     (export (open INTERFACE)))))

;;;;; Primitive Unit Definitions ;;;;;

;;;;;;;; Interface Units ;;;;;;;;

(define main-interface-no-menus@
  (unit/sig main-interface^
    (import num-entry-interface^ result-display-interface^
	    rule-select-interface^ compute-interface^
	    quit-interface^ (mred : mred^))
    (define outer-frame
      (make-object mred:frame% `() "Toy Prover"))
    (define v-panel
      (make-object mred:vertical-panel% outer-frame))
    (define upper-hpanel
      (make-object mred:horizontal-panel% v-panel))
    (define lower-hpanel
      (make-object mred:horizontal-panel% v-panel))
    (define create-interface
      (lambda ()
	(create-num-entry-interface upper-hpanel)
	(create-result-display-interface lower-hpanel)
	(create-compute-interface upper-hpanel)
	(create-quit-interface upper-hpanel outer-frame)
	(create-rule-select-interface lower-hpanel)
	(send outer-frame show #t)))
    (define quit-prover
      (lambda ()
	(send outer-frame show #f)))
    ))

(define main-interface-menus@
  (unit/sig main-interface^
    (import num-entry-interface^ result-display-interface^
	    rule-select-interface^ compute-interface^
	    quit-interface^ (mred : mred^))
    (define myframe%
      (class mred:standard-menus-frame% args
	     (rename [super-make-menu-bar make-menu-bar])
	     (public
	      [make-menu-bar
	       (lambda ()
		 (let ([mb (super-make-menu-bar)])
		   (create-rule-select-interface mb)
		   mb))]
	      [panel% mred:vertical-panel%]
	      )
	     (sequence (apply super-init args))))
    (define outer-frame
      (make-object myframe% null "Toy Prover"))
    (define v-panel (ivar outer-frame panel))
    (define upper-hpanel
      (make-object mred:horizontal-panel% v-panel))
    (define lower-hpanel
      (make-object mred:horizontal-panel% v-panel))
    (define create-interface
      (lambda ()
	(create-num-entry-interface upper-hpanel)
	(create-result-display-interface lower-hpanel)
	(create-compute-interface upper-hpanel)
	(create-quit-interface upper-hpanel outer-frame)
	(send outer-frame show #t)))
    (define quit-prover
      (lambda ()
	(send outer-frame show #f)))
    ))

(define num-entry-interface-3text@
  (unit/sig num-entry-interface^
    (import (mred : mred^))

    (define num1 (void))
    (define num2 (void))
    (define num3 (void))

    (define create-num-entry-interface
      (lambda (panel)
	(set! num1
	  (make-object mred:text% panel (lambda (self event) #f)
		       "" ""))
	(set! num2
	  (make-object mred:text% panel (lambda (self event) #f)
		       "" ""))
	(set! num3
	  (make-object mred:text% panel (lambda (self event) #f)
		       "" ""))))
    
    (define get-numbers
      (lambda () 
	(map string->number
	     (list (send num1 get-value) 
		   (send num2 get-value)
		   (send num3 get-value)))))
    ))

(define result-display-interface-text@
  (unit/sig result-display-interface^
    (import (mred : mred^))
    
    (define resdisplay (void))

    (define create-result-display-interface
      (lambda (panel)
	(set! resdisplay
	  (make-object mred:text% panel (lambda (self event) #f) "Result"))
	))
    
    (define show-result
      (lambda (res)
	(send resdisplay set-value
	      (cond ((string? res) res)
		    ((number? res) (number->string res))
		    ((boolean? res) (if res "True" "False"))
		    (error "Argument of Unknown Type")))))
    ))

(define rule-select-interface-radio@
  (unit/sig rule-select-interface^
    (import prover-core^ (mred : mred^))
    
    (define current-proof-rule default-rule-checker)
    
    (define set-current-proof-rule
      (lambda (rule)
	(set! current-proof-rule rule)))
    
    (define create-rule-select-interface
      (lambda (panel)
	(make-object mred:radio-box%
		     panel
		     (lambda (self event)
		       (set-current-proof-rule
			(list-ref proof-rule-checkers
				  (send self get-selection))))
		     "" -1 -1 -1 -1
		     proof-rule-names)))
    ))

(define rule-select-interface-menus@
  (unit/sig rule-select-interface^
	    (import prover-core^ (mred : mred^))
	    (define current-proof-rule default-rule-checker)
	    
	    (define set-current-proof-rule
	      (lambda (rule)
		(set! current-proof-rule rule)))
    
	    (define create-rule-select-interface
	      (lambda (mb)
		(let ([rules-menu (make-object mred:menu%)])
		  (map
		   (lambda (name checker)
		     (send rules-menu append-item name
			   (lambda ()
			     (set-current-proof-rule checker))))
		   proof-rule-names
		   proof-rule-checkers)
		  (send mb append rules-menu "Rules"))))
	    ))

(define compute-interface-button@
  (unit/sig compute-interface^
    (import result-display-interface^ rule-select-interface^
      num-entry-interface^ (mred : mred^))
    (define create-compute-interface
      (lambda (panel)
	(make-object mred:button%
		     panel
		     (lambda (self event)
		       (show-result 
			(apply current-proof-rule (get-numbers))))
		     "Apply Rule")))))

(define quit-interface-button@
  (unit/sig quit-interface^
    (import main-interface^ (mred : mred^))
    (define create-quit-interface
      (lambda (panel outer-frame)
	(make-object mred:button% 
		     panel
		     (lambda (self event) (quit-prover))
		     "Quit")))))

;;;;;;;; Prover Core Units ;;;;;;;;

(define proof-core-calculator@
  (unit/sig prover-core^
    (import)
    (define-struct proof-rule (name checker))

    (define proof-rules
      (list
	(make-proof-rule "Add" +)
	(make-proof-rule "Mult" *)
	(make-proof-rule "Max" max)))

    (define proof-rule-names (map proof-rule-name proof-rules))
    (define proof-rule-checkers (map proof-rule-checker proof-rules))
    (define default-rule-checker (car proof-rule-checkers))))

(define proof-core-tester@
  (unit/sig prover-core^
    (import)
    (define-struct proof-rule (name checker))
    
    (define square
      (lambda (num)
	(* num num)))
    
    (define proof-rules
      (list
       (make-proof-rule "Test Add"
			(lambda (num-1 num-2 num-3)
			  (= num-3 (+ num-1 num-2))))
       (make-proof-rule "Test Mult"
			(lambda (num-1 num-2 num-3)
			  (= num-3 (* num-1 num-2))))
       (make-proof-rule "Test Increasing"
			(lambda (num-1 num-2 num-3)
			  (and (< num-1 num-2)
			       (< num-2 num-3))))
       (make-proof-rule "Test Triangle"
			(lambda (num-1 num-2 num-3)
			  (= (square num-3)
			     (+ (square num-1) (square num-2)))))
	))

    (define proof-rule-names (map proof-rule-name proof-rules))
    (define proof-rule-checkers (map proof-rule-checker proof-rules))
    (define default-rule-checker (car proof-rule-checkers))))

;;;;; Creating Provers ;;;;;

(define make-calculator/radio
  (lambda ()
    (invoke-open-unit/sig
     (prover@ proof-core-calculator@
	    (interface@ num-entry-interface-3text@
		      result-display-interface-text@
		      rule-select-interface-radio@
		      compute-interface-button@
		      quit-interface-button@ 
		      main-interface-no-menus@))
     #f (mred : mred^))
    (create-interface)))

(define make-checker/radio
  (lambda ()
    (invoke-open-unit/sig
     (prover@ proof-core-tester@
	    (interface@ num-entry-interface-3text@
		      result-display-interface-text@
		      rule-select-interface-radio@
		      compute-interface-button@
		      quit-interface-button@ 
		      main-interface-no-menus@))
     #f (mred : mred^))
    (create-interface)))

(define make-checker/menu
  (lambda ()
    (invoke-open-unit/sig
     (prover@ proof-core-tester@
	    (interface@ num-entry-interface-3text@
		      result-display-interface-text@
		      rule-select-interface-menus@
		      compute-interface-button@
		      quit-interface-button@ 
		      main-interface-menus@))
     #f (mred : mred^))
    (create-interface)))
