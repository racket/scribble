
(define lookup-number
  (let ([phonebook '(("Mr. Bumpy" "555-BUMP" "555-SNOT")
		     ("Squishington" "555-SQSH" "555-BOWL")
		     ("Miss Molly" "555-MOLL" "555-COMF"))])
    (lambda (name home?)
      (sleep 2) ; artificial database delay
      (let loop ([pb phonebook])
	(cond
	 [(null? pb) #f]
	 [(regexp-match name (caar pb))
	  ((if home? cadar caddar) pb)]
	 [else (loop (cdr pb))])))))

(define a-frame 
  (make-object mred:frame% 
	       null ; No parent frame 
	       "Phone Book")) ; The frame's title 

(define a-panel 
  (make-object mred:vertical-panel% 
	       a-frame)) ; Panel is in a-frame 

(define h-panel 
  (make-object mred:horizontal-panel% 
	       a-panel)) ; Panel is in a-frame 

(define name-text  
  (make-object mred:text%  
	       h-panel  
	       (lambda (self event) (refresh-number-info)) 
	       "Name"  ; label
	       ""))    ; initial value
(define quit-button 
  (make-object mred:button% 
	       h-panel 
	       (lambda (self event)  
		 (send a-frame show #f)) 
	       "Quit")) ; Button label

(define number-selector 
  (make-object mred:radio-box% 
	       a-panel 
	       (lambda (self event) (refresh-number-info)) 
	       "" ; No label 
	       -1 -1 -1 -1  ; Default position and size 
	       (list "Home Number" "Office Number")))

(define number-text 
  (make-object mred:text% 
	       a-panel 
	       (lambda (self event) #f) ; No event-handling 
	       "Number" 
	       "(Unknown)"))
(send number-text set-editable #f)

;; First revision: unthreaded

(define refresh-number-info 
  (lambda () 
    (let* ([name (send name-text get-value)] 
	   [home? (zero? (send number-selector get-selection))] 
	   [number (lookup-number name home?)] 
	   [number-string (if number 
			      number 
			      "(Unknown)")]) 
      (send number-text set-value number-string))))

;; Second revision: threaded

(define refresh-number-info
  (let ([adj-cancel-sema (make-semaphore 1)]
	[previous-cancel (box #f)])
    (lambda ()
      (let ([this-cancel (box #f)])
	(semaphore-wait adj-cancel-sema)
	(set-box! previous-cancel #t)
	(set! previous-cancel this-cancel)
	(semaphore-post adj-cancel-sema)
	(thread
	 (lambda ()
	   (send number-text set-value "(Searching...)")
	   (let* ([name (send name-text get-value)]
		  [home? (zero? (send number-selector get-selection))]
		  [number (lookup-number name home?)] ; May take a while...
		  [number-string (if number
				     number
				     "(Unknown)")])
	     (semaphore-wait adj-cancel-sema)
	     (unless (unbox this-cancel)
		     (send number-text set-value number-string))
	     (semaphore-post adj-cancel-sema))))))))

;; Make a class:

(define pb-session%
  (class null ()
    (public
      [refresh-number-info
       (let ([adj-cancel-sema (make-semaphore 1)]
	     [previous-cancel (box #f)])
	 (lambda ()
	   (let ([this-cancel (box #f)])
	     (semaphore-wait adj-cancel-sema)
	     (set-box! previous-cancel #t)
	     (set! previous-cancel this-cancel)
	     (semaphore-post adj-cancel-sema)
	     (thread
	      (lambda ()
		(send number-text set-value "(Searching...)")
		(let* ([name (send name-text get-value)]
		       [home? (zero? (send number-selector get-selection))]
		       [number (lookup-number name home?)] ; May take a while...
		       [number-string (if number
					  number
					  "(Unknown)")])
		  (semaphore-wait adj-cancel-sema)
		  (unless (unbox this-cancel)
			  (send number-text set-value number-string))
		  (semaphore-post adj-cancel-sema)))))))])
    (public
      (a-frame (make-object mred:frame% null "Phonebook"))
      (a-panel (make-object mred:vertical-panel% a-frame)))
    (private
      (h-panel (make-object mred:horizontal-panel% a-panel))
      (name-text (make-object mred:text% h-panel
			      (lambda (self event) (refresh-number-info))
			      "Name" ""))
      (quit-button (make-object mred:button% h-panel
				(lambda (self event) (send a-frame show #f))
				"Quit"))
      (number-selector (make-object mred:radio-box% a-panel
				    (lambda (self event) (refresh-number-info))
				    "" -1 -1 -1 -1 (list "Home Number" "Office Number")))
      (number-text (make-object mred:text% a-panel
				(lambda (self event) #f)
				"Number" "(Unknown)")))
    (sequence
      (send a-frame show #t)
      (send number-text set-editable #f))))



(define pb-counted-session%
  (class pb-session% ()
    (inherit a-frame a-panel) ; We need to access the panel object...
    (rename [basic-refresh-number-info refresh-number-info]) ; and old refresh
    (private [search-counter 0]) ; Counter value
    (public
      [refresh-number-info ; Increment the counter and call old refresh
       (lambda ()
         (set! search-counter (add1 search-counter))
         (send counter-text set-value (number->string search-counter))
         (basic-refresh-number-info))])
    (sequence
      (super-init))  ; Do base class initialization
    (private
     (counter-text (make-object mred:text% a-panel
				(lambda (self event) #f)
				"Number of Searches Started"
				"0")))
    (sequence
     (send counter-text set-editable #f))))
