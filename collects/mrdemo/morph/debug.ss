(define debug-on? (lambda (type) #f))

(define debug-on? (lambda (type) #t))

(define debug-on?
  (lambda (type)
    (case type
      [(2d-vector%) #f]
      [(engine) #t]
      [(graph) #f]
      [(main) #t]
      [(mesh%) #t]
      [(pager%) #t]
      [else #t])))

(defmacro debug-print (type . args)
  (if (debug-on? type)
      `(begin
	 (display (quote ,type))
	 (display ": ")
	 (debug-print-function ,@args))
      '(void)))

(define debug-print-function
  (letrec ([debug-print-one
	    (lambda (x)
		(cond
		 [(list? x) (debug-print-list x)]
		 [(pair? x) (display "(")
			    (debug-print-one (car x))
			    (display " . ")
			    (debug-print-one (cdr x))
			    (display ")")]
		 [(posn? x) (show-posn x)]
		 [(bary? x) (show-bary x)]
;		 [(3vector? x) (show-3vector x)]
;		 [(intersection? x) (show-intersection x)]
		 [else (display x)]))]
	   [debug-print-list-innards
	    (lambda (list)
	      (cond
	       [(null? list) (void)]
	       [(null? (cdr list)) (debug-print-one (car list))]
	       [else (debug-print-one (car list))
		     (display " ")
		     (debug-print-list-innards (cdr list))]))]
	   [debug-print-list
	    (lambda (x)
	      (display "(")
	      (debug-print-list-innards x)
	      (display ")"))])
    (lambda args
      (debug-print-list-innards args)
      (newline)
      (flush-output)
      (void))))

