(define build-pgm-image
  (lambda (filename)
    (let* ([comment #\#]
	   [coords
	    (call-with-input-file
	     filename
	     (lambda (p)
	       (let ([tmp null])
		 (let loop ([line (read-line p)])
		   (unless (eof-object? line)
		     (unless (char=? comment (string-ref line 0))
		       (let ([sp (open-input-string line)])
			 (let loop ([object (read sp)])
			   (unless (eof-object? object)
			     (set! tmp (cons object tmp))
			     (loop (read sp))))))
		     (loop (read-line p))))
		 (reverse tmp))))])
      (when (< (length coords) 4)
	(error 'build-image "found less than four numbers in file: ~s"
	       filename))
      (let* ([tag (first coords)]
	     [width (second coords)]
	     [height (third coords)]
	     [top-value (exact->inexact (fourth coords))]
	     [data (list-tail coords 4)]
	     [2dvec (make-object 2d-vector% width height top-value)]
	     [normalized-data 
	      (map (lambda (x) (/ x top-value))
			      data)])
	(when (not (= (* width height) (length data)))
	  (error 'build-image
		 "data sizes incorrect in file ~s, found ~s expected ~s"
		 filename (length data) (* width height)))
	(send 2dvec bend normalized-data)			      
	2dvec))))

(define build-image
  (lambda (filename)
    (let ([error
	   (lambda ()
	     (error 'build-image "The file \"~a\" could not be loaded." 
		    filename)
	     #f)])
      (if (regexp-match "\\.gif" filename)
	  (let ([b (make-object wx:bitmap% filename wx:const-bitmap-type-gif)])
	    (if (send b ok?)
		(let ([m (make-object wx:memory-dc%)])
		  (send m select-object b)
		  (if (send m ok?)
		      (let* ([w (send b get-width)]
			     [h (send b get-height)]
			     [2dvec (make-object 2d-vector% w h 255)]
			     [update (ivar 2dvec update)]
			     [c (make-object wx:colour%)]
			     [r (ivar c red)]
			     [g (ivar c green)]
			     [b (ivar c blue)])
			(let iloop ([i (sub1 w)])
			  (let jloop ([j (sub1 h)])
			    (send m get-pixel i j c)
			    (let ([r (exact->inexact (r))]
				  [g (exact->inexact (g))]
				  [b (exact->inexact (b))])
			      (update i j (/ (sqrt (/ (+ (* r r) (* g g) (* b b)) 3)) 255)))
			    (unless (zero? j)
				    (jloop (sub1 j))))
			  (unless (zero? i)
				  (iloop (sub1 i))))
			2dvec)
		      (error)))
		(error)))
	  (build-pgm-image filename)))))

(define build-memory-dc
  (lambda (2dvec)
    (let* ([memory-dc (make-object wx:memory-dc%)]
	   [set-pixel (ivar memory-dc set-pixel)]
	   [lookup (ivar 2dvec lookup)]
	   [width (ivar 2dvec width)]
	   [height (ivar 2dvec height)]
	   [bitmap (make-object wx:bitmap% width height)]
	   [scale (1- num-colors)])
      (send memory-dc select-object bitmap)
      (send memory-dc begin-set-pixel)
      (let loop ([x (1- width)] [y (1- height)])
	(let* ([l (lookup x y)])
	  (set-pixel x y (vector-ref colors (floor (* scale l)))))
	(cond
	 [(and (zero? y) (zero? x)) (void)]
	 [(zero? x) (loop (1- width) (1- y))]
	 [else (loop (1- x) y)]))
      (send memory-dc end-set-pixel)
      memory-dc)))

(define get-number-from-user
  (lambda (message title default)
    (let* ([input (wx:get-text-from-user message title default)]
	   [read-in
	    (read-string
	     input
	     (lambda (debug string . rest)
	       (wx:message-box 
		(string-append
		 string
		 (apply
		  string-append
		  (map expr->string rest))))
	       (apply reg-error debug string rest)))])
      (if (and (number? read-in)
	       (positive? read-in)
	       (integer? read-in))
	  read-in
	  (wx:message-box "Expected a positive integer")))))
