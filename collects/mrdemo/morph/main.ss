(define main
  (lambda (mesh src-mem-dc dest-mem-dc src-2dvec dest-2dvec total)
    (letrec* ([width (max (ivar src-2dvec width) (ivar dest-2dvec width))]
	      [dummy-name-width width]
	      [height (max (ivar src-2dvec height) (ivar dest-2dvec height))]

	      [src-lookup (ivar src-2dvec lookup)]
	      [dest-lookup (ivar dest-2dvec lookup)]

	      [build-w
	       (lambda (w)
		 (debug-print main "building image percent" w)
		 (let* ([new-point (engine mesh
					   src-lookup
					   dest-lookup
					   w)]
			[memory-dc (make-object wx:memory-dc%)]
			[set-pixel (ivar memory-dc set-pixel)]
			[bitmap (make-object wx:bitmap% width height)]
			[scale (1- num-colors)])
		   (send memory-dc select-object bitmap)
		   (send memory-dc begin-set-pixel)
		   (let loop ([x width] [y height])
		     (let-values ([(draw-x draw-y color) (new-point x y)])
		       (set-pixel draw-x draw-y 
				  (vector-ref colors
					      (floor (* color
							scale))))
		       (cond
			[(and (zero? y) (zero? x)) (void)]
			[(zero? x) (begin (when (= 0 (modulo y 15))
					    (debug-print main 'y y))
					  (loop width (1- y)))]
			[else (loop (1- x) y)])))
		   (send memory-dc end-set-pixel)
		   memory-dc))]

	      [memory-dcs1
	       '(list->vector
		(reverse
		 (cons dest-mem-dc
		       (let loop ([i 1])
			 (if (= i (- total 1)) (list src-mem-dc)
			     (cons (build-w (/ i (1- total)))
				   (loop (1+ i))))))))]

	      [memory-dcs2
	       '(list->vector
		 (let loop ([i total])
		   (if (zero? i)
		       (list (build-w (/ i (1- total))))
		       (cons (build-w (/ i (1- total)))
			     (loop (1- i))))))]

	      [sub-pager%
	       (make-class pager%
		 (public
		  [width dummy-name-width]
		  [picture-height height]
		  [memory-dcs
		   (list->vector
		    (reverse
		     (cons dest-mem-dc
			   (let loop ([i 1])
			     (if (= i (- total 1)) (list src-mem-dc)
				 (cons (build-w (/ i (1- total)))
				       (loop (1+ i))))))))])
		 (lambda ()
		   (super-init)))])
      '(debug-print main memory-dcs)
      (send src-2dvec set-default 1)
      (send dest-2dvec set-default 1)
      (mred:show-busy-cursor
       (lambda ()
	 (make-object sub-pager%))))))

