(define pager%
  (make-class ()
    (public
     [x-pos 650]
     [y-pos 60]
     [width 300]
     [picture-height 300]
     [slider-height 50]
     [button-width 30]
     [slider-width (- width button-width)]
     [memory-dcs (vector)]
     [save-images
      (lambda (fn)
	(let loop ([i (1- (vector-length memory-dcs))])
	  (when (<= 0 i)
	    (let* ([this-name (string-append
			       fn (number->string (1+ i)) ".pgm")]
		   [port (open-output-file this-name 'replace)]
		   [memory-dc (vector-ref memory-dcs i)]
		   [get-pixel (ivar memory-dc get-pixel)]
		   [color (make-object wx:colour% "white")]
		   [get-colors (ivar color get)]
		   [bgreen (box 0)]
		   [bred (box 0)]
		   [bblue (box 0)])
	      (debug-print pager% 'writing 'image this-name)
	      (fprintf port
		       "P2~n# Robby's Morpher via MrEd~n~s ~s~n255~n"
		       width picture-height)
	      (let loop ([x 0] [y 0])
		(if (get-pixel x y color)
		    (get-colors bred bgreen bblue)
		    (set-box! bgreen 255))
		(fprintf port "~s" (unbox bgreen))
		(fprintf port (if (= x width) "~n" " "))
		(cond [(and (= x (1- width)) (= y (1- picture-height))) (void)]
		      [(= x (1- width)) (loop 0 (1+ y))]
		      [else (loop (1+ x) y)]))
	      (debug-print pager% 'wrote this-name)
	      (close-output-port port))
	    (loop (1- i)))))]
     [frame%
      (make-class mred:menu-frame%
	(rename [super-make-menu-bar make-menu-bar])
	(inherit make-menu show)
	(public
	 [make-menu-bar
	  (lambda ()
	    (let ([bar (super-make-menu-bar)]
		  [file-menu (make-menu)])
	      (send file-menu append-item "Save"
		    (lambda ()
		      (let ([fn (mred:common-put-file '()
				 "Please Specify a prefix for the images.")])
			(debug-print pager% 'fn fn)
			(when fn
			  (debug-print pager% 'fn fn)
			  (save-images fn)))))
	      (send file-menu append-item "Close" (lambda () (show #f)))
	      (send bar append file-menu "File")
	      bar))]))]
     [frame (make-object frame% '() "Morph")]
     [panel (ivar frame panel)]
     [canvas%
      (make-class mred:canvas%
	(inherit clear set-background)
        (public
	 [w-brush (make-object wx:brush% "white" wx:const-solid)]
	 [on-paint
	  (lambda ()
	    '(send dc destroy-clipping-region)
	    '(send dc set-clipping-region 0 0 (1+ width) (1+ picture-height))
	    (set-background w-brush)
	    (clear)
	    (when slider
	      (send dc blit 0 0 width picture-height
		    (vector-ref memory-dcs (send slider get-value))
		    0 0 wx:const-copy)))]))]
     [canvas (make-object canvas% panel)]
     [s-panel (make-object mred:horizontal-panel% panel)]
     [slider
      (let ([show-arg
	     (lambda (s e)
	       (send canvas on-paint))])
	(if (> (vector-length memory-dcs) 1)
	    (make-object mred:slider% s-panel show-arg ""
			 0 0 (sub1 (vector-length memory-dcs)) slider-width)
	    #f))]
     [button-click
      (lambda args
	(mred:show-busy-cursor
	 (lambda ()
	   (let ([old-slider-pos (send slider get-value)])
	     (let loop ([img 0])
	       (when (< img (vector-length memory-dcs))
		 (send slider set-value img)
		 (send canvas on-paint)
		 (wx:flush-display)
		 (sleep 0.5)
		 (loop (1+ img))))
	     '(send slider set-value old-slider-pos)
	     (send canvas on-paint)))))]
     [button
      (when (> (vector-length memory-dcs) 1)
	    (make-object mred:button% s-panel button-click "Play"))]
     [dc (send canvas get-dc)]
     [shutdown (lambda () (send frame show #f))])
    (lambda ()
      (send panel stretchable-in-y #f)
      (let ([w (box 0)]
	    [h (box 0)])
	(send canvas get-client-size w h)
	(let ([diff (- (send canvas get-width) (unbox w))])
	  (send canvas user-min-width (+ diff width))
	  (send canvas user-min-height (+ diff picture-height))))
      (send canvas stretchable-in-x #f)
      (send canvas stretchable-in-y #f)
      (send frame show #t)
      (send canvas on-paint))))

