(define num-colors 200)
(define mesh-color (make-object wx:colour% 50 155 50))
(define mesh-pen (make-object wx:pen% mesh-color 1 wx:const-solid))

(define black-pen (make-object wx:pen% "BLACK" 1 wx:const-solid))

(define colors
  (tabulate num-colors
	    (lambda (i)
	      (make-object wx:colour%
			   (* 255 (expt (/ i (1- num-colors)) 1/2))
			   (* 255 (/ i (1- num-colors)))
			   (* 255 (expt (/ i (1- num-colors)) 1/2))))))

(define mesh%
  (make-class mred:menu-frame%
    (rename [super-make-menu-bar make-menu-bar]
;	    [super-on-paint on-paint]
	    )
    (inherit make-menu show panel)
    (public
     [mesh #f]
     
     [filename #f]
     [margin 10]

     [on-paint
      (lambda ()
	(send left-canvas on-paint)
	(send right-canvas on-paint))]

     [canvas%
      (make-class mred:canvas%
	(inherit set-background get-dc clear get-client-size)
	(rename [super-on-event on-event])
	(private
	 [w-brush (make-object wx:brush% "white" wx:const-solid)])
        (public
	 [node-value-get (void)]
	 [node-value-update (void)]
	 [memory-dc (make-object wx:memory-dc%)]
	 [set-memory-dc! (lambda (mdc) (set! memory-dc mdc))]
	 [draw-line (void)]
	 [dc (void)]
	 [on-event
	  (let* ([dragging-node (void)]
		 [connections null]
		 [orig-x (void)]
		 [orig-y (void)]
		 [last-x -1]
		 [last-y -1]
		 [lines (lambda ()
			  (for-each
			   (lambda (x)
			     (let ([p (node-value-get (graph:value x))])
			       (draw-line (posn-x p) (posn-y p)
					  last-x last-y)))
			   connections))])
	    (lambda (event)
	      (cond
	       [(send event dragging?)
		(let ([x (send event get-x)]
		      [y (send event get-y)])
		  (when (and (not (and (= x last-x)
				       (= x last-y)))
			     '(inside-triangles dragging-node x y))
		    (lines)
		    (set! last-x x)
		    (set! last-y y)
		    (lines)))]
		[(send event button-down?)
		 (let ([x (send event get-x)]
		       [y (send event get-y)])
		   (send dc set-logical-function wx:const-xor)
		   (when mesh
			 (set! dragging-node (find-closest mesh x y node-value-get))
			 (set! connections (graph:connections dragging-node)))
		   (set! last-x x)
		   (set! last-y y)
		   (set! orig-x x)
		   (set! orig-y y)
		   (lines))]
		[(send event button-up?)
		 (lines)
		 (send dc set-logical-function wx:const-copy)
		 (unless (void? dragging-node)
			 (node-value-update (graph:value dragging-node)
					    (make-posn last-x last-y)))
		 (let* ([posns (cons (make-posn last-x last-y)
				     (map (lambda (x)
					    (node-value-get (graph:value x)))
					  connections))]
			[box-width (box 0)]
			[box-height (box 0)]
			[min-x orig-x]
			[min-y orig-y]
			[max-x orig-x]
			[max-y orig-y])
		   (for-each (lambda (p)
			       (let ([x (posn-x p)]
				     [y (posn-y p)])
				 (when (< max-x x) (set! max-x x))
				 (when (< max-y y) (set! max-y y))
				 (when (< x min-x) (set! min-x x))
				 (when (< y min-y) (set! min-y y))))
			     posns)
		   '(send dc set-clipping-region min-x min-y
			 (1+ (- max-x min-x))
			 (1+ (- max-y min-y)))
		   (on-paint)
		   (get-client-size box-width box-height)
		   (send dc set-clipping-region
			 0 0 (unbox box-width) (unbox box-height)))
		 (set! dragging-node (void))
		 (set! connections (void))
		 (set! orig-x (void))
		 (set! orig-y (void))
		 (set! last-x (void))
		 (set! last-y (void))]
		[else (super-on-event event)])))]
	 [on-paint
	  (lambda ()
	    (let* ([get-points
		    (lambda (node)
		      (let* ([value (graph:value node)]
			     [p (node-value-get value)])
			(values (posn-x p)(posn-y p))))]
		   [traverse
		    (lambda (node)
		      (let-values ([(b-x b-y) (get-points node)])
			(let* ([children (graph:children node)]
			       [chili
				(lambda (child)
				  (let-values ([(e-x e-y) (get-points child)])
				    (draw-line b-x b-y e-x e-y)))])
			  (for-each chili children))))]
		   [dc (get-dc)]
		   [by (box 0)]
		   [bx (box 0)])
	      (get-client-size bx by)
	      (set-background w-brush)
	      (clear)
	      (send dc set-pen black-pen)
	      (send dc blit 0 0 (unbox bx) (unbox by)
		    memory-dc 0 0 wx:const-copy)
	      (when mesh
		(send dc set-pen mesh-pen)
		'(send dc set-logical-function wx:const-xor)
		(graph:traverse mesh traverse)
		'(send dc set-logical-function wx:const-copy))))])
	(lambda (get update . args)
	  (apply super-init args)
	  (set! dc (get-dc))
	  (set! draw-line (ivar dc draw-line))
	  (set! node-value-get get)
	  (set! node-value-update update)))]

     [left-filename #f]
     [right-filename #f]
     [left-2dvec #f]
     [right-2dvec #f]
     [left-canvas #f]
     [right-canvas #f]
     [default-size 200]

     [canvas-size-diff 0]

     [get-sizes
      (lambda ()
	(let* ([left-width (if left-2dvec
			       (ivar left-2dvec width)
			       default-size)]
	       [left-height (if left-2dvec
				(ivar left-2dvec height)
				default-size)]
	       [right-width (if right-2dvec
				(ivar right-2dvec width)
				default-size)]
	       [right-height (if right-2dvec
				 (ivar right-2dvec height)
				 default-size)])
	  (values left-width left-height right-width right-height)))]
     [resize-panels
      (lambda ()
	(let-values ([(left-width left-height right-width right-height)
		      (get-sizes)])
	  (send left-canvas user-min-width (+ canvas-size-diff left-width))
	  (send left-canvas user-min-height (+ canvas-size-diff left-height))
	  (send right-canvas user-min-width (+ canvas-size-diff right-width))
	  (send right-canvas user-min-height (+ canvas-size-diff right-height))))]
     [open
      (lambda (fn)
	(mred:show-busy-cursor
	 (lambda ()
	   (let* ([port (open-input-file fn)]
		  [local-mesh #f]
		  [read-value
		   (lambda (port)
		     (let ([left (read port)]
			   [right (read port)])
		       (cons (apply make-posn left)
			     (apply make-posn right))))])
	     (set! left-filename (read port))
	     (set! right-filename (read port))
	     (when (read port)
		 (set! local-mesh (graph:read read-value port)))
	     (close-input-port port)
	     (debug-print mesh% 'read 'file)
	     (set! left-2dvec (build-image left-filename))
	     (set! right-2dvec (build-image right-filename))
	     (debug-print mesh% 'built-2dvecs)
	     (send left-canvas set-memory-dc! (build-memory-dc left-2dvec))
	     (send right-canvas set-memory-dc! (build-memory-dc right-2dvec))
	     (debug-print mesh% 'updated 'canvases)
	     (set! mesh local-mesh)
	     (set! filename fn)
	     (resize-panels)
	     (on-paint)))))]
     [save
      (lambda ()
	(if filename
	    (begin
	      (let ([port (open-output-file filename 'replace)]
		    [print-value
		     (lambda (port v)
		       (let ([left (car v)]
			     [right (cdr v)])
			 (fprintf port "~s ~s"
				  (list (posn-x left) (posn-y left))
				  (list (posn-x right) (posn-y right)))))])
		(fprintf port ";;; data req'd to setup a morph~n~n")
		(fprintf port "~s ; source filename~n" left-filename)
		(fprintf port "~s ; destination filename~n" right-filename)
		(fprintf port "~n;; mesh~n")
		(if mesh
		    (begin
		      (fprintf port "#t ; mesh is setup~n")
		      (graph:fprintf port mesh print-value))
		    (fprintf port "#f ; mesh is not setup~n"))
		(close-output-port port)))
	    (save-as)))]
     [save-as
      (lambda ()
	(let ([fn (mred:common-put-file)])
	  (when fn
	    (set! filename fn)
	    (save))))]

     [make-menu-bar
      (lambda ()
	(let ([bar (super-make-menu-bar)]
	      [file-menu (make-menu)]
	      [picture-menu (make-menu)]
	      [mesh-menu (make-menu)]
	      [morph-menu (make-menu)])
	  (send file-menu append-item "Open..."
		(lambda ()
		  (let ([fn (mred:common-get-file)])
		    (when fn (open fn)))))
	  (send file-menu append-item "Save..." save)
	  (send file-menu append-item "Save As..." save-as)
	  (send file-menu append-separator)
	  (send file-menu append-item "Close" (lambda () (show #f)))
	  (send picture-menu append-item "Select Source Image..."
		(lambda () 
		  (let ([fn (mred:common-get-file
			     '() "Please choose an image.")])
		    (when fn
		      (mred:show-busy-cursor
		       (lambda ()
			 (set! left-filename fn)
			 (time (set! left-2dvec (build-image fn)))
			 (time (send left-canvas set-memory-dc!
				     (build-memory-dc left-2dvec)))
			 (set! mesh #f)
			 (resize-panels)
			 (on-paint)))))))
	  (send picture-menu append-item "Select Destination Image..."
		(lambda ()
		  (let ([fn (mred:common-get-file
			     '() "Please choose an image.")])
		    (when fn
		      (mred:show-busy-cursor
		       (lambda ()
			 (set! right-filename fn)
			 (set! right-2dvec (build-image fn))
			 (send right-canvas set-memory-dc!
			       (build-memory-dc right-2dvec))
			 (set! mesh #f)
			 (resize-panels)
			 (on-paint)))))))
	  (send picture-menu append-item "Exchange Images"
		(lambda ()
		  (let ([left-mdc (ivar left-canvas memory-dc)]
			[right-mdc (ivar right-canvas memory-dc)]
			[tmp left-2dvec]
			[exchange
			 (lambda (node)
			   (let* ([value (graph:value node)]
				  [left (car value)])
			     (set-car! value (cdr value))
			     (set-cdr! value left)))])
		    (set! left-2dvec right-2dvec)
		    (set! right-2dvec tmp)
		    (send right-canvas set-memory-dc! left-mdc)
		    (send left-canvas set-memory-dc! right-mdc)
		    (graph:traverse mesh exchange)
		    (resize-panels)
		    (on-paint))))
	  (send mesh-menu append-item "Fresh Mesh..."
		(lambda ()
		  (fresh-mesh (get-number-from-user
			       "How large should the mesh be?"
			       "Mesh Size" "5"))
		  (on-paint)))
	  (send morph-menu append-item "Morph..."
		(lambda () (morph (get-number-from-user
				   "How many steps in the morph?"
				   "Morph Size" "3"))))
	  (send bar append file-menu "File")
	  (send bar append picture-menu "Images")
	  (send bar append mesh-menu "Mesh")
	  (send bar append morph-menu "Morph")
	  bar))]
     [morph
      (lambda (steps)
	(if (and left-2dvec right-2dvec mesh)
	    (main mesh (ivar left-canvas memory-dc)
		  (ivar right-canvas memory-dc)
		  left-2dvec right-2dvec steps)
	    (wx:message-box "Please choose images and mesh.")))]
     [fresh-mesh
      (lambda (n)
	(let-values ([(left-width left-height right-width right-height)
		      (get-sizes)])
	  (let* ([nodes
		  (let loop ([row (1- n)] [column (1- n)])
		    (let ([row-percent (/ row (1- n))]
			  [column-percent (/ column (1- n))])
		      (cons 
		       (graph:node
			(cons (make-posn (* left-width row-percent)
					 (* left-height column-percent))
			      (make-posn (* right-width row-percent)
					 (* right-height column-percent))))
		       (cond
			[(and (= row 0) (= column 0)) null]
			[(= row 0) (loop (1- n) (1- column))]
			[else (loop (1- row) column)]))))]
		 [2dvec (make-object 2d-vector% n n 0)]
		 [lookup (ivar 2dvec lookup)]
		 [update (ivar 2dvec update)])
	    (send 2dvec bend (reverse nodes))

	    (if (< 2 n)
		(begin
		  '(graph:edge (lookup 0 (1- n)) (lookup 1 (1- n)))
		  (let loop ([x (- n 2)] [y (- n 2)])
		    (graph:edge (lookup x y) (lookup x (1+ y)))
		    (graph:edge (lookup x y) (lookup (1+ x) y))
		    (graph:edge (lookup x y) (lookup (1+ x) (1+ y)))
		    (cond [(and (= x 0) (= y 0)) (void)]
			  [(= x 0) (loop (- n 2) (1- y))]
			  [else (loop (1- x) y)]))
		  (let loop ([i (- n 2)])
		    (when (<= 0 i)
		      (graph:edge (lookup (1- n) i) (lookup (1- n) (1+ i)))
		      (graph:edge (lookup i (1- n)) (lookup (1+ i) (1- n)))
		      (loop (1- i)))))
		(begin
		  (graph:edge (lookup 0 0) (lookup 0 1))
		  (graph:edge (lookup 0 0) (lookup 1 0))
		  (graph:edge (lookup 0 0) (lookup 1 1))
		  (graph:edge (lookup 1 0) (lookup 1 1))
		  (graph:edge (lookup 0 1) (lookup 1 1))))

	    (set! mesh (lookup 0 0)))))])

    (lambda ()
      (super-init '() "Mesh Construction")
      (send panel stretchable-in-x #f)
      (send panel stretchable-in-y #f)
      (let ([p (make-object mred:horizontal-panel% panel)]
	    [init
	     (lambda (c)
	       (send c user-min-height default-size)
	       (send c user-min-width default-size)
	       (send c stretchable-in-x #f)
	       (send c stretchable-in-y #f))])
	(send p stretchable-in-x #f)
	(send p stretchable-in-y #f)
	(set! left-canvas (make-object canvas% car set-car! p))
	(init left-canvas)
	(set! right-canvas (make-object canvas% cdr set-cdr! p))
	(init right-canvas))
      (let ([w (box 0)]
	    [h (box 0)])
	(send right-canvas get-client-size w h)
	(set! canvas-size-diff (- (send right-canvas get-width) (unbox w))))
      (show #t))))

(define mesh
  (begin (when (defined? 'mesh)
	   (send mesh show #f))
	 (make-object mesh%)))
'(send mesh open "/home/robby/class/graphics/lab5/test/rd.morph")
'(send mesh morph 3)
