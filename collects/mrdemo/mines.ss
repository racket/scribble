
(require-library "macro.ss")

(define TILE-HW 24)
(define WIDTH 16)
(define HEIGHT 16)
(define BOMB-COUNT 30)

; Temporary
(define FRDW 12)
(define FRDH 39)
(define PANEL-HEIGHT 50)
(define TIME-WIDTH 600)
(define COUNT-WIDTH 600)

(define DIGIT-COLOR-NAMES
  ; 0th is background; 8th is foreground
  (vector "LIGHT GREY" "BLUE" "GREEN" "RED" "PURPLE" "ORANGE" "YELLOW" "BROWN" "BLACK"))

(define DIGIT-COLORS
  (build-vector 9 (lambda (i)
		    (send wx:the-colour-database find-colour 
			  (vector-ref DIGIT-COLOR-NAMES i)))))

(define BG-COLOR (vector-ref DIGIT-COLORS 0))
(define FG-COLOR (vector-ref DIGIT-COLORS 8))
(define EXPLODE-COLOR (send wx:the-colour-database find-colour "RED"))

(define BG-PEN (send wx:the-pen-list find-or-create-pen BG-COLOR 1 wx:const-solid))
(define FG-PEN (send wx:the-pen-list find-or-create-pen FG-COLOR 1 wx:const-solid))

(define step-while
  (opt-lambda (first test until step f [accum void] [init (void)])
    (let loop ([n first][a init])
      (if (test n until)
	  (loop (step n) (accum a (f n)))
	  a))))

(define tile:plain%
  (class null ()
    (private
      [state 'covered]
      [neighbor-bomb-count 0])
    (public
      [set-state
       (lambda (newstate)
	 (set! state newstate))]
      [get-state
       (lambda ()
	 state)]
      [set-neighbor-bomb-count
       (lambda (c)
	 (set! neighbor-bomb-count c))]
      [get-neighbor-bomb-count
       (lambda ()
	 neighbor-bomb-count)]
      [draw-text-tile
       (lambda (dc x y w h hilite? border? str color)
	 (if border?
	     (send dc set-pen FG-PEN)
	     (send dc set-pen BG-PEN))
	 (send dc draw-rectangle x y w h)
	 (when hilite?
	   (send dc draw-rectangle 
		 (add1 x) (add1 y) 
		 (- w 2) (- h 2)))
	 (when str
	   (if color
	       (send dc set-text-foreground color)
	       (send dc set-text-foreground FG-COLOR))
	   (let ([tw (box 0)][th (box 0)])
	     (send dc get-text-extent str tw th)
	     (send dc draw-text str 
		   (+ x (/ (- w (unbox tw)) 2))
		   (+ y (/ (- h (unbox th)) 2))))))]
      [draw
       (lambda (dc x y w h hilite?)
	 (case state
	   [(covered) (draw-text-tile dc x y w h hilite? #t #f #f)]
	   [(flagged) (draw-text-tile dc x y w h hilite? #t "X" #f)]
	   [(semi-flagged) (draw-text-tile dc x y w h hilite? #t "?" #f)]
	   [(uncovered) (draw-text-tile 
			 dc x y w h #f #f
			 (if (zero? neighbor-bomb-count)
			     #f
			     (number->string neighbor-bomb-count))
			 (vector-ref DIGIT-COLORS neighbor-bomb-count))]))])))

(define tile:bomb%
  (class tile:plain% ()
    (inherit get-state draw-text-tile)
    (rename [super-draw draw])
    (private
      [explode-source? #f])
    (public
      [set-explode-source
       (lambda (s?)
	 (set! explode-source? s?))]
      [draw
       (lambda (dc x y w h hilite?)
	 (if (eq? (get-state) 'uncovered)
	     (draw-text-tile dc x y w h #f #f "*"
			     (and explode-source? EXPLODE-COLOR))
	     (super-draw dc x y w h hilite?)))])
    (sequence
      (super-init))))

(define (get-tile b x y)
  (vector-ref (vector-ref b x) y))

(define (set-tile! b x y t)
  (vector-set! (vector-ref b x) y t))

(define (do-surrounding b x y accum start default f)
  (step-while -1 <= 1 add1
	      (lambda (dx)
		(step-while -1 <= 1 add1
			   (lambda (dy)
			     (if (and (not (and (zero? dx) (zero? dy)))
				      (< -1 (+ x dx) WIDTH)
				      (< -1 (+ y dy) HEIGHT))
				 (f dx dy)
				 default))
			   accum start))
	      accum start))

(define (is-bomb? x)
  (is-a? x tile:bomb%))

(define (count-surrounding-bombs b x y)
  (do-surrounding 
   b x y + 0 0
   (lambda (dx dy)
     (if (is-bomb? (get-tile b (+ x dx) (+ y dy)))
	 1
	 0))))

(define (for-each-tile b f)
  (step-while 0 < WIDTH add1
	      (lambda (x)
		(step-while 0 < HEIGHT add1
			    (lambda (y)
			      (f (get-tile b x y) x y))))))

(define (make-board)
  (let ([b (build-vector WIDTH
			 (lambda (i)
			   (build-vector HEIGHT
					 (lambda (j)
					   (make-object tile:plain%)))))])
    (let loop ([n BOMB-COUNT])
      (unless (zero? n)
	(let rloop ()
	  (let* ([x (random WIDTH)]
		 [y (random HEIGHT)]
		 [t (get-tile b x y)])
	    (if (is-a? t tile:bomb%)
		(rloop)
		(begin
		  (set-tile! b x y (make-object tile:bomb%))
		  (loop (sub1 n))))))))
    (for-each-tile b (lambda (t x y)
		       (send t
			     set-neighbor-bomb-count
			     (count-surrounding-bombs b x y))))
    b))

(define f (make-object mred:frame% null "Minesweeper"))
(define vpanel (make-object mred:vertical-panel% f))

(define ms:canvas%
  (class mred:canvas% args
    (inherit get-dc clear 
	     set-min-width set-min-height 
	     stretchable-in-x stretchable-in-y)
    (private
      [panel (make-object mred:horizontal-panel% vpanel)])
    (sequence
      (send panel stretchable-in-y #f))
    (private
      [lspace (make-object mred:vertical-panel% panel)]
      [time (make-object mred:message% panel "Time: 00000")]
      [lmspace (make-object mred:vertical-panel% panel)]
      [button (make-object mred:button% panel (lambda (b e) (reset)) "Reset")]
      [rmspace (make-object mred:vertical-panel% panel)]
      [count (make-object mred:message% panel "Count: 000")]
      [rspace (make-object mred:vertical-panel% panel)]

      [set-time
       (lambda (t)
	 (send time set-label (string-append "Time: " (number->string t))))]
      [set-count
       (lambda (c)
	 (send count set-label (string-append "Bombs: " (number->string c))))]
      
      [clicking #f]
      [clicking-x 0]
      [clicking-y 0]
      [ready? #t]
      [start-time #f]
      [elapsed-time 0]
      [timer #f]
      [bomb-count BOMB-COUNT]
      [cover-count (* HEIGHT WIDTH)]
      [board null])
    (public
      (stop-timer
       (lambda ()
	 (when timer
	   (send timer stop)
	   (set! timer #f))))
      (start-timer
       (lambda ()
	 (set! start-time (current-seconds))
	 (set! timer
	       (make-object
		(class-asi wx:timer%
		  (public
		    [notify
		     (lambda ()
		       (let ([e (- (current-seconds) start-time)])
			 (when (> e elapsed-time)
			   (set! elapsed-time e)
			   (set-time e))))]))))
	 (send timer start 100 #f)))
      (end-of-game
       (lambda (win?)
	 (stop-timer)
	 (set! ready? #f)
	 (set! start-time #f)
	 (unless win?
	   (show-all-bombs))
	 (set-count BOMB-COUNT)))
      (explode
       (lambda ()
	 (end-of-game #f)))
      (win
       (lambda ()
	 (end-of-game #t)))
      (reset
       (lambda ()
	 (stop-timer)
	 (set! ready? #t)
	 (set! start-time #f)
	 (set! elapsed-time 0)
	 (set! cover-count (* HEIGHT WIDTH))
	 (clear)
	 (set-time 0)
	 (set! bomb-count BOMB-COUNT)
	 (set-count BOMB-COUNT)
	 (set! board (make-board))
	 (on-paint)))
      (show-all-bombs
       (lambda ()
	 (for-each-tile board
			(lambda (t x y)
			  (when (is-bomb? t)
			    (change-state t (send t get-state) 'uncovered #f)
			    (paint-one t x y))))))
      (autoclick-surrounding 
       (lambda (x y)
	 (do-surrounding 
	  board x y void (void) (void)
	  (lambda (dx dy)
	    (let* ([x2 (+ x dx)]
		   [y2 (+ y dy)]
		   [t (get-tile board x2 y2)]
		   [state (send t get-state)]
		   [nc (send t get-neighbor-bomb-count)])
	      (unless (eq? state 'uncovered)
		(change-state t state 'uncovered #t)
		(paint-one t x2 y2)
		(when (zero? nc)
		  (autoclick-surrounding x2 y2))))))))
      (change-state
       (lambda (t old-state new-state update-count?)
	 (send t set-state new-state)
	 (when (and update-count? (not (eq? new-state old-state)))
	   (when (eq? new-state 'uncovered)
	     (set! cover-count (sub1 cover-count)))
	   (when (eq? old-state 'uncovered)
	     (set! cover-count (add1 cover-count)))
	   (when (eq? new-state 'flagged)
	     (set! bomb-count (sub1 bomb-count))
	     (set-count bomb-count))
	   (when (eq? old-state 'flagged)
	     (set! bomb-count (add1 bomb-count))
	     (set-count bomb-count)))))
      (do-select
       (lambda (x y flag?)
	 (let* ([t (get-tile board x y)]
		[state (send t get-state)]
		[new-state
		 (case state
		   [(covered)
		    (if flag? 'flagged 'uncovered)]
		   [(flagged)
		    (if flag? 'semi-flagged state)]
		   [(semi-flagged)
		    (if flag? 'covered 'uncovered)]
		   [else state])]
		[nc (send t get-neighbor-bomb-count)]
		[new-uncover? (and (eq? new-state 'uncovered)
				   (not (eq? state 'uncovered)))]
		[bomb? (is-bomb? t)])
	   (change-state t state new-state #t)
	   (when (and new-uncover? bomb?)
	     (send t set-explode-source #t))
	   (paint-one t x y)
	   (when new-uncover?
	     (if bomb?
		 (explode)
		 (begin
		   (when (zero? nc)
		     (autoclick-surrounding x y))))
	     (when (and ready? (= cover-count BOMB-COUNT))
	       (win))))))
      (on-event
       (lambda (e)
	 (when ready?
	   (unless start-time
	     (when (send e button-down?)
	       (start-timer)))
	   (let* ([x (quotient (inexact->exact (floor (send e get-x))) TILE-HW)]
		  [y (quotient (inexact->exact (floor (send e get-y))) TILE-HW)]
		  [t (if (and (< -1 x WIDTH)
			      (< -1 y HEIGHT))
			 (get-tile board x y)
			 #f)])
	     (cond
	       [(and clicking (or (not (eq? t clicking))
				  (not (or (send e button-up?)
					   (send e dragging?)))))
		(let ([old clicking])
		  (set! clicking #f)
		  (paint-one old clicking-x clicking-y))]
	       [(and t
		     (not (eq? (send t get-state) 'uncovered))
		     (or (send e button-down?)
			 (and (send e dragging?)
			      (= x clicking-x)
			      (= y clicking-y))))
		(set! clicking t)
		(set! clicking-x x)
		(set! clicking-y y)
		(paint-one t x y)]
	       [(send e button-down?) ; click not on a tile
		(set! clicking-x -1)]
	       [(and clicking (send e button-up?))
		(set! clicking #f)
		(do-select x y (send e button-up? 3))]
	       [else 'ok])))))
      (paint-one
       (lambda (t x y)
	 (let ([xloc (* x TILE-HW)]
	       [yloc (* y TILE-HW)])
	   (send t draw dc xloc yloc TILE-HW TILE-HW
		 (eq? t clicking)))))
      (on-paint
       (lambda ()
	 (for-each-tile board (lambda (t x y)
				(paint-one t x y))))))
    (sequence
      (apply super-init args)
      (set-min-width (* TILE-HW WIDTH))
      (set-min-height (* TILE-HW HEIGHT))
      (stretchable-in-x #f)
      (stretchable-in-y #f))
    (private
      [dc (get-dc)])
    (sequence
      (reset)
      (send dc set-text-background BG-COLOR)
      (send dc set-brush (send wx:the-brush-list find-or-create-brush 
			       BG-COLOR wx:const-solid)))))

(define c (make-object ms:canvas% vpanel))

(send f show #t)
