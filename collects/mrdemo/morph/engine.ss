
;; a mesh is a graph of the connectivities of the points. 
(define-struct mesh (index children))

;; an image is a 2d array of color values, all of which are between
;; one and zero.


;; input:
;; src-verticies: the positions in the mesh where the
;;                verticies are in the src image.
;; src-img: the source image
;; dest-verticies: the positions in the mesh where the
;;                 verticies are in the dest image.
;; src-img: the destination image
;; w: a constant between zero and one indicating how far the morph between
;;    the images. 
;; 
;; It returns a function which computes the color value for the pixel
;; x and y, which should be a number between one and zero.



(define engine-simple
  (lambda (mesh src-lookup dest-lookup w)
    (lambda (x y)
      (let ([scale-pt (+ (* w (src-lookup x y))
			 (* (- 1 w) (dest-lookup x y)))]
	    [x (- (/ (random 101) 400) 1/8)])
	(max 0 (min 1 (+ scale-pt x)))))))

(define ormap-count 'uhoh)

(define ormap-debug
  (lambda (f list)
    (letrec ([helper
	      (lambda (i l)
		(cond
		 [(null? l) (set! ormap-count 'not-there)
			    #f]
		 [else (let ([w (f (car l))])
			 (if w
			     (begin
			       (set! ormap-count (cons i w))
			       w)
			     (helper (1+ i) (cdr l))))]))])
      (helper 1 list))))

(define engine
  (lambda (mesh src-lookup dest-lookup w)
    (let* ([tmp-triangles (build-triangles w mesh)]
	   [triangles (cons (car tmp-triangles) tmp-triangles)]
	   [1-w (- 1 w)])
      (lambda (x y)
	(let* ([bc-tri (ormap (point-in-triangle? (make-posn x y)) triangles)])
	  (if bc-tri
	      (let* ([bc (car bc-tri)]
		     [triangle-triple (cdr bc-tri)]
		     [to-p (find-euclid (triangles-to triangle-triple) bc)]
		     [to-x (posn-x to-p)]
		     [to-y (posn-y to-p)]
		     [from-p (find-euclid (triangles-from triangle-triple) bc)]
		     [from-x (posn-x from-p)]
		     [from-y (posn-y from-p)])
		(set-car! triangles triangle-triple)
		(values (+ (* w from-x) (* 1-w to-x))
			(+ (* w from-y) (* 1-w to-y))
			(+ (* w (src-lookup from-x from-y))
			   (* 1-w (dest-lookup to-x to-y)))))
	      (values x
		      y
		      (begin '(/ (+ (src-lookup x y) (dest-lookup x y)) 2)
			     1))))))))

'(define engine engine-simple)

(define get-points
  (lambda (node)
    (let ([value (graph:value node)])
      (values (car value) (cdr value)))))

;; this returns either #f or a pair, the triangle and the barycentric
;; coordinates of (x,y) with respect to that triangle.
(define point-in-triangle?
  (lambda (posn)
    (lambda (triangle-triple)
      (let* ([intermediate-triangle (triangles-intermediate triangle-triple)]
	     [bary (find-barycentric-area intermediate-triangle posn)])
	(if (and (<= 0 (bary-a bary))
		 (<= 0 (bary-b bary))
		 (<= 0 (bary-c bary)))
	    (cons bary triangle-triple)
	    #f)))))

;; This maps over a list pairwise, e.g.
;; (for-each-pairwise (list 1 2 3) f)
;; =
;; (begin (f 1 2) (f 2 3) (f 3 1))

(define for-each-pairwise
  (lambda (l f)
    (cond
     [(<= (length l) 1) (void)]
     [(= 2 (length l)) (f (first l) (second l))]
     [else (letrec ([first-ele (car l)]
		    [helper 
		     (lambda (l)
		       (cond 
			[(null? (cdr l)) (begin '(f (first l) first-ele)
						(void))]
			[else (f (first l) (second l))
			      (helper (cdr l))]))])
	     (helper l))])))

(define-struct triangles (from intermediate to))

(define build-triangles
  (lambda (w mesh)
    (let* ([triangles null]
	   [1-w (- 1 w)]
	   [combine
	    (lambda (p q)
	      (make-posn (+ (* w (posn-x p)) (* 1-w (posn-x q)))
			 (+ (* w (posn-y p)) (* 1-w (posn-y q)))))])
      (graph:traverse
       mesh
       (lambda (node)
	 (let-values ([(left right) (get-points node)])
	   (let ([children (graph:children node)])
	     (when (= (length children) 3)
	       (let ([one (first children)]
		     [two (second children)]
		     [three (third children)])
		 (let-values ([(left-one right-one) (get-points one)]
			      [(left-two right-two) (get-points two)]
			      [(left-three right-three) (get-points three)])
		   (let* ([int (combine left right)]
			  [int-one (combine left-one right-one)]
			  [int-two (combine left-two right-two)]
			  [int-three (combine left-three right-three)]
			  [left-tri1 (build-tri left left-one left-two)]
			  [int-tri1 (build-tri int int-one int-two)]
			  [right-tri1 (build-tri right right-one right-two)]
			  [left-tri2 (build-tri left left-three left-one)]
			  [int-tri2 (build-tri int int-three int-one)]
			  [right-tri2 (build-tri right right-three right-one)])
		     (set! triangles
			   (list*
			    (make-triangles left-tri1 int-tri1 right-tri1)
			    (make-triangles left-tri2 int-tri2 right-tri2)
			    triangles))))))))))
      (if (null? triangles)
	  (error 'build-triangles "empty mesh")
	  triangles))))
