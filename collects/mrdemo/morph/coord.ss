;;; this file sets up coordinates 

;; bary represents just a three tuple of coordinates, not necessarily
;; with respect to some triangle.
(define-struct bary (a b c))
(define show-bary
  (let ([shorten (lambda (x) (/ (floor (* x 100)) 100))])
    (lambda (p)
      (printf "#<str:bary ~s ~s ~s>"
	      (shorten (bary-a p))
	      (shorten (bary-b p))
	      (shorten (bary-c p))))))

(define-struct posn (x y))
(define show-posn 
  (let ([shorten (lambda (x) (/ (floor (* x 10)) 10))])
    (lambda (p)
      (printf "#<str:posn ~s ~s>"
	      (shorten (posn-x p))
	      (shorten (posn-y p))))))

(define-struct tri (a b c area))
(define build-tri
  (lambda (a b c)
    (make-tri a b c (find-area a b c))))

(define posn-minus
  (lambda (p q)
    (make-posn (- (posn-x p) (posn-x q))
	       (- (posn-y p) (posn-y q)))))

(define bary-minus
  (lambda (p1 p2)
    (make-bary (- (bary-a p1) (bary-a p2))
	       (- (bary-b p1) (bary-b p2))
	       (- (bary-c p1) (bary-c p2)))))

(define square (lambda (x) (* x x)))

(define norm
  (lambda (b)
    (+ (square (bary-a b))
       (square (bary-b b))
       (square (bary-c a)))))

(define find-area
  (let ([helper
	 (lambda (p q)
	   (let ([sroot (- (* (posn-y p) (posn-x q))
			   (* (posn-x p) (posn-y q)))])
	     (/ (abs sroot) 2)))])
    (lambda (p1 p2 p3)
      (helper (posn-minus p1 p2) (posn-minus p1 p3)))))

(define find-barycentric
  (lambda (p1 p2 p3 p)
    (find-barycentric-area (build-tri p1 p2 p3) p)))

(define find-barycentric-area
  (lambda (tri p)
    (let* ([p1 (tri-a tri)]
	   [p2 (tri-b tri)]
	   [p3 (tri-c tri)]
	   [area (tri-area tri)]
	   [little-area/big-area
	    (lambda (p q)
	      (let ([sroot (- (* (posn-y p) (posn-x q))
			      (* (posn-x p) (posn-y q)))])
		(/ sroot area 2)))]
	   [p1-p (posn-minus p1 p)]
	   [p2-p (posn-minus p2 p)]
	   [p3-p (posn-minus p3 p)])
      (make-bary (little-area/big-area p2-p p3-p)
		 (little-area/big-area p3-p p1-p)
		 (little-area/big-area p1-p p2-p)))))

(define find-euclid
  (lambda (tri b)
    (let ([p1 (tri-a tri)]
	  [p2 (tri-b tri)]
	  [p3 (tri-c tri)]
	  [a (bary-a b)]
	  [b (bary-b b)]
	  [c (bary-c b)])
      (make-posn (+ (* a (posn-x p1))
		    (* b (posn-x p2))
		    (* c (posn-x p3)))
		 (+ (* a (posn-y p1))
		    (* b (posn-y p2))
		    (* c (posn-y p3)))))))

(define distance-sq
  (lambda (p q)
    (let ([x-delta (- (posn-x p) (posn-x q))]
	  [y-delta (- (posn-y p) (posn-y q))])
      (+ (* x-delta x-delta) (* y-delta y-delta)))))
