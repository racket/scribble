(define 2d-vector%
  (make-class ()
    (private
     [default? #f]
     [default (void)]
     [vec (void)]
     [found-error
      (lambda (x w h)
	(error '2d-vector% "~s out of bounds: ~s ~s; array size ~sx~s"
	       x w h width height))]
     [index (lambda (w h) (+ (* h width) w))])
    (public
     [height (void)]
     [width (void)]
     [set-default
      (lambda (d)
	(set! default? #t)
	(set! default d))]
     [no-default (lambda () (set! default? #f))]
     [straighten (lambda () vec)]
     [bend (lambda (list)
	     (when (not (list? list))
	       (error '2d-vector%
		      "bend expected a list, found: ~s" list))
	     (when (not (= (length list) (* height width)))
	       (error '2d-vector%
		      (string-append "attempted to bend a list of length ~s,"
				     " should have length ~s")
		      (length list) (* height width)))
	     (set! vec (list->vector list)))]
     [update (lambda (w h v)
	       (let ([w (floor w)]
		     [h (floor h)])
		 (debug-print 2d-vector% 'width width 'height height w h
			      'index (index w h))
		 (if (and (<= 0 w) (< w width) (<= 0 h) (< h height))
		     (vector-set! vec (index w h) v)
		     (found-error 'update w h))))]
     [lookup (lambda (w h)
	       (let ([w (floor w)]
		     [h (floor h)])
		 (debug-print 2d-vector% 'width width 'height height w h
			      'index (index w h))
		 (if (and (<= 0 w) (< w width) (<= 0 h) (< h height))
		     (vector-ref vec (index w h))
		     (if default?
			 default
			 (found-error 'lookup w h)))))])
    (lambda (w h v)
      (if (and (integer? w) (integer? h))
	  (begin 
	    (set! width w)
	    (set! height h)
	    (set! vec (make-vector (* w h) v)))
	  (error '2d-vector%
		 "received w = ~s and h = ~s; both must should be integral" 
		 w h)))))