(print-struct #t)
(print-graph #t)
(define last
  (lambda (x)
    (cond
     [(null? x) (error 'last "empty list")]
     [(null? (cdr x)) (car x)]
     [else (last (cdr x))])))

(define foldl2
  (lambda (f init l)
    (letrec ([helper
	      (lambda (l sofar)
		(cond
		 [(null? l) sofar]
		 [else (helper (cdr l) (f (car l) sofar))]))])
      (helper l init))))

(define tabulate
  (lambda (n f)
    (letrec ([build (lambda (i)
		      (cond
		       [(= i n) null]
		       [else (cons (f i) (build (1+ i)))]))])
      (list->vector (build 0)))))

(define vector-for-each
  (lambda (f v)
    (let ([size (vector-length v)])
      (let loop ([n 0])
	(when (< n size)
	  (f i (vector-ref v i))
	  (loop))))))

'(define remove
  (lambda (list f)
    (letrec ([helper
	      (lambda (l)
		(cond [(null? l) null]
		      [(f (car l)) (helper (cdr l))]
		      [else (cons (car l) (helper (cdr l)))]))])
      (helper list))))

(define-macro package
  (lambda (x . args)
    (car 'package)))
