;; graph:node takes a value to be stored on a node and creates a new node.
;; graph:edge takes two edges and joins them
;; graph:value takes a node and returns it's value

(define-values (graph:node
		graph:edge
		graph:value
		graph:children
		graph:parents
		graph:connections
		graph:traverse
		graph:fprintf
		graph:read
		graph:id
		find-closest)
  (local
    [(define counter 0)
     
     (define-struct node (value children parents visited? id))
     
     (define graph:id node-id)
     
     (define graph:node
       (lambda (value)
	 (make-node value null null #f
		    (begin0 counter
			    (set! counter (add1 counter))))))
     ; from n1 to n2. n1 is n2's parent
     (define graph:edge
       (lambda (n1 n2)
	 (set-node-children! n1 (cons n2 (node-children n1)))
	 (set-node-parents! n2 (cons n1 (node-parents n2)))))
     (define graph:value node-value)
     (define graph:children node-children)
     (define graph:parents node-parents)
     (define graph:connections (lambda (x)
				 (append (graph:parents x)
					 (graph:children x))))
     (define graph:traverse
       (lambda (node f)
	 (unless (node? node)
	   (error 'graph:traverse "expected a node, found ~s" node))
	 (letrec ([to-reset null]
		  [helper
		   (lambda (node)
		     (when (not (node-visited? node))
		       (f node)
		       (set! to-reset (cons node to-reset))
		       (set-node-visited?! node #t)
		       (for-each helper (node-children node))))])
	   (helper node)
	   (for-each (lambda (node)
		       (set-node-visited?! node #f))
		     to-reset))))
     
     (define graph:fprintf
       (lambda (port node fprintf-node-value)
	 (unless (node? node)
	   (error 'graph:fprintf "expected a node, found ~s" node))
	 (let ([min (node-id node)] [max (node-id node)])
	   (graph:traverse
	    node (lambda (x)
		   (when (< max (node-id x))
		     (set! max (node-id x)))
		   (when (< (node-id x) min)
		     (set! min (node-id x)))))
	   (fprintf port "~s ; min~n~s ; max~n; nodes~n" min max))
	 (graph:traverse
	  node
	  (lambda (node)
	    (fprintf port "~s	~s	~s	" (node-id node)
		     (map node-id (node-children node))
		     (map node-id (node-parents node)))
	    (fprintf-node-value port (node-value node))
	    (fprintf port "~n")))
	 (fprintf port "~s~n" 'double-check)
	 (fprintf port "~s~n" (node-id node))))
     
     (define graph:read
       (opt-lambda (read-value [port (current-input-port)])
	 (let* ([min (read port)]
		[max (read port)]
		[v (tabulate (add1 (- max min)) (lambda (x)
						(graph:node x)))])
	   (let loop ([i (- max min)])
	     (when (<= 0 i)
	       (let* ([index (- (read port) min)]
		      [node (vector-ref v index)]
		      [chili (lambda (i) (vector-ref v (- i min)))]
		      [children (read port)]
		      [parents (read port)])
		 (set-node-children! node (map chili children))
		 (set-node-parents! node (map chili parents))
		 (set-node-value! node (read-value port))
		 (debug-print graph (+ index min) 'children children
			      'parents parents
			      'node node))
	       (loop (sub1 i))))
	   (when (not (eq? 'double-check (read port)))
	     (error 'graph:read "input corrupted"))
	   (let ([node-count (read port)])
	     (debug-print graph 'important-node node-count
			  'min min
			  (vector-ref v (- node-count min)))
	     (vector-ref v (- node-count min))))))
     
     (define find-closest
       (lambda (node x y node-value-get)
	 (let* ([mouse-posn (make-posn x y)]
		[closest node]
		[value (graph:value node)]
		[posn (node-value-get value)]
		[current-dist-sq (distance-sq mouse-posn posn)])
	   (graph:traverse node
			   (lambda (node)
			     (let* ([value (graph:value node)]
				    [this-posn (node-value-get value)]
				    [this-dist-sq
				     (distance-sq mouse-posn this-posn)])
			       (when (< this-dist-sq current-dist-sq)
				 (set! posn this-posn)
				 (set! current-dist-sq this-dist-sq)
				 (set! closest node)))))
	   closest)))]
         


     (values graph:node
	     graph:edge
	     graph:value
	     graph:children
	     graph:parents
	     graph:connections
	     graph:traverse
	     graph:fprintf
	     graph:read
	     graph:id
	     find-closest)))