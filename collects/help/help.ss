
(require-library "browser.ss" "browser")

(define f (make-object frame% "PLT Help" #f 400 400))
(define top (make-object vertical-pane% f))
(define results (make-object editor-canvas% f))

(define t (make-object text-field% "Help on:" top void))
(define search-pane (make-object horizontal-pane% top))
(define button (make-object button% "Search" search-pane (lambda (b e) (start-search)) '(border)))
(define where (make-object choice% #f '("for Keyword"
					"for Keyword or Index Entry"
					"for Keyword, Index Entry, or Text")
			   search-pane
			   void))
(define exact (make-object check-box% "Exact" search-pane void))

(define editor (make-object text%))
(send results set-editor editor)

(send top stretchable-height #f)

(send t focus)

(send f show #t)

(define link-delta (make-object style-delta% 'change-underline #t))
(let ([mult (send link-delta get-foreground-mult)]
      [add (send link-delta get-foreground-add)])
  (send mult set 0 0 0)
  (send add set 0 0 255))

(define (add-choice type name title page label)
  (send editor insert (format "~a " type) (send editor last-position))
  (let ([start (send editor last-position)])
    (send editor begin-edit-sequence)
    (send editor insert (format "~a in ~s~n" name title) start)
    (let ([end (sub1 (send editor last-position))])
      (send editor change-style link-delta start end)
      (send editor set-clickback start end
	    (lambda (edit start end)
	      (open-url (make-url
			 "file"
			 #f			; host
			 #f			; port
			 page
			 #f			; params
			 #f			; query
			 label)))))
    (send editor end-edit-sequence)))

(define re:iname "A HREF=\"(node[0-9]*[.]htm)\"><IMG SRC=\"../icons/index.gif")
(define re:ientry "<DT><TT>([^<]*)</TT>")
(define re:ilink "<A HREF=\"(node[0-9]*[.]htm)#([0-9]*)\">([^<]*)</A>(.*)")

(define indices (make-hash-table))

(define (load-index doc)
  (hash-table-get
   indices
   (string->symbol doc)
   (lambda ()
     (let ([index-file (with-handlers ([(lambda (x) #t) (lambda (x) #f)])
			 (with-input-from-file (build-path doc "index.htm")
			   (lambda ()
			     (let loop ()
			       (let ([r (read-line)])
				 (cond
				  [(eof-object? r) #f]
				  [(regexp-match re:iname r)
				   =>
				   (lambda (m) (cadr m))]
				  [else (loop)]))))))])
       (let ([index 
	      (and index-file
		   (with-handlers ([(lambda (x) #f) (lambda (x) #f)])
		     (with-input-from-file (build-path doc index-file)
		       (lambda ()
			 (let loop ()
			   (let ([r (read-line)])
			     (if (eof-object? r)
				 null
				 (let ([m (regexp-match re:ientry r)])
				   (if m
				       (cons (cons (cadr m) r) (loop))
				       (loop))))))))))])
	 (hash-table-put! indices (string->symbol doc) index)
	 index)))))

(define (start-search)
  (let ([find (send t get-value)]
	[search-level (send where get-selection)]
	[exact? (send exact get-value)])
    (dynamic-wind
     (lambda ()
       (send button enable #f)
       (send where enable #f)
       (send exact enable #f))
     (lambda ()
       (send editor erase)
       (let* ([path (collection-path "doc")]
	      [docs (map (lambda (x) (build-path path x)) (directory-list path))])
	 ;; Keyword searches
	 (for-each
	  (lambda (doc)
	    (let ([keywords (build-path doc "keywords")])
	      (when (file-exists? keywords)
		(let ([keys (with-input-from-file keywords read)]
		      [add-key-choice (lambda (v)
					(add-choice
					 "key" (cadr v) (list-ref v 4)
					 (build-path doc (list-ref v 2))
					 (list-ref v 3)))])
		  (let ([v (assoc find keys)])
		    (when v (add-key-choice v)))
		  (unless exact?
		    (for-each
		     (lambda (v)
		       (when (regexp-match find (car v))
			 (unless (string=? find (car v))
			   (add-key-choice v))))
		     keys))))))
	  docs)
	 ;; Index searches
	 (unless (< search-level 1)
	   (for-each
	    (lambda (doc)
	      (let ([index (load-index doc)]
		    [add-index-choice (lambda (name desc)
					(let loop ([desc desc])
					  (let ([m (regexp-match re:ilink desc)])
					    (when m
					      (add-choice "idx" name
							  (list-ref m 3)
							  (build-path doc (list-ref m 1))
							  (list-ref m 2))
					      (loop (list-ref m 4))))))])
		(when index
		  (let ([v (assoc find index)])
		    (when v (add-index-choice (car v) (cdr v))))
		  (unless exact?
		    (for-each
		     (lambda (v)
		       (when (regexp-match find (car v))
			 (unless (string=? find (car v))
			   (add-index-choice (car v) (cdr v)))))
		     index)))))
	    docs)))
       (when (zero? (send editor last-position))
	 (send editor insert (format "Found nothing for ~a." find))))
     (lambda ()
       (send button enable #t)
       (send where enable #t)
       (send exact enable #t)))))

(yield (make-semaphore 0))
