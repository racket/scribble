
(require-library "browser.ss" "browser")

(define f (make-object (class frame% args
			 (override [on-close exit])
			 (sequence (apply super-init args)))
		       "PLT Help" #f 600 440))
(define html-panel (make-object hyper-panel% f))
(define results (send html-panel get-canvas))

(define top (make-object vertical-pane% f))
(define t (make-object text-field% "Find help on:" top void))
(define search-pane (make-object horizontal-pane% top))
(define button (make-object button% "Search" search-pane (lambda (b e) (set! collecting-thread (thread start-search))) '(border)))
(define where (make-object choice% #f '("for Keyword"
					"for Keyword or Index Entry"
					"for Keyword, Index Entry, or Text")
			   search-pane void))
(define exact (make-object choice% #f '("exact match"
					"containing match"
					"regexp match")
			   search-pane void))
(define stop (make-object button% "Stop" search-pane
			  (lambda (b e)
			    (break-thread collecting-thread))))

(send exact set-selection 1)
(send stop show #f)

(define results-editor% (class hyper-text% ()
			  (inherit set-title)
			  (sequence 
			    (super-init #f)
			    (set-title "Search Results"))))

(send top stretchable-height #f)

(send t focus)

(let* ([mb (make-object menu-bar% f)]
       [file (make-object menu% "&File" mb)]
       [edit (make-object menu% "&Edit" mb)])
  (append-editor-operation-menu-items edit)

  (make-object menu-item% "Quit" file (lambda (i e) (exit)) #\Q))

(send f show #t)

(send results goto-url 
      (string-append "file:" (build-path (collection-path "doc") "start.htm"))
      #f)

(define cycle-key #f)
(define collecting-thread #f)

(define choices-sema (make-semaphore 1))
(define choices null)

(define (add-choice type name title page label ckey)
  (semaphore-wait choices-sema)
  (set! choices (cons (list type name title page label) choices))
  (semaphore-post choices-sema)
  (queue-callback
   (lambda ()
     (when (eq? cycle-key ckey)
       (semaphore-wait choices-sema)
       (let ([l choices]
	     [editor (send results get-editor)])
	 (set! choices null)
	 (semaphore-post choices-sema)
	 (send editor begin-edit-sequence)
	 (for-each
	  (lambda (i)
	    (let-values ([(type name title page label) (apply values i)])
	      (if type
		  (begin
		    (send editor insert " " (send editor last-position) 'same #f)
		    (let ([start (send editor last-position)])
		      (send editor insert name start 'same #f)
		      (let ([end (send editor last-position)])
			(send editor insert (format " in ~s~n" title) end 'same #f)
			(send editor make-link-style start end)
			(send editor set-clickback start end
			      (lambda (edit start end)
				(send results goto-url 
				      (make-url
				       "file"
				       #f ; host
				       #f ; port
				       page
				       #f ; params
				       #f ; query
				       label)
				      #f))))))
		  (begin
		    (send editor insert (format "In ~a:~n" name) (send editor last-position) 'same #f)))))
	  (reverse l))
	 (send editor end-edit-sequence))))
   #f))

(define (add-section name ckey)
  (add-choice #f name #f #f #f ckey))

(define re:iname (regexp "A HREF=\"(node[0-9]*[.]htm)\"><IMG SRC=\"../icons/index.gif"))
(define re:ientry (regexp "<DT>(.*)<DD>"))
(define re:ilink (regexp "(.*)<A HREF=\"(node[0-9]*[.]htm)#([0-9]*)\">(.*)</A>"))

(define indices (make-hash-table))

(define (clean-index-entry s)
  (regexp-replace*
   "<[^>]*>"
   (regexp-replace* 
    "&gt;"
    (regexp-replace*
     "&lt;"
     s
     "<")
    ">")
   ""))

(define (load-index doc)
  (hash-table-get
   indices
   (string->symbol doc)
   (lambda ()
     (let ([index-file (with-handlers ([(lambda (x) (not (exn:misc:user-break? x))) (lambda (x) #f)])
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
		   (with-handlers ([(lambda (x) (not (exn:misc:user-break? x))) 
				    (lambda (x) 
				      ; (printf "~a~n" (exn-message x))
				      #f)])
		     (with-input-from-file (build-path doc index-file)
		       (lambda ()
			 (let loop ()
			   (let ([r (read-line)])
			     (if (eof-object? r)
				 null
				 (let ([m (regexp-match re:ientry r)])
				   (if m
				       (cons (cons (clean-index-entry (cadr m)) r)
					     (loop))
				       (loop))))))))))])
	 (hash-table-put! indices (string->symbol doc) index)
	 index)))))

(define (non-regexp s)
  (list->string
   (apply
    append
    (map
     (lambda (c)
       (if (memq c '(#\$ #\| #\\ #\[ #\] #\. #\* #\? #\+ #\( #\)))
	   (list #\\ c)
	   (list c)))
     (string->list s)))))

(define (start-search)
  (let* ([given-find (send t get-value)]
	 [find (let ([s given-find])
		 (case (send exact get-selection)
		   [(0) s]
		   [(1) (regexp (non-regexp s))] ; substring (not regexp) match
		   [else (regexp s)]))]
	 [search-level (send where get-selection)]
	 [regexp? (= 2 (send exact get-selection))]
	 [exact? (= 0 (send exact get-selection))]
	 [ckey (gensym)]
	 [editor (let ([e (send results get-editor)])
		   (if (is-a? e results-editor%)
		       e
		       (let ([e (make-object results-editor%)])
			 (send results set-page (editor->page e) #t)
			 e)))])
    (dynamic-wind
     (lambda ()
       (begin-busy-cursor)
       (send html-panel enable #f)
       (send button enable #f)
       (send where enable #f)
       (send exact enable #f)
       (set! cycle-key ckey))
     (lambda ()
       (with-handlers ([exn:misc:user-break?
			(lambda (x)
			  (queue-callback
			   (lambda ()
			     (when (eq? cycle-key ckey)
			       (send editor insert "(Search stopped.)" (send editor last-position) 'same #f)))
			   #f))])
	 (send stop show #t)
	 (send editor erase)
	 (let* ([path (collection-path "doc")]
		[doc-names (directory-list path)]
		[docs (map (lambda (x) (build-path path x)) doc-names)])
	   (for-each
	    (lambda (doc doc-name)
	      (define found-one? #f)
	      (define (found)
		(unless found-one?
		  (set! found-one? #t)
		  (add-section doc-name ckey)))
	      ;; Keyword search
	      (let ([keywords (build-path doc "keywords")])
		(when (file-exists? keywords)
		  (let ([keys (with-input-from-file keywords read)]
			[add-key-choice (lambda (v)
					  (found)
					  (add-choice
					   "key" (cadr v) (list-ref v 4)
					   (build-path doc (list-ref v 2))
					   (list-ref v 3)
					   ckey))])
		    (unless regexp?
		      (for-each
		       (lambda (v)
			 (when (string=? given-find (car v))
			   (add-key-choice v)))
		       keys))
		    (unless exact?
		      (for-each
		       (lambda (v)
			 (when (regexp-match find (car v))
			   (unless (and (not regexp?) (string=? given-find (car v)))
			     (add-key-choice v))))
		       keys)))))
	      ;; Index search
	      (unless (< search-level 1)
		(let ([index (load-index doc)]
		      [add-index-choice (lambda (name desc)
					  (let loop ([desc desc])
					    (let ([m (regexp-match re:ilink desc)])
					      (when m
						(loop (list-ref m 1))
						(found)
						(add-choice "idx" name
							    (clean-index-entry (list-ref m 4))
							    (build-path doc (list-ref m 2))
							    (clean-index-entry (list-ref m 3))
							    ckey)))))])
		  (when index
		    (unless regexp?
		      (for-each
		       (lambda (v)
			 (when (string=? given-find (car v))
			   (add-index-choice (car v) (cdr v))))
		       index))
		    (unless exact?
		      (for-each
		       (lambda (v)
			 (when (regexp-match find (car v))
			   (unless (and (not regexp?) (string=? given-find (car v)))
			     (add-index-choice (car v) (cdr v)))))
		       index))))))
	    docs doc-names))
	 (queue-callback
	  (lambda ()
	    (when (eq? cycle-key ckey)
	      (when (zero? (send editor last-position))
		(send editor insert (format "Found nothing for \"~a\"." given-find)))))
	  #f)))
     (lambda ()
       (send stop show #f)
       (send html-panel enable #t)
       (send button enable #t)
       (send where enable #t)
       (send exact enable #t)
       (end-busy-cursor)))))

(yield (make-semaphore 0))
