
(require-library "browser.ss" "browser")

(define collecting-thread #f)

(define results-editor% (class hyper-text% ()
			  (inherit set-title)
			  (sequence 
			    (super-init #f)
			    (set-title "Search Results"))))

(define f (make-object (class frame% args
			 (rename [super-on-subwindow-char on-subwindow-char])
			 (override 
			   [on-close exit]
			   [on-subwindow-char 
			    (lambda (w e)
			      (case (send e get-key-code)
				[(prior) (send (send results get-editor) move-position 'up #f 'page) #t]
				[(next) (send (send results get-editor) move-position 'down #f 'page) #t]
				[else (if (and (eq? #\tab (send e get-key-code))
					       (eq? w results))
					  ; Override normal behavior, which is to pass the tab on to
					  ; the edit
					  (if (send e get-shift-down)
					      (send before-results focus)
					      (send search-text focus))
					  (super-on-subwindow-char w e))]))])
			 (sequence (apply super-init args)))
		       "PLT Help Desk" #f 600 440))
(define html-panel (make-object (class hyper-panel% ()
				  (rename [super-leaving-page leaving-page])
				  (public
				    [stop-search
				     (lambda ()
				       (when collecting-thread
					 (semaphore-wait break-sema)
					 (break-thread collecting-thread)
					 (semaphore-post break-sema)))])
				  (override
				    [leaving-page
				     (lambda (page new-page)
				       (unless (is-a? (page->editor new-page) results-editor%)
					 (stop-search))
				       (super-leaving-page page new-page))]
				    [on-navigate stop-search])
				  (sequence (super-init f)))))
(define results (send html-panel get-canvas))

(define before-results
  (let loop ([l (send html-panel get-children)])
    (cond
     [(null? (cdr l)) results]
     [(eq? (cadr l) results) (let loop ([v (car l)])
			       (if (is-a? v area-container<%>)
				   (loop (car (last-pair (send v get-children))))
				   v))]
     [else (loop (cdr l))])))

(define top (make-object vertical-pane% f))
(define search-text (make-object text-field% "Find docs for:" top
				 (lambda (t e)
				   (send search enable (positive? (send (send t get-editor) last-position))))))
(define search-pane (make-object horizontal-pane% top))
(define search (make-object button% "Search" search-pane 
			    (lambda (b e) 
			      (semaphore-wait break-sema) ; protects from too-early break
			      (set! collecting-thread (thread start-search))) 
			    '(border)))
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

(send where set-selection 1)
(send exact set-selection 1)
(send search enable #f)
(send stop show #f)

(send top stretchable-height #f)

(send search-text focus)

(let* ([mb (make-object menu-bar% f)]
       [file (make-object menu% "&File" mb)]
       [edit (make-object menu% "&Edit" mb)])
  (append-editor-operation-menu-items edit)

  (make-object menu-item% "Open URL..." file 
	       (lambda (i e)
		 (letrec ([d (make-object dialog% "Open URL" f 500)]
			  [t (make-object text-field% "URL:" d
					  (lambda (t e)
					    (send ok enable 
						  (positive? (send (send t get-editor) 
								   last-position)))))]
			  [p (make-object horizontal-panel% d)]
			  [browse (make-object button% "Browse..." p
					       (lambda (b e)
						 (let ([f (get-file)])
						   (send t set-value (string-append "file:" f)))))]
			  [spacer (make-object vertical-pane% p)]
			  [ok (make-object button% "Open" p
					   (lambda (b e)
					     (let ([s (send t get-value)])
					       (with-handlers ([void
								(lambda (x)
								  (message-box "Bad URL" 
									       (format "Bad URL: ~a" (exn-message x))
									       d))])
						 (let ([url (string->url s)])
						   (send results goto-url url #f)
						   (send d show #f)))))
					   '(border))]
			  [cancel (make-object button% "Cancel" p 
					       (lambda (b e) (send d show #f)))])
		   (send p set-alignment 'right 'center)
		   (send ok enable #f)
		   (send d center)
		   (send t focus)
		   (send d show #t)))
	       #\O)
  (make-object menu-item% "Quit" file (lambda (i e) (exit)) #\Q))

(send f show #t)

(send results goto-url 
      (string-append "file:" (build-path (collection-path "doc") "index.htm"))
      #f)

(define cycle-key #f)
(define break-sema (make-semaphore 1))

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

(define not-break? (lambda (x) (not (exn:misc:user-break? x))))

; Locate standard HTML documentation
(define-values (std-docs std-doc-names)
  (let* ([path (collection-path "doc")]
	 [doc-names (directory-list path)]
	 [docs (map (lambda (x) (build-path path x)) doc-names)])
    (values docs doc-names)))

; Check collections for doc.txt files:
(define-values (txt-docs txt-doc-names)
  (let loop ([collection-paths (current-library-collection-paths)]
	     [docs null]
	     [names null])
    (cond
     [(null? collection-paths)
      (values docs names)]
     [else (let ([path (car collection-paths)])
	     (let cloop ([l (with-handlers ([void (lambda (x) null)]) (directory-list path))]
			 [docs docs]
			 [names names])
	       (cond
		[(null? l) (loop (cdr collection-paths) docs names)]
		[(and (directory-exists? (build-path path (car l)))
		      (not (member (car l) names))
		      (file-exists? (build-path path (car l) "doc.txt")))
		 (cloop (cdr l) (cons (build-path path (car l)) docs)
			(cons (car l) names))]
		[else (cloop (cdr l) docs names)])))])))

(define docs (append std-docs txt-docs))
(define doc-names (append std-doc-names (map (lambda (s) (format "~a collection" s)) txt-doc-names)))
(define doc-kinds (append (map (lambda (x) 'html) std-docs) (map (lambda (x) 'text) txt-docs)))

(define re:iname (regexp "A HREF=\"(node[0-9]*[.]htm)\"><IMG SRC=\"../icons/index.gif"))
(define re:ientry (regexp "<DT>(.*)<DD>"))
(define re:ilink (regexp "(.*)<A HREF=\"(node[0-9]*[.]htm)#([0-9]*)\">(.*)</A>"))

(define (clean-html s)
  (regexp-replace*
   "&[^;]*;"
   (regexp-replace*
    "<[^>]*>"
    (regexp-replace* 
     "&gt;"
     (regexp-replace*
      "&lt;"
      s
      "<")
     ">")
    "")
   ""))

(define (with-hash-table ht key compute)
  (hash-table-get
   ht
   (string->symbol key)
   (lambda ()
     (let ([v (compute)])
       (hash-table-put! ht (string->symbol key) v)
       v))))

(define html-indices (make-hash-table))
(define (load-html-index doc)
  (with-hash-table
   html-indices
   doc
   (lambda ()
     (let ([index-file (with-handlers ([not-break? (lambda (x) #f)])
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
		   (with-handlers ([not-break?
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
				       (cons (cons (clean-html (cadr m)) r)
					     (loop))
				       (loop))))))))))])
	 index)))))

(define (parse-txt-file doc ht handle-one)
  (with-hash-table
   ht
   doc
   (lambda ()
     (with-handlers ([not-break? (lambda (x) 
				   (printf "~a~n" (exn-message x))
				   null)])
       (with-input-from-file (build-path doc "doc.txt")
	 (lambda ()
	   (let loop ()
	     (let ([start (file-position (current-input-port))]
		   [r (read-line)])
	       (cond
		[(eof-object? r) null]
		[(handle-one r start) => (lambda (vs) (append vs (loop)))]
		[else (loop)])))))))))

(define re:keyword-line (regexp "^>[^I]"))
(define text-keywords (make-hash-table))
(define (load-txt-keywords doc)
  (parse-txt-file
   doc
   text-keywords
   (lambda (r start)
     (cond
      [(regexp-match re:keyword-line r)
       (let* ([entry (read (open-input-string (substring r 1 (string-length r))))]
	      [key (let loop ([entry entry])
		     (cond
		      [(symbol? entry) entry]
		      [(pair? entry) (loop (car entry))]
		      [else (error "bad entry")]))])
	 (list
	  ; Make the keyword entry:
	  (list (symbol->string key) ; the keyword name
		(let ([p (open-output-string)])
		  (display entry p)
		  (get-output-string p)) ; the text to display
		"doc.txt" ; file
		start ; label (a position in this case)
		"doc.txt")))] ; title
      [else #f]))))

(define re:index-line (regexp "^>INDEX:(.*)"))
(define text-indices (make-hash-table))
(define (load-txt-index doc)
  (parse-txt-file
   doc
   text-indices
   (lambda (r start)
     (cond
      [(regexp-match re:index-line r)
       => (lambda (m)
	    (let ([p (open-input-string (cadr m))])
	      (let loop ()
		(let ([r (read p)])
		  (if (eof-object? r)
		      null
		      (cons
		       ; Make an index entry:
		       (cons r start)
		       (loop)))))))]
      [else #f]))))

(define (non-regexp s)
  (list->string
   (apply
    append
    (map
     (lambda (c)
       (cond 
	[(memq c '(#\$ #\| #\\ #\[ #\] #\. #\* #\? #\+ #\( #\)))
	 (list #\\ c)]
	[(char-alphabetic? c)
	 (list #\[ (char-upcase c) (char-downcase c) #\])]
	[else (list c)]))
     (string->list s)))))

(define (start-search)
  (let* ([given-find (send search-text get-value)]
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
       (send search enable #f)
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
	 (semaphore-post break-sema)
	 (send stop show #t)
	 (send editor erase)
	 (for-each
	  (lambda (doc doc-name doc-kind)
	    (define found-one? #f)
	    (define (found)
	      (unless found-one?
		(set! found-one? #t)
		(add-section doc-name ckey)))
	    ;; Keyword search
	    (let ([keys (case doc-kind
			 [(html)
			  (let ([keywords (build-path doc "keywords")])
			    (if (file-exists? keywords)
				(with-input-from-file keywords read)
				null))]
			 [(text) (load-txt-keywords doc)]
			 [else null])]
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
		 keys)))
	    ;; Index search
	    (unless (< search-level 1)
	      (let ([index (case doc-kind
			     [(html) (load-html-index doc)]
			     [(text) (load-txt-index doc)]
			     [else null])]
		    [add-index-choice (lambda (name desc)
					(case doc-kind
					  [(html)
					   (let loop ([desc desc])
					     (let ([m (regexp-match re:ilink desc)])
					       (when m
						 (loop (list-ref m 1))
						 (found)
						 (add-choice "idx" name
							     (clean-html (list-ref m 4))
							     (build-path doc (list-ref m 2))
							     (clean-html (list-ref m 3))
							     ckey))))]
					  [(text)
					   (found)
					   (add-choice "idx" name
						       "indexed content"
						       (build-path doc "doc.txt")
						       desc
						       ckey)]))])
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
		     index)))))
	    ;; Content Search
	    (unless (or (< search-level 2) exact?)
	      (let ([files (case doc-kind
			     [(html) (with-handlers ([not-break? (lambda (x) null)]) (directory-list doc))]
			     [(text) (list "doc.txt")]
			     [else null])])
		(for-each
		 (lambda (f)
		   (with-handlers ([not-break? (lambda (x)
						 ; (printf "~a~n" (exn-message x))
						 #f)])
		     (with-input-from-file (build-path doc f)
		       (lambda ()
			 (let loop ()
			   (let ([r (read-line)])
			     (unless (eof-object? r)
			       (when (regexp-match find r)
				 (found)
				 (add-choice "txt" 
					     (if (eq? doc-kind 'html)
						 (clean-html r)
						 r)
					     "content"
					     (build-path doc f)
					     "HTML"
					     ckey))
			       (loop))))))))
		 files))))
	  docs doc-names doc-kinds)
	 (queue-callback
	  (lambda ()
	    (when (eq? cycle-key ckey)
	      (when (zero? (send editor last-position))
		(send editor insert (format "Found nothing for \"~a\"." given-find)))))
	  #f))
       (semaphore-wait break-sema)) ; turn off breaks...
     (lambda ()
       (semaphore-post break-sema) ; breaks ok now because they have no effect
       (send stop show #f)
       (send search enable #t)
       (send where enable #t)
       (send exact enable #t)
       (end-busy-cursor)))))

(yield (make-semaphore 0))
