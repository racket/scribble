(module help mzscheme 
  (require (lib "web-server.ss" "web-server")
	   (lib "util.ss" "web-server")
	   (lib "class.ss")
	   (lib "mred.ss" "mred")
	   (lib "cmdline.ss")
	   (lib "configuration.ss" "web-server")
	   (lib "configuration-structures.ss" "web-server")
	   "private/server.ss"
	   "private/browser.ss")

  (define launch-browser? #t)
  (define external-connections? #f)
  (define iconize? #f)
  (define quiet? #f)
  (define port #f)

  (command-line
   "help-desk"
   (current-command-line-arguments)
   (once-each
    [("-n" "--no-browser") "Do not launch browser"
     (set! launch-browser? #f)]
    [("-x" "--external-connections") "Allow external connections"
     (set! external-connections? #t)]
    [("-i" "--iconize") "Iconize the control panel"
     (set! iconize? #t)]
    [("-q" "--quiet") "Don't show the control panel"
     (set! quiet? #t)]
    [("-p" "--port") number "Use given port number"
     (with-handlers
      ((void (lambda _
	       (error "Help Desk: expected exact integer for port"))))
      (let ([port-val (string->number number)])
	(unless (and (integer? port-val) (exact? port-val))
		(raise 'not-exact-integer))
	(set! port port-val)))]))

  (define hd-cookie (start-help-server port external-connections?))
  (define help-desk-port (hd-cookie->port hd-cookie))

  ; allow server startup time
  (let loop ()
    (with-handlers
     ([void (lambda _ (sleep 1) (loop))])
     (let-values 
      ([(iport oport) (tcp-connect "127.0.0.1" help-desk-port)])
      (sleep 1)
      (close-output-port oport)
      (close-input-port iport))))

  (when launch-browser?
	(help-desk-browser hd-cookie)
       ; allow browser startup time
	(sleep 2))

  (if quiet?
      (semaphore-wait (make-semaphore 0))
      (let* ([hd-frame%
	      (class frame%
		     (inherit show)
		     (field
		      [panel #f]
		      [main-sd-button #f]
		      [shutdown-dialog
		       (lambda ()
			 (let* ([cb-frame (instantiate frame% ()
						       (label "Confirm"))]
				[vpanel (instantiate vertical-panel% ()
						     (parent cb-frame))]
				[msg (instantiate message% ()
						  (label "Really shutdown Help Desk server?")
						  (parent vpanel))]
				[hpanel (instantiate horizontal-panel% ()
						     (parent vpanel)
						     (alignment '(center center)))]
				[sd-button (instantiate button% ()
							(label "Shutdown")
							(parent hpanel)
							(callback (lambda (b ev)
								    (send cb-frame show #f)
								    (send this show #f))))]
				[no-sd-button (instantiate button% ()
							   (label "Cancel")
							   (parent hpanel)
							   (callback (lambda (b ev)
								       (send main-sd-button 
									     enable #t)
								       (send cb-frame show #f))))])
			   (send main-sd-button enable #f)
			   (send cb-frame center)
			   (send cb-frame show #t)))])
		     (super-instantiate ())
		     (set! panel
			   (instantiate vertical-panel% ()
					(parent this)))
		     (instantiate message% () 
				  (label (format "Help Desk server running on port ~a" 
						 (hd-cookie->port hd-cookie)))
				  (parent panel))
		     (instantiate button% ()
				  (label "Help Desk Home")
				  (parent panel)
				  (min-width 100)
				  (callback 
				   (lambda (b ev)
				     (help-desk-browser hd-cookie))))
		     (set! main-sd-button
			   (instantiate button% ()
					(label "Shutdown Server")
					(parent panel)
					(min-width 100)
					(callback (lambda (b ev)
						    (shutdown-dialog))))))]
	     [frame 
	      (instantiate hd-frame% ()
			   (label "PLT Help Desk")
			   (min-width 175)
			   (stretchable-width #f)
			   (stretchable-height #f))])
	(send frame center)
	(send frame show #t)
	(when iconize?
	      (send frame iconize #t)))))

	
	
	
	
	
	
	
	
	
	
