(module help mzscheme 
  (require (lib "web-server.ss" "web-server")
	   (lib "util.ss" "web-server")
	   (lib "cmdline.ss")
	   (lib "configuration.ss" "web-server")
	   (lib "configuration-structures.ss" "web-server")
	   "private/server.ss"
	   "private/browser.ss")

  (require (lib "exit.ss" "help" "servlets" "private"))

  (define launch-browser? #t)
  (define external-connections? #f)

  (command-line
   "help-desk"
   (current-command-line-arguments)
   (once-each
    [("-n" "--no-browser") "Do not launch browser"
     (set! launch-browser? #f)]
    [("-x" "--external-connections") "Allow external connections"
     (set! external-connections? #t)]))

  (define hd-cookie (start-help-server external-connections?))
  (define help-desk-port (hd-cookie->port hd-cookie))

  (define exit-sem (make-semaphore 0))
  (set-box! exit-box (lambda () (semaphore-post exit-sem)))

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
	(help-desk-browser hd-cookie))

  ; wait until shutdown
  (semaphore-wait/enable-break exit-sem))
