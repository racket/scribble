(module help mzscheme 

  (require (lib "web-server.ss" "web-server")
	   (lib "util.ss" "web-server")
	   (lib "configuration.ss" "web-server")
	   (lib "configuration-structures.ss" "web-server")
	   (lib "server.ss" "help")
	   (lib "browser.ss" "help"))	

  (require (lib "exit.ss" "doc" "help" "servlets" "private"))

  (define hd-cookie (start-help-server))
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

  (help-desk-browser hd-cookie)
  ; wait until shutdown
  (semaphore-wait/enable-break exit-sem))










