(module help mzscheme 
  (require (lib "cmdline.ss")
	   "private/server.ss"
	   "private/browser.ss")

  (define remote-connections? #f)
  (define iconize? #f)
  (define quiet? #f)
  
  (command-line
   "help-desk"
   (current-command-line-arguments))

  (define hd-cookie (start-help-server (lambda (x) x)))
  (unless hd-cookie 
    (printf "Help Desk: could not start server\n")
    (exit -1))

  (help-desk-browser hd-cookie))


