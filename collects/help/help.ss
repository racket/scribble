(when (getenv "MREDDEBUG")
  (current-load (let ([ol (current-load)]) (lambda (x) (printf "~a~n" x) (ol x))))
  (eval '(require "errortrace.ss" "errortrace"))
  (error-print-width 180))
#|
 TODO:
   * demonstrate setup-plt launcher
   * manuals as `doc' sub-collections?
|#


(module help mzscheme 
  (require "sig.ss"
           (lib "get-info.ss" "setup")
           (lib "file.ss")
           (lib "list.ss")
           (lib "string.ss")
           (lib "url.ss" "net")
           "startup-url.ss"
           (lib "framework.ss" "framework")
           (lib "plt-installer.ss" "setup")
           (lib "get-info.ss" "setup")
           "help-unit.ss")
  
  (define-values/invoke-unit/sig
   help:get-info^
   (unit/sig help:get-info^
     (import)
     
     (define (get-language-level)
       'unknown)
     (define (get-teachpack-names)
       'unknown))
   drscheme:export:help-info)

  (define frame-mixin values)
  (define (user-defined-doc-position x) #f)

  (preferences:set-default
   'drscheme:font-size
   (send (send (send (make-object text%) 
                     get-style-list)
               basic-style)
         get-size)
   (lambda (x) (and (number? x) (exact? x) (= x (floor x)))))

  (define-values/invoke-unit/sig help:help^
                                 help-unit@
                                 #f
                                 setup:plt-installer^
                                 mred^
                                 framework^
                                 (frame-mixin)
                                 help:doc-position^)
  
  (new-help-frame startup-url))

