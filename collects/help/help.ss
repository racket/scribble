(when (getenv "MREDDEBUG")
  (current-load (let ([ol (current-load)]) (lambda (x) (printf "~a~n" x) (ol x))))
  (eval '(require (lib "errortrace.ss" "errortrace")))
  (error-print-width 180))
#|
 TODO:
   * demonstrate setup-plt launcher
   * manuals as `doc' sub-collections?
|#

(module help mzscheme 
  (require "startup-url.ss"
           (lib "framework.ss" "framework")
           "help-unit.ss"
           "help-sig.ss"
           (lib "plt-installer.ss" "setup")
           (lib "getinfo.ss" "setup")
           (lib "mred.ss"))
  
  (provide-signature-elements help^)
  
  (define frame-mixin values)
  (define (user-defined-doc-position x) #f)

  ;; just in case drscheme hasn't been run before, we
  ;; need a default for this preference.
  (preferences:set-default
   'drscheme:font-size
   (send (send (send (make-object text%) 
                     get-style-list)
               basic-style)
         get-size)
   (lambda (x) (and (number? x) (exact? x) (= x (floor x)))))

  (define-values/invoke-unit/sig help^
                                 help-unit@
                                 #f
                                 setup:plt-installer^
                                 setup:get-info^
                                 mred^
                                 framework^
                                 (frame-mixin)
                                 help:doc-position^)
  
  (new-help-frame startup-url))

