(when (getenv "MREDDEBUG")
  (current-load (let ([ol (current-load)]) (lambda (x) (printf "~a~n" x) (ol x))))
  (require-library "errortrace.ss" "errortrace")
  (error-print-width 180))
#|
 TODO:
   * demonstrate setup-plt launcher
   * manuals as `doc' sub-collections?
|#


(require-relative-library "sig.ss")

(require-library "get-info.ss" "setup")

(require-library "file.ss")
(require-library "functio.ss")
(require-library "string.ss")

(require-library "url.ss" "net")

(require-library "startup-url.ss" "help")

(require-library "framework.ss" "framework")

(require-library "plt-installer.ss" "setup")

(begin-elaboration-time
 (require-library "invoke.ss"))

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
  (require-relative-library "helpr.ss")
  #f
  mzlib:function^
  mzlib:string^
  mzlib:file^
  mzlib:url^
  setup:plt-installer^
  mred^
  framework^
  (frame-mixin)
  help:doc-position^)

(new-help-frame startup-url)

(yield (make-semaphore))
