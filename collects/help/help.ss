; (require-library "errortrace.ss" "errortrace")
; (error-print-width 80)
#|
 TODO:
   * demonstrate setup-plt launcher
   * manuals as `doc' sub-collections?
|#


(require-relative-library "sig.ss")

(require-library "file.ss")
(require-library "functio.ss")
(require-library "string.ss")

(require-library "url.ss" "net")

(define startup-url 
  (string-append "file:" (build-path (collection-path "help") "index.htm")))

(invoke-unit/sig
 (require-relative-library "helpr.ss")
 help:option^
 mzlib:function^
 mzlib:string^
 mzlib:file^
 mzlib:url^
 mred^)
