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

(require-library "startup-url.ss" "help")

(invoke-unit/sig
 (require-relative-library "helpr.ss")
 help:option^
 mzlib:function^
 mzlib:string^
 mzlib:file^
 mzlib:url^
 mred^)
