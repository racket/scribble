
#|
 TODO:
   * demonstrate setup-plt launcher

   * manuals as `doc' sub-collections?
   * doc.txt in sub-collections?

   * document help system
|#


(require-relative-library "sig.ss")

(require-library "file.ss")
(require-library "functio.ss")
(require-library "string.ss")

(require-library "url.ss" "net")

(define startup-url 
  (string-append "file:" (build-path (collection-path "doc") "index.htm")))

(invoke-unit/sig
 (require-relative-library "helpr.ss")
 help:option^
 mzlib:function^
 mzlib:string^
 mzlib:file^
 mzlib:url^
 mred^)
