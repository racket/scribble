
#|
 TODO:
   * starting location as option
   * define a max hit count

   * setup-plt launcher

   * manuals as `doc' sub-collections?
   * doc.txt in sub-collections?

   * document help system
   * keywordize and index existing doc.txt files
|#


(require-relative-library "sig.ss")

(require-library "file.ss")
(require-library "functio.ss")
(require-library "string.ss")

(require-library "url.ss" "net")

(invoke-unit/sig
 (require-relative-library "helpr.ss")
 mzlib:function^
 mzlib:string^
 mzlib:file^
 mzlib:url^
 mred^)
