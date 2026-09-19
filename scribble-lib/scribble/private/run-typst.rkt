#lang racket/base

(require racket/system racket/port)

(provide run-typst)

(define (run-typst file [notify void])
  (define typst (get-typst-binary))
  (define pdf-file (path-replace-suffix file #".pdf"))
  (notify "running typst on ~a" file)
  (define stderr (open-output-bytes))
  (unless (parameterize ([current-output-port (open-output-nowhere)]
                         [current-error-port stderr])
            (system* typst "compile" file pdf-file))
    (write-bytes (get-output-bytes stderr) (current-error-port))
    (error 'run-typst "got error exit code"))
  pdf-file)

(define (get-typst-binary)
  (or (or (find-executable-path "typst")
          (and (eq? (system-type) 'windows) (find-executable-path "typst.exe")))
      (error 'run-typst "could not find a `typst' executable")))
