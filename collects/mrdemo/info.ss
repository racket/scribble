(let ([info (require-library "info.ss" "system")])
  (lambda (request)
    (case request
      [(app-unit-library) "app.ss"]
      [(app-sig-library) "sig.ss"]
      [(name) "MrEd Demos"]
      [else (info request)])))
