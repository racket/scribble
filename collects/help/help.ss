(module help mzscheme
  (define cm-env-var "PLTHDCM")
  (define install-cm? (getenv cm-env-var))
  (define cm-trace? (and install-cm? (equal? (getenv cm-env-var) "trace")))
  
  (define et-env-var "PLTHDDEBUG")
  (define errortrace? (getenv et-env-var))
  
  (cond
    [errortrace?
     (printf "~a: installing errortrace & compilation manager\n" et-env-var)
     (use-compiled-file-paths (list (build-path "compiled" "errortrace")))
     (error-display-handler (dynamic-require '(lib "errortrace-lib.ss" "errortrace")
                                             'errortrace-error-display-handler))
     (let-values ([(current-managed-zo-compile
                    make-compilation-manager-load/use-compiled-handler
                    manager-trace-handler)
                   (parameterize ([current-namespace (make-namespace)])
                     (values
                      (dynamic-require '(lib "cm.ss") 'current-managed-zo-compile)
                      (dynamic-require '(lib "cm.ss") 'make-compilation-manager-load/use-compiled-handler)
                      (dynamic-require '(lib "cm.ss") 'manager-trace-handler)))])
       (current-managed-zo-compile
        (dynamic-require '(lib "zo-compile.ss" "errortrace") 'zo-compile))
       (current-load/use-compiled (make-compilation-manager-load/use-compiled-handler)))]
    [install-cm?
     (printf "~a: installing compilation manager\n" cm-env-var)
     (let-values ([(make-compilation-manager-load/use-compiled-handler
                    manager-trace-handler)
                   (parameterize ([current-namespace (make-namespace)])
                     (values
                      (dynamic-require '(lib "cm.ss") 'make-compilation-manager-load/use-compiled-handler)
                      (dynamic-require '(lib "cm.ss") 'manager-trace-handler)))])
       (current-load/use-compiled (make-compilation-manager-load/use-compiled-handler))
       (when cm-trace?
         (manager-trace-handler
          (lambda (x) (display x) (newline)))))])
  
  ;; start help desk for real
  (dynamic-require '(lib "help-app-main.ss" "help" "private") #f))