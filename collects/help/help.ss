#|
 TODO:
   * demonstrate setup-plt launcher
   * manuals as `doc' sub-collections?
|#

(module help mzscheme 
  (require (lib "class.ss")
           (lib "unitsig.ss")
           "startup-url.ss"
           "help-unit.ss"
           "help-sig.ss"
	   "proxy-prefs.ss"
           (lib "string-constant.ss" "string-constants")
           (lib "framework.ss" "framework")
           (lib "framework-sig.ss" "framework")
           (lib "plt-installer.ss" "setup")
           (lib "plt-installer-sig.ss" "setup")
           (lib "mred-sig.ss" "mred")
           (lib "mred.ss" "mred"))
    
  (preferences:add-general-panel)
  (add-proxy-prefs-panel)

  (define (frame-mixin %)
    (class %
      (define/override (help-menu:about-string)
        (string-constant about-help-desk))
      (define/override (help-menu:about-callback i e)
        (message-box (string-constant about-help-desk)
                     (format 
                      (string-constant help-desk-about-string)
                      (version:version))
                     this))
      (define/override (help-menu:create-about?) #t)
      (define/override (help-menu:after-about menu)
        (make-object menu-item% (string-constant help-on-help) menu
          (lambda (i e)
            (message-box
             (string-constant help-on-help)
             (string-constant help-on-help-details)
             this))))
      (super-instantiate ())))
  
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
                                 help@
                                 #f
                                 setup:plt-installer^
                                 mred^
                                 framework^
                                 (frame-mixin)
                                 help:doc-position^)
  
  (new-help-frame startup-url))
