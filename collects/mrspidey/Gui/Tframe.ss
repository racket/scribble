;; Tframe.ss - creates MrSpidey frames
; ----------------------------------------------------------------------
; Copyright (C) 1995-97 Cormac Flanagan
;
; This program is free software; you can redistribute it and/or
; modify it under the terms of the GNU General Public License
; version 2 as published by the Free Software Foundation.
; 
; This program is distributed in the hope that it will be useful,
; but WITHOUT ANY WARRANTY; without even the implied warranty of
; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
; GNU General Public License for more details.
; 
; You should have received a copy of the GNU General Public License
; along with this program; if not, write to the Free Software
; Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.
; ----------------------------------------------------------------------

; Global variable to record the most-recently created frame
(define tf (void))

(define shake-it-repetitions 25)

(define spidey:frame%
  (class 
    (mred:make-searchable-frame% mred:simple-menu-frame%)
    (arg-main arg-filename summary-edit . init-locs)

    (inherit show get-canvas ;; get-canvas%
      make-menu create-status-line set-status-text
      get-position get-size set-icon panel
      set-title file-menu set-title-prefix
      set-size)

    ;; ----------

    (rename
      [super-on-close on-close]
      [super-make-menu-bar make-menu-bar])

    ;; ----------
      
    (public

      [get-edit (lambda () program-edit)]
      [auto-set-wrap #f]
      [edit% flow-arrow:media-edit%]    
      [get-canvas% (lambda () mred:wrapping-canvas%)]
      [on-close
        (lambda ignored
          (send main on-frame-close filename)
          (send this show #f)
          (remq-callback-sdl-alg-changed! flush-type-cache)
          (super-on-close))]

      [set-show-mode
        (lambda (which)
          (pretty-debug-gui `(set-show-mode ,which))
          (unless (eq? which canvas-show-mode)
            (set! canvas-show-mode which)
            (send summary-canvas stretchable-in-y (eq? which 'summary))
            (send panel change-children
              (lambda (ignore)
                (filter
                  (lambda (x) x)
                  (list 
                    (and (or (eq? which 'program) (eq? which 'both))
                      program-canvas)
                    (and (or (eq? which 'summary) (eq? which 'both))
                      summary-canvas)))))))]

      ;; ---------- Set up the menus
      [file-menu:new #f]
      [file-menu:open #f]
      [file-menu:revert #f]
      [file-menu:save #f]
      [file-menu:save-as #f]
      ;;[file-menu:print #f]
      ;;[file-menu:between-print-and-close (lambda args (void))]
      [file-menu:between-save-and-print (lambda args (void))]

      [edit-menu:undo #f]   
      [edit-menu:redo #f]
      [edit-menu:cut #f]
      [edit-menu:paste #f]
      [edit-menu:delete #f]
      ;;[edit-menu:find #f]
      [edit-menu:replace #f]
      [edit-menu:between-replace-and-preferences (lambda args (void))]

      [file-menu:close on-close]
      [file-menu:between-open-and-save
        (lambda (file-menu)
          (send file-menu
            append-item
            "Open ..."
            (lambda () (send main open-analyzed-file-choice)))
          (send file-menu
            append-item
            "Open All"
            (lambda () (wrap-busy-cursor (lambda () (send main open-all #t)))))
          (send file-menu
            append-item
            "Load All"
            (lambda () (wrap-busy-cursor (lambda () (send main open-all #f)))))
          (send file-menu
            append-item
            "Reanalyze"
            (lambda () (wrap-busy-cursor (lambda () (send main reanalyze)))))
          (send file-menu append-separator))]
      [file-menu:between-close-and-quit
        (lambda (file-menu)
          (send file-menu
            append-item
            "Close All"
            ;;(format "Close ~a" st:name)
            (lambda () 
              (wrap-busy-cursor 
                (lambda () 
                  (send main close-all-frames))))))]

      [flush-type-cache (lambda () (void))]

      [calc-show
        (lambda ()
          (set-show-mode 'program)
          (when (and 
                  summary-canvas
                  ;; Show summary only if some real content
                  (> (send summary-edit last-line) 3))
            ;;(printf "Summary-edit size ~s~n" (send summary-edit last-line))
            (set-show-mode 'both)))]

      [make-menu-bar
        (lambda ()
          (let ([menu-bar (super-make-menu-bar)])
            (let ([show-menu (make-menu)])
              (send menu-bar append show-menu "Show")
              (set! init-show-menu
                (lambda ()
                  (send show-menu
                    append-check-set
                    (list 
                      (cons "Program Only" 'program)
                      (cons "Summary Only" 'summary)
                      (cons "Both"         'both))
                    set-show-mode
                    (case canvas-show-mode
                      [(program) 0]
                      [(summary) 1]
                      [(both) 2]
                      [else 0]))))
                          
              '(send show-menu append-separator)
              '(send show-menu
                 append-check-set
                 (map (lambda (mode) (cons (mode-name mode) mode))
                   modes)
                 set-display-mode))

            (let ([clear-menu (make-menu)])
              (send menu-bar append clear-menu "Clear")
              (send* clear-menu
                (append-item 
                  "Arrows+Types" 
                  (lambda () 
                    (wrap-busy-cursor 
                      (lambda () 
                        (send* program-edit 
                          (delete-arrows)
                          (delete-types))))
                    "Removes both types and arrows from the window"))
                (append-item
                  "Arrows" 
                  (lambda () 
                    (wrap-busy-cursor 
                      (lambda () 
                        (send program-edit delete-arrows))))
                  "Removes all arrows from the window")
                (append-item
                  "Types" 
                  (lambda () 
                    (wrap-busy-cursor 
                      (lambda () 
                        (send program-edit delete-types))))
                  "Removes all types from the window"))
              (unless st:restricted
                (send clear-menu append-item
                  "Shake Buffer" 
                  (lambda () (for i 0 shake-it-repetitions (shake-it)))
                  "Sends random inputs to buffer")
                (send clear-menu append-item
                  "Rewrite Buffer" 
                  (lambda () 
                    (wrap-busy-cursor 
                      (lambda () 
                        (set-display-mode display-mode))))
                  "Removes all types and arrows from the window")))    

            (let ([filter-menu (make-menu)])
              (send menu-bar append filter-menu "Filter")
              (send filter-menu
                append-check-set
                (analysis-get-filters)
                analysis-set-arrow-filter!)
              (analysis-set-arrow-filter! #f))

            

            menu-bar))]

      [init-show-menu #f]
      )
    
    ;; ----------

    (public
      [main arg-main]                   ; parent containing the global state
      program-canvas
      program-edit
      summary-canvas                    ; or #f if no summary
      [filename arg-filename]

      [canvas-show-mode 'none]          ; 'program, 'summary, or 'both
      [display-mode  (car modes)]       ; which display mode

      [set-display-mode
        (lambda (which)
          (set! display-mode which)
          (pretty-debug-gui `(set-display-mode ,display-mode ,filename))
          ;; Call main to create a new edit buffer,
          ;; and to load and annotate file
          (set! program-edit 
            (send main annotated-edit display-mode filename program-canvas))
          (send program-canvas set-media program-edit))]

      [focus-def
        (lambda (pos)
          (unless (memq display-mode '(program both))
            (set-show-mode 'both))
          (let* ( [real-pos (send program-edit real-start-position pos)]
                  [end (mred:scheme-forward-match 
                         program-edit real-pos 
                         (send program-edit last-position))])
            (thread
              (lambda ()
                (sleep)
                (send program-canvas set-focus)
                (send program-edit
                  set-position-bias-scroll -1 real-pos end)))))]

      [shake-it
        (lambda ()
          (send program-edit shake-it))]

      ;; ----------

      )
	
    (sequence 
      (pretty-debug-gui
        `(Tframe ,arg-main ,arg-filename ,summary-edit ,@init-locs))
      (match init-locs
        [(w h x y) 
          (pretty-debug-gui `(send this set-size ,(+ x 15) ,(+ y 15) ,w ,h))
          (set-size x y w h)]
        [() (void)])
      (pretty-debug-gui `(Tframe super-init))
      (let ([t (format "~a: ~a" 
                 st:name (file-name-from-path arg-filename))]) 
        (super-init t)
        (pretty-debug-gui `(Tframe super-init done))
        (set-title-prefix t)))

    	
    ;; ---------------------------------------------------------

    (sequence
      (set! flush-type-cache
        (lambda ()
          (pretty-debug-gui '(Tframe flush-type-cache))
          (unless (void? program-edit) 
            (send program-edit flush-type-cache))))

      (add-callback-sdl-alg-changed! flush-type-cache)

      (pretty-debug-gui
        `(Tframe ,arg-main ,arg-filename ,summary-edit ,@init-locs))
      (set! tf this)

      ;; ------------------------------------------------------------
      ;; build the canvases

      (pretty-debug-gui '(setting summary-canvas))
      (set! summary-canvas
        (and summary-edit
          (let ([c 
                  ;(make-object (get-canvas%) panel)
                  (make-object (class-asi mred:one-line-canvas%
                                 (public
                                   [lines 5]
                                   [style-flags 0]))
                    panel) 
                  ])
            ;;(send c set-lines 2)
            (send c set-media summary-edit)
            c)))
      (assert (is-a? summary-canvas mred:connections-media-canvas%))
      (pretty-debug-gui '(setting program-canvas))
      (set! program-canvas (get-canvas))
      (set-display-mode (car modes))
      (pretty-debug-gui '(done setting canvases))

      ;; ------------------------------------------------------------
      ;; install the icon
      
      '(let ([icon (make-object wx:icon% 
                     (build-absolute-path 
                       (collection-path "mrspidey") ; MATTHEW: got rid of plt-home
                       "icon.gif")
                     wx:const-bitmap-type-gif
                     )])
         (when (send icon ok?) (set-icon icon)))

      ;; ------------------------------------------------------------
      ;; status help line 

      ;;(unless (eq? mred:platform 'macintosh)
      ;;  (create-status-line))

      ;;(set-status-text 
      ;; "Mouse: Left-type/parents  Midde-Ancestors  Right-Close")

      ;; ------------------------------------------------------------

      ;;(set-display-mode display-mode)
      (calc-show)
      (init-show-menu)
      (show #t)
      
      )))


