; We'll use the same function as both a keyboard and mouse event
;  handler. If it's a mouse event, we only want to insert something
;  on the button-down part
(define insert-star
  (lambda (edit event)
    (if (or (not (is-a? event wx:mouse-event%))
	    (send event button-down?))
	(send edit insert "*"))
    #t))

(let ((kmap (send mred:console-edit get-keymap)))
	(send kmap add-key-function "insert-star" insert-star)	
	(send kmap add-mouse-function "insert-star" insert-star)	
	(send kmap map-function "c:." "insert-star")
	(send kmap map-function "ESC;rightbutton" "insert-star"))

