(for-each (lambda (x)
	    (printf "loading ~s...~n" x)
	    (flush-output)
	    (load-relative x))
	  '("setup.ss"
	    "debug.ss"
	    "2darray.ss"
	    "coord.ss"
	    "mesh.ss"
	    "utils.ss"
	    "ui.ss"
	    "pager.ss"
	    "engine.ss"
	    "main.ss"))
(printf "done loading~n")
