;;   ncad02c.scm - Scheme code for NanoCAD version 0.2
;;   Copyright (C) 1996 Will Ware
;;   Modified by Matthew Flatt for MrEd 43
;;   
;;   This program is free software; you can redistribute it and/or
;;   modify it under the terms of the GNU General Public License
;;   as published by the Free Software Foundation; either version 2
;;   of the License, or (at your option) any later version.
;;   
;;   This program is distributed in the hope that it will be useful,
;;   but WITHOUT ANY WARRANTY; without even the implied warranty of
;;   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;   GNU General Public License for more details.
;;   
;;   You should have received a copy of the GNU General Public License
;;   along with this program; if not, write to the Free Software
;;   Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
;;
;;   I can be reached via email at <wware@world.std.com>.

;; ******************************************************************
;; DATA STRUCTURES

;; An atom (in the chemistry sense) is list with an integer (an index
;; into the "Periodic" table, followed by a 3-vector of position
;; information in angstroms.

;; The force list is just a list of force vectors, one for each
;; atom.  Forces are in tens of nanojoules (attojoules per
;; angstrom). When I get around to implementing a velocity list, it
;; will also just be a list of vectors, but I don't know what I'll
;; use for units.

;; A bond is a three-integer list. The first integer is the order of the
;; bond (1, 2, or 3), and the second and third integers are indices into
;; the atom list.

;; A "Periodic" table entry is a list with five elements: an atom for the
;; name of the element, three double-precision-reals for evdw, rvdw, and
;; atomic mass, and an integer for the Official MM2 Type (to stay
;; compliant with tables in 'Nanosystems').

;; An MM2 term is a list starting with an atom (LENGTH, ANGLE, TORSION, or
;; VDW) followed by some integers, which are indices into the atom list.

;; The atom list, bond list, "periodic" table, and term list are respectively
;; lists of all these things.

(define ncad@
  (unit/sig ()
    (import mred:wx^ (mred : mred^))
    
    (define ncad:stand-alone #f)
    (define (error-msg txt) ())  ;; redefined in GUI
    (define (entering fcn) ())
    
    (define atom-list ())
    (define bond-list ())
    (define term-list ())
    (define force-list ())
    
    ;; ******************************************************************
    ;; Empirical coefficients
    
    (define periodic-table
      '(("C" 0.357 1.9 19.925 1)         ;; sp3
	("C" 0.357 1.94 19.925 2)        ;; sp2 alkene
	("C" 0.357 1.94 19.925 3)        ;; sp2 carbonyl
	("C" 0.357 1.94 19.925 4)        ;; sp acetylene
	("H" 0.382 1.5 1.674 5)          ;; hydrocarbon
	("O" 0.406 1.74 26.565 6)        ;; C-O-[HC]
	("O" 0.536 1.74 26.565 7)        ;; O carbonyl
	("N" 0.447 1.82 23.251 8)        ;; sp3
	("F" 0.634 1.65 31.545 11)       ;; flouride
	("Cl" 1.95 2.03 38.064 12)       ;; chloride
	("Br" 2.599 2.18 131.038 13)     ;; bromide
	("I" 3.444 2.32 210.709 14)      ;; iodide
	("S" 1.641 2.11 53.087 15)       ;; sulfide
	("Si" 1.137 2.25 46.454 19)      ;; silane
	("LP" 0.13 1.2 0.0 20)           ;; lone pair
	("H" 0.292 1.2 1.674 21)         ;; alcohol
	("C" 0.357 1.9 19.925 22)        ;; cyclopropane
	("P" 1.641 2.11 51.464 25)))     ;; phosphide
    
    (define (lookup-helper compare-func result-func default-result the-list)
      (cond ((null? the-list) default-result)
	    ((compare-func (car the-list)) (result-func (car the-list)))
	    (else (lookup-helper compare-func result-func default-result
				 (cdr the-list)))))
    
    ;; A length-coefficient entry is two MM2 atom types, followed by a
    ;; k_s value, followed by an r0 value.
    
    (define length-coefficients
      '((1 5 460 1.113)
	(1 1 440 1.523)
	(2 2 960 1.337)
	(4 4 1560 1.212)
	(1 6 536 1.402)
	(1 8 510 1.438)
	(3 7 1080 1.208)
	(1 11 510 1.392)
	(1 12 323 1.795)
	(1 13 230 1.949)
	(1 14 220 2.149)
	(8 20 610 0.6)
	(8 8 560 1.381)
	(6 20 460 0.6)
	(6 21 460 0.942)
	(6 6 781 1.470)
	(1 19 297 1.880)
	(1 25 291 1.856)
	(1 15 321.3 1.815)
	(19 19 185 2.332)
	(22 22 440 1.501)))
    
    (define (lookup-length-coeffs m n)
      (lookup-helper
       (lambda (x) (or (and (= m (car x))
			    (= n (cadr x)))
		       (and (= n (car x))
			    (= m (cadr x)))))
       (lambda (x) (cddr x))
       '(400 1.3)
       length-coefficients))
    
    ;; An angle-coefficient entry is three MM2 atoms types, followed by
    ;; a k_th value, followed by a th0 value.
    
    (define angle-coefficients
      '((1 1 1 0.45 1.911)
	(1 1 5 0.36 1.909)
	(5 1 5 0.32 1.909)
	(1 1 11 0.65 1.911)
	(11 1 11 1.07 1.869)
	(1 2 1 0.45 2.046)
	(2 1 2 0.45 1.911)
	(1 2 2 0.55 2.119)
	(2 2 5 0.36 2.094)
	(2 2 2 0.43 2.094)
	(1 4 4 0.2 3.142)
	(1 3 7 0.46 2.138)
	(1 6 1 0.77 1.864)
	(1 8 1 0.63 1.88)
	(1 1 6 0.57 1.911)
	(1 6 20 0.35 1.835)
	(1 8 20 0.5 1.906)
	(20 6 20 0.24 2.286)
	(19 19 19 0.35 1.943)
	(19 1 19 0.4 2.016)
	(1 19 1 0.48 1.934)
	(12 1 12 1.08 1.95)
	(1 1 15 0.55 1.902)
	(1 15 1 0.72 1.902)
	(4 4 5 0.4 3.142)
	(7 3 1 0.4 3.142)
	(7 3 2 0.4 3.142)
	(7 3 3 0.4 3.142)
	(5 8 5 1.1 1.85)))
    
    (define (lookup-angle-coeffs m n p)
      (lookup-helper
       (lambda (x) (or (and (= m (car x))
			    (= n (cadr x))
			    (= p (caddr x)))
		       (and (= p (car x))
			    (= n (cadr x))
			    (= m (caddr x)))))
       (lambda (x) (cdddr x))
       '(0.4 2.094)
       angle-coefficients))
    
    ;; An torsion-coefficient entry is four MM2 atoms types, followed by
    ;; three values for v1, v2, and v3 respectively.
    
    (define torsion-coefficients
      '((1 1 1 1 1.39 1.88 0.65)
	(1 1 1 5 0.0 0.0 1.85)
	(5 1 1 5 0.0 0.0 1.65)
	(1 1 1 11 0.0 -0.6 6.46)
	(1 2 2 1 -0.69 69.47 0.0)
	(2 2 2 2 -6.46 55.58 0.0)
	(5 2 2 5 0.0 104.21 0.0)))
    
    (define (lookup-torsion-coeffs m n p q)
      (lookup-helper
       (lambda (x) (or (and (= m (car x))
			    (= n (cadr x))
			    (= p (caddr x))
			    (= q (cadddr x)))
		       (and (= q (car x))
			    (= p (cadr x))
			    (= n (caddr x))
			    (= m (cadddr x)))))
       (lambda (x) (cddddr x))
       '(0.0 0.0 0.0)
       torsion-coefficients))
    
    ;; ******************************************************************
    ;; Geometric fun with vectors
    
    (define (vplus v1 v2)
      (vector (+ (vector-ref v1 0) (vector-ref v2 0))
	      (+ (vector-ref v1 1) (vector-ref v2 1))
	      (+ (vector-ref v1 2) (vector-ref v2 2))))
    
    (define (vdiff v1 v2)
      (vector (- (vector-ref v1 0) (vector-ref v2 0))
	      (- (vector-ref v1 1) (vector-ref v2 1))
	      (- (vector-ref v1 2) (vector-ref v2 2))))
    
    (define (dot-product v1 v2)
      (+ (* (vector-ref v1 0) (vector-ref v2 0))
	 (* (vector-ref v1 1) (vector-ref v2 1))
	 (* (vector-ref v1 2) (vector-ref v2 2))))
    
    (define (cross-product v1 v2)
      (vector (- (* (vector-ref v1 1) (vector-ref v2 2))
		 (* (vector-ref v1 2) (vector-ref v2 1)))
	      (- (* (vector-ref v1 2) (vector-ref v2 0))
		 (* (vector-ref v1 0) (vector-ref v2 2)))
	      (- (* (vector-ref v1 0) (vector-ref v2 1))
		 (* (vector-ref v1 1) (vector-ref v2 0)))))
    
    (define (vlen v)
      (sqrt (dot-product v v)))
    
    (define (vscale v x)
      (vector (* (vector-ref v 0) x)
	      (* (vector-ref v 1) x)
	      (* (vector-ref v 2) x)))
    
    ;; find the component of v1 perpendicular to v2
    (define (perpendicular-component v1 v2)
      (vdiff v1 (vscale v2 (/ (dot-product v1 v2) (dot-product v2 v2)))))
    
    (define (safe-acos z)
      (if (> z 1.0) (set! z 1.0))
      (if (< z -1.0) (set! z -1.0))
      (acos z))
    
    (define (v-angle v1 v2)
      (safe-acos (/ (dot-product v1 v2) (* (vlen v1) (vlen v2)))))
    
    (define (i-length v1 v2)
      (vlen (vdiff v1 v2)))
    
    (define (i-angle v1 v2 v3)
      (v-angle (vdiff v1 v2) (vdiff v3 v2)))
    
    (define (i-torsion v1 v2 v3 v4)
      (let ((p (vdiff v1 v2))
	    (q (vdiff v2 v3))
	    (r (vdiff v4 v3)))
	(v-angle (perpendicular-component p q)
		 (perpendicular-component r q))))
    
    ;; ******************************************************************
    ;; Forces computed by energy terms
    
    (define (do-to-all-atoms f)
      (define (helper f n lst)
	(cond ((null? lst) ())
	      (else (cons (apply f (list n (car lst)))
			  (helper f (+ n 1) (cdr lst))))))
      (set! atom-list
	    (helper f 0 atom-list)))
    
    (define (mm2-type a)
      (cadddr (cdr (list-ref periodic-table a))))
    
    (define (add-comp v n x)
      (vector-set! v n (+ (vector-ref v n) x)))
    
    (define (sub-add-to-force L n vsrc)
      (cond ((null? L) ())
	    ((= n 0)
	     (let ((v (car L)))
	       (cons (vector (+ (vector-ref v 0) (vector-ref vsrc 0))
			     (+ (vector-ref v 1) (vector-ref vsrc 1))
			     (+ (vector-ref v 2) (vector-ref vsrc 2)))
		     (cdr L))))
	    (else (cons (car L)
			(sub-add-to-force (cdr L) (- n 1) vsrc)))))
    
    (define (add-to-force n v)
      (set! force-list
	    (sub-add-to-force force-list n v)))
    
    (define (length-force m n)
      (entering "length-force")
      (let* ((ma (list-ref atom-list m))  ;; members of atom-list
	     (na (list-ref atom-list n))
	     (coeffs
	      (lookup-length-coeffs
	       (mm2-type (car ma))
	       (mm2-type (car na))))
	     (ks (* 0.01 (car coeffs)))
	     (r0 (cadr coeffs))
	     (rd (vdiff (cadr ma) (cadr na)))
	     (r (sqrt (dot-product rd rd)))
	     (du-dr (* ks (- r r0)))
	     (mult (/ (- du-dr) r)))
	(add-to-force m (vscale rd mult))
	(add-to-force n (vscale rd (- mult)))))
    
    ;; OK, for angles it gets a bit trickier. Let the atom positions be vectors
    ;; r1, r2, r3, with r2 the vertex. Define the following:
    ;; L1 = -dotproduct(r1-r2,r3-r2) * (r1-r2) + dotproduct(r1-r2,r1-r2) * (r3-r2)
    ;; L3 = -dotproduct(r1-r2,r3-r2) * (r3-r2) + dotproduct(r3-r2,r3-r2) * (r1-r2)
    ;; m1 = L1/|L1|, m3 = L3/|L3|
    ;; Potential energy is given by U = f(theta), force on atom 1 is
    ;; f1 = -m1 * dU/dm1 = (-m1/|r1-r2|) * dU/dtheta
    ;; Likewise f3 = (-m3/|r3-r2|) * dU/dtheta
    ;; Conservation: f2 = -f1-f3
    
    (define (angle-force m n p)
      (entering "angle-force")
      (let* ((ma (list-ref atom-list m))  ;; members of atom-list
	     (na (list-ref atom-list n))
	     (pa (list-ref atom-list p))
	     (coeffs
	      (lookup-angle-coeffs
	       (mm2-type (car ma))
	       (mm2-type (car na))
	       (mm2-type (car pa))))
	     (kth (car coeffs))
	     (th0 (cadr coeffs))
	     (th (i-angle (cadr ma) (cadr na) (cadr pa)))
	     (tdif (- th th0))
	     (du-dth (* kth (* tdif (+ 1 (* 1.508 tdif tdif)))))
	     (r1r2 (vdiff (cadr ma) (cadr na)))
	     (r3r2 (vdiff (cadr pa) (cadr na)))
	     (L1 (vdiff (vscale r3r2 (dot-product r1r2 r1r2))
			(vscale r1r2 (dot-product r1r2 r3r2))))
	     (f1 (vscale L1 (/ du-dth (* (vlen L1) (vlen r1r2)))))
	     (L3 (vdiff (vscale r1r2 (dot-product r3r2 r3r2))
			(vscale r3r2 (dot-product r1r2 r3r2))))
	     (f3 (vscale L3 (/ du-dth (* (vlen L3) (vlen r3r2)))))
	     (f2 (vscale (vplus f1 f3) -1.0)))
	(add-to-force m f1)
	(add-to-force n f2)
	(add-to-force p f3)))
    
    ;; To think about torsions, think of the projection of the whole thing into
    ;; the plane perpendicular to the torqued bond.
    
    ;; Actually, torsions appear to contribute a negligible amount of force
    ;; compared even to van der Waals, but they require this atrociously complex
    ;; calculation. I'm thinking about just not bothering to compute them at all.
    ;; I think the math here is correct, just woefully inefficient.
    
    (define use-torsion-forces #f)
    
    (define (torsion-force m n p q)
      (entering "torsion-force")
      (if use-torsion-forces
	  (let* ((ma (list-ref atom-list m))
		 (na (list-ref atom-list n))
		 (pa (list-ref atom-list p))
		 (qa (list-ref atom-list q))
		 (coeffs
		  (lookup-torsion-coeffs
		   (mm2-type (car ma))
		   (mm2-type (car na))
		   (mm2-type (car pa))
		   (mm2-type (car qa))))
		 (v1 (car coeffs))
		 (v2 (cadr coeffs))
		 (v3 (caddr coeffs))
		 (pv (vdiff (cadr ma) (cadr na)))
		 (qv (vdiff (cadr pa) (cadr na)))
		 (rv (vdiff (cadr qa) (cadr pa)))
		 (pp (dot-product pv pv))
		 (qq (dot-product qv qv))
		 (rr (dot-product rv rv))
		 (pq (dot-product pv qv))
		 (qr (dot-product qv rv))
		 (alpha (sqrt (/ (* pq pq) qq)))
		 (beta (sqrt (* qq qq)))
		 (gamma (sqrt (/ (* qr qr) qq)))
		 (vm1 (cross-product qv pv))
		 (vq1 (cross-product rv qv))
		 (vm2 (vscale vm1 (/ 1.0 (* (vlen vm1)))))
		 (vq2 (vscale vq1 (/ 1.0 (* (vlen vq1)))))
		 (w (safe-acos (dot-product vm2 vq2)))
		 (du-dw (* -0.0005 (+ (* v1 (sin w))
				      (* -2 v2 (sin (* 2 w)))
				      (* 3 v3 (sin (* 3 w))))))
		 (fm (vscale vm2 (/ du-dw
				    (sqrt (- pp (/ (* pq pq) qq))))))
		 (fq (vscale vq2 (/ du-dw
				    (sqrt (- rr (/ (* qr qr) qq)))))))
	    (add-to-force m fm)
	    (add-to-force q fq)
	    (add-to-force p
			  (vdiff (vscale fm (/ alpha beta))
				 (vscale fq (/ (+ gamma beta) beta))))
	    (add-to-force n
			  (vdiff (vscale fq (/ gamma beta))
				 (vscale fm (/ (+ alpha beta) beta)))))))
    
    ;; vdw is similar to length force
    ;; du/dr = 6*evdw*[(r/rvdw)^-7 - (r/rvdw)^-13]
    ;; Don't forget the factor of 0.001
    
    (define use-vdw-forces #t)
    
    (define (vdw-force m n)
      (entering "vdw-force")
      (if use-vdw-forces
	  (let* ((ma (list-ref atom-list m))  ;; members of atom-list
		 (na (list-ref atom-list n))
		 (evdw (* 0.5 (+ (cadr (list-ref periodic-table (car ma)))
				 (cadr (list-ref periodic-table (car na))))))
		 (rvdw (+ (caddr (list-ref periodic-table (car ma)))
			  (caddr (list-ref periodic-table (car na)))))
		 (rd (vdiff (cadr ma) (cadr na)))
		 (r (sqrt (dot-product rd rd)))
		 (rsq_recip (/ (* rvdw rvdw) (dot-product rd rd))) ;; (r/rvdw)^-2
		 (r_recip (sqrt rsq_recip))
		 (r6 (* rsq_recip rsq_recip rsq_recip))    ;; (r/rvdw)^-6
		 (du-dr (* 0.006 evdw r_recip r6 (- 1 (* 2 r6))))
		 (mult (/ (- du-dr) r)))
	    (add-to-force m (vscale rd mult))
	    (add-to-force n (vscale rd (- mult))))))
    
    (define need-to-resetup-terms #t)
    
    (define (compute-forces)
      (entering "compute-forces")
      (if need-to-resetup-terms
	  (let ()
	    (setup-terms)
	    (set! need-to-resetup-terms #f)))
      ;; first set all forces to zero, one force vector for each atom
      (set! force-list
	    (map (lambda (atm)
		   (vector 0.0 0.0 0.0))
		 atom-list))
      ;; next figure out contributions for each energy term
      (do ((L term-list (cdr L)))
	((null? L) ())
	(case (caar L)
	  ((length) (apply length-force (cdar L)))
	  ((angle) (apply angle-force (cdar L)))
	  ((torsion) (apply torsion-force (cdar L)))
	  ((vdw) (apply vdw-force (cdar L))))))
    
    ;; ******************************************************************
    ;; Building up a term list from an atom list and bond list
    
    (define (other-end bond atm)
      (cond ((= atm (cadr bond)) (caddr bond))
	    ((= atm (caddr bond)) (cadr bond))
	    (else #f)))
    
    (define whine-about-bond-count #f)
    
    (define (count-bonds n expected)
      (do ((dbonds 0)
	   (tbonds 0)
	   (sbonds 0)
	   (bond ())
	   (L bond-list (cdr L)))
	((null? L) (let ((nb (+ sbonds (* 2 dbonds) (* 3 tbonds))))
		     (if (not (= expected nb))
			 (set! whine-about-bond-count #t))
		     (list sbonds dbonds tbonds)))
	(set! bond (car L))
	(if (or (= n (cadr bond)) (= n (caddr bond)))
	    (case (car bond)
	      ((1) (set! sbonds (+ sbonds 1)))
	      ((2) (set! dbonds (+ dbonds 1)))
	      (else (set! tbonds (+ tbonds 1)))))))
    
    (define (setup-terms)
      ;; for each atom, figure out what element it's supposed to be,
      ;; count its bonds, verify against valence, and reassign its
      ;; periodic table index according to hybridization
      (do-to-all-atoms
       (lambda (n atm)
	 (let ((b ()))
	   (case (car atm)
	     ;; carbon
	     ((0 1 2 3) (set! b (count-bonds n 4))
	      (if (= (caddr b) 1)
		  (cons 3 (cdr atm))
		  (if (= (cadr b) 2)
		      (cons 2 (cdr atm))
		      (if (= (cadr b) 1)
			  (cons 1 (cdr atm))
			  (cons 0 (cdr atm))))))
	     ;; hydrogen
	     ((4) (set! b (count-bonds n 1))
		atm)
	     ;; oxygen
	     ((5 6) (set! b (count-bonds n 2))
	      (if (= (cadr b) 1)
		  (cons 6 (cdr atm))
		  (cons 5 (cdr atm))))
	     ;; nitrogen
	     (else (count-bonds n 3)
		   atm)))))
      (set! term-list ())
      ;; tally up length terms
      (do ((BL bond-list (cdr BL)))
	((null? BL) ())
	(set! term-list
	      (cons (list 'length (cadar BL) (caddar BL)) term-list)))
      ;; tally up angle terms
      (do ((AL atom-list (cdr AL))
	   (x 0 (+ x 1)))
	((null? AL) ())
	(do ((BL bond-list (cdr BL)))
	  ((null? BL) ())
	  (let ((y (other-end (car BL) x)))
	    (if y
		(do ((B2L bond-list (cdr B2L)))
		  ((null? B2L) ())
		  (let ((z (other-end (car B2L) y)))
		    (if (and z (> z x))
			(set! term-list
			      (cons (list 'angle x y z) term-list)))))))))
      ;; tally up the torsion terms
      (do ((AL atom-list (cdr AL))
	   (w 0 (+ w 1)))
	((null? AL) ())
	(do ((BL bond-list (cdr BL)))
	  ((null? BL) ())
	  (let ((x (other-end (car BL) w)))
	    (if x
		(do ((B2L bond-list (cdr B2L)))
		  ((null? B2L) ())
		  (let ((y (other-end (car B2L) x)))
		    (if (and y (not (= w y)))
			(do ((B3L bond-list (cdr B3L)))
			  ((null? B3L) ())
			  (let ((z (other-end (car B3L) y)))
			    (if (and z (not (= z x)) (> z w))
				(set! term-list
				      (cons (list 'torsion w x y z)
					    term-list))))))))))))
      ;; tally up the van der Waals terms (unbonded atom pairs)
      (do ((AL atom-list (cdr AL))
	   (x 0 (+ x 1)))
	((null? AL) ())
	(do ((A2L (cdr AL) (cdr A2L))
	     (y (+ x 1) (+ y 1))
	     (flag #t))
	  ((null? A2L) ())
	  (set! flag #t)
	  (do ((BL bond-list (cdr BL)))
	    ((null? BL) ())
	    (let ((p (other-end (car BL) x)))
	      (if (and p (= p y))
		  (let ()
		    (set! flag #f)
		    (set! BL '(4))))))
	  (if flag
	      (set! term-list
		    (cons (list 'vdw x y)
			  term-list))))))
    
    ;; ******************************************************************
    ;; Rotations and Centering
    
    (define (center-structure)
      (if (> (length atom-list) 0)
	  (let ((cog (vector 0.0 0.0 0.0))  ;; center of gravity
		(num-atoms 0))
	    (do ((L atom-list (cdr L)))
	      ((null? L) ())
	      (vector-set! cog 0
			   (+ (vector-ref cog 0)
			      (vector-ref (cadar L) 0)))
	      (vector-set! cog 1
			   (+ (vector-ref cog 1)
			      (vector-ref (cadar L) 1)))
	      (vector-set! cog 2
			   (+ (vector-ref cog 2)
			      (vector-ref (cadar L) 2)))
	      (set! num-atoms (+ num-atoms 1)))
	    (vector-set! cog 0
			 (/ (vector-ref cog 0) num-atoms))
	    (vector-set! cog 1
			 (/ (vector-ref cog 1) num-atoms))
	    (vector-set! cog 2
			 (/ (vector-ref cog 2) num-atoms))
	    (do ((L atom-list (cdr L)))
	      ((null? L) ())
	      (vector-set! (cadar L) 0
			   (- (vector-ref (cadar L) 0) (vector-ref cog 0)))
	      (vector-set! (cadar L) 1
			   (- (vector-ref (cadar L) 1) (vector-ref cog 1)))
	      (vector-set! (cadar L) 2
			   (- (vector-ref (cadar L) 2) (vector-ref cog 2)))))))
    
    (define (rotate-all-atoms-x-axis theta)
      (let ((ct (cos theta))
	    (st (sin theta))
	    (temp 0.0))
	(do ((L atom-list (cdr L)))
	  ((null? L) ())
	  (set! temp (- (* ct (vector-ref (cadar L) 1))
			(* st (vector-ref (cadar L) 2))))
	  (vector-set! (cadar L) 2
		       (+ (* ct (vector-ref (cadar L) 2))
			  (* st (vector-ref (cadar L) 1))))
	  (vector-set! (cadar L) 1 temp))))
    
    (define (rotate-all-atoms-y-axis theta)
      (let ((ct (cos theta))
	    (st (sin theta))
	    (temp 0.0))
	(do ((L atom-list (cdr L)))
	  ((null? L) ())
	  (set! temp (+ (* ct (vector-ref (cadar L) 0))
			(* st (vector-ref (cadar L) 2))))
	  (vector-set! (cadar L) 2
		       (- (* ct (vector-ref (cadar L) 2))
			  (* st (vector-ref (cadar L) 0))))
	  (vector-set! (cadar L) 0 temp))))
    
    (define negligible-angle-sq 0.00001)
    
    (define (rotate-structure xt yt)
      (if (> (* xt xt) negligible-angle-sq)
	  (rotate-all-atoms-y-axis xt))
      (if (> (* yt yt) negligible-angle-sq)
	  (rotate-all-atoms-x-axis yt)))
    
    (define (set-atom-position n x y z)
      (do ((L atom-list (cdr L))
	   (got-it #f))
	((cond ((null? L) #t)
	       ((= n 0) (vector-set! (cadar L) 0 x)
		(vector-set! (cadar L) 1 y)
		(vector-set! (cadar L) 2 z)
		; (set! n-ok #t)
		#t)
	       (else #f))
	 got-it)
	(set! n (- n 1))))
    
    (define (get-atom-position n x y z)
      (do ((L atom-list (cdr L))
	   (return-value #f))
	((cond ((null? L) #t)
	       ((= n 0) (set! return-value (cadar L))
		#t)
	       (else #f))
	 return-value)
	(set! n (- n 1))))
    
    
    ;; ******************************************************************
    ;; Conversion, screen coordinates <=> angstroms
    
    (define scale-factor 25.0)
    
    (define (set-scale-factor x) (set! scale-factor x))
    (define (su2a x) (/ x scale-factor))
    (define (a2su x) (* x scale-factor))
    
    ;; ******************************************************************
    ;; Add/delete/select atoms and bonds
    
    (define (remove-from-list L n)
      (cond ((null? L) ())
	    ((= n 0) (cdr L))
	    (else (cons (car L)
			(remove-from-list (cdr L) (- n 1))))))
    
    (define (remove-from-list-if-test L test)
      (cond ((null? L) ())
	    ((apply test (list (car L)))
	     (remove-from-list-if-test (cdr L) test))
	    (else (cons (car L)
			(remove-from-list-if-test (cdr L) test)))))
    
    (define (add-bond m n order)
      (if (not (= m n))
	  (let ()
	    (define need-to-resetup-terms #t)
	    (delete-bond m n)
	    (set! bond-list (cons (list order m n) bond-list)))))
    
    (define (delete-bond m n)
      (set! need-to-resetup-terms #t)
      (set! bond-list
	    (remove-from-list-if-test
	     bond-list
	     (lambda (bond) (or (and (= m (cadr bond))
				     (= n (caddr bond)))
				(and (= n (cadr bond))
				     (= m (caddr bond))))))))
    
    (define (add-atom x y e)
      (set! need-to-resetup-terms #t)
      (set! atom-list
	    (append
	     atom-list
	     (list
	      (list e (vector (su2a x) (su2a y) 0.0)))))
      (set! force-list
	    (append
	     force-list
	     (list
	      (vector 0.0 0.0 0.0)))))
    
    (define (delete-atom n)
      (set! need-to-resetup-terms #t)
      (set! atom-list (remove-from-list atom-list n))
      (set! force-list (remove-from-list force-list n))
      (set! bond-list (remove-from-list-if-test
		       bond-list
		       (lambda (bond) (or (= n (cadr bond))
					  (= n (caddr bond))))))
      (set! bond-list
	    (map
	     (lambda (bond)
	       (if (> (cadr bond) n)
		   (set! bond (list (car bond)
				    (- (cadr bond) 1)
				    (caddr bond))))
	       (if (> (caddr bond) n)
		   (set! bond (list (car bond)
				    (cadr bond)
				    (- (caddr bond) 1))))
	       bond)
	     bond-list)))
    
    (define (select-atom x y)
      (set! x (su2a x))
      (set! y (su2a y))
      (do ((n #f)
	   (i 0 (+ i 1))
	   (p 0.0)
	   (sq-dist 0.0)
	   (min-sq-dist 0.0)
	   (L atom-list (cdr L)))
	((null? L) n)
	(set! p (- x (vector-ref (cadar L) 0)))
	(set! sq-dist (* p p))
	(set! p (- y (vector-ref (cadar L) 1)))
	(set! sq-dist (+ sq-dist (* p p)))
	(if (or (not n) (< sq-dist min-sq-dist))
	    (let ()
	      (set! min-sq-dist sq-dist)
	      (set! n i)))))
    
    (define (move-atom n x y)
      (define (move-helper n x y lst)
	(cond ((null? lst) ())
	      ((= n 0) (cons
			(let ((atm (car lst)))
			  (list (car atm)
				(vector (su2a x)
					(su2a y)
					(vector-ref (cadr atm) 2))))
			(cdr lst)))
	      (else (cons (car lst)
			  (move-helper (- n 1) x y (cdr lst))))))
      (set! atom-list
	    (move-helper n x y atom-list)))
    
    ;; ******************************************************************
    ;; Drawing lists
    
    ;; For wireframe drawing lists, we want only to return an unordered list
    ;; of bonds, and we can throw away information about bond order. This
    ;; should be very quick, so we can draw wireframes while rotating a
    ;; molecule smoothly.
    
    (define (wireframe-drawing-list)
      (entering "wireframe-drawing-list")
      (do ((L bond-list (cdr L))
	   (drawing-list ())
	   (m 0)
	   (n 0))
	((null? L) drawing-list)
	(set! m (cadar L))
	(set! n (caddar L))
	(set! drawing-list
	      (cons
	       (list (a2su (vector-ref (cadr (list-ref atom-list m)) 0))
		     (a2su (vector-ref (cadr (list-ref atom-list m)) 1))
		     (a2su (vector-ref (cadr (list-ref atom-list n)) 0))
		     (a2su (vector-ref (cadr (list-ref atom-list n)) 1)))
	       drawing-list))))
    
    ;; To create detailed drawing lists, we want to specify an order in which
    ;; things are drawn, with the most-positive-z-value things drawn first, and
    ;; the more-negative-z-value things drawn on top of them (the Painter's
    ;; algorithm) for crude depth rendering. To do this we use a data structure,
    ;; a list of lists, each inner list containing a boolean, an integer, and a
    ;; z-value. The boolean tells whether this object is in the atom list or the
    ;; bond list, the integer indexes into that list, and the z value represents
    ;; either an atomic nucleus or the midpoint of a bond.
    
    (define (bubble-1 criterion lst)
      (cond ((not (pair? lst)) lst)
	    ((null? (cdr lst)) lst)
	    (else (let ((a (car lst))
			(b (cadr lst)))
		    (if (apply criterion (list a b))
			(cons a (bubble-1 criterion (cdr lst)))
			(cons b (bubble-1 criterion (cons a (cddr lst)))))))))
    
    (define (bubble-sort criterion lst)
      (cond ((null? lst) ())
	    (else (set! lst (reverse (bubble-1 criterion (reverse lst))))
		  (cons (car lst)
			(bubble-sort criterion (cdr lst))))))
    
    (define (detailed-drawing-list)
      (entering "detailed-drawing-list")
      (let ((DL ()))
	(do ((L bond-list (cdr L))
	     (i 0 (+ i 1)))
	  ((null? L) ())
	  (set! DL
		(cons
		 (list
		  #f i
		  (* 0.5
		     (+ (vector-ref (cadr (list-ref atom-list (cadar L))) 2)
			(vector-ref (cadr (list-ref atom-list (caddar L))) 2))))
		 DL)))
	(do ((L atom-list (cdr L))
	     (i 0 (+ i 1)))
	  ((null? L) ())
	  (set! DL (cons (list #t i (vector-ref (cadar L) 2))
			 DL)))
	(set! DL
	      (bubble-sort (lambda (x y)
			     (> (caddr x) (caddr y)))
			   DL))
	(map (lambda (widget)
	       (if (car widget)
		   (let* ((atm (list-ref atom-list (cadr widget)))
			  (atm-pos (cadr atm))
			  (atm-force (list-ref force-list (cadr widget))))
		     (list #t
			   (a2su (vector-ref atm-pos 0))
			   (a2su (vector-ref atm-pos 1))
			   (car atm)
			   (* 0.05 (a2su (vector-ref atm-force 0)))
			   (* 0.05 (a2su (vector-ref atm-force 1)))))
		   (let* ((bnd (list-ref bond-list (cadr widget)))
			  (pos1 (cadr (list-ref atom-list (cadr bnd))))
			  (pos2 (cadr (list-ref atom-list (caddr bnd)))))
		     (list #f
			   (a2su (vector-ref pos1 0))
			   (a2su (vector-ref pos1 1))
			   (a2su (vector-ref pos2 0))
			   (a2su (vector-ref pos2 1))
			   (car bnd)))))
	     DL)))
    
    ;; ******************************************************************
    ;; Loading and saving structures
    
    (define (get-structure)
      (list (map (lambda (z)
		   (list (car z)
			 (vector-ref (cadr z) 0)
			 (vector-ref (cadr z) 1)
			 (vector-ref (cadr z) 2)))
		 atom-list)
	    bond-list))
    
    (define (set-structure s)
      (set! atom-list (map (lambda (z)
			     (list (car z)
				   (vector (cadr z) (caddr z) (cadddr z))))
			   (car s)))
      (set! bond-list (cadr s)))
    
    ;; ******************************************************************
    ;; Potential energy minimization
    
    ;; We compute force vectors, and then scale them until to be displacement
    ;; vectors, where the largest displacement has a magnitude equal to
    ;; emin-factor (which is in angstroms).
    
    ;; currently, these represent angstroms
    (define fast-emin-factor 0.2)
    (define stable-emin-factor 0.01)
    
    (define emin-factor stable-emin-factor)
    
    (define (emin-step)
      (entering "emin-sub-step")
      (set! whine-about-bond-count #f)
      (setup-terms)
      (do ((i 0 (+ i 1)))
	((= i 25) ())
	(compute-forces)
	(do-to-all-atoms
	 (lambda (n atm)
	   (let ((f (list-ref force-list n)))
	     (list (car atm)
		   (vplus (cadr atm)
			  (vscale f (/ emin-factor (vlen f)))))))))
      (update-display #t #f)
      (if whine-about-bond-count
	  (error-msg "Mismatch between bonds and valences")))
    
    
    
    ;; ******************************************************************
    ;; ******************************************************************
    ;;                        THE REST IS GUI CODE
    ;; ******************************************************************
    ;; ******************************************************************
    
    (define center-x #f)
    (define center-y #f)
    (define start-mouse ())
    (define selected-atom 0)
    (define current-element 0)
    (define bond-order 1)
    (define atom-drawing-radius 10)
    (define draw-force-vectors #f)
    (define current-mouse-button #f)
    
    (define (select-an-atom x y)
      (set! selected-atom
	    (select-atom (- x center-x)
			 (- y center-y))))
    
    (define (rotate-press x y)
      (center-structure)
      (set! start-mouse (list x y)))
    
    (define (rotate-drag x y)
      (rotate-structure
       (* 0.01 (- x (car start-mouse)))
       (* -0.01 (- y (cadr start-mouse))))
      (set! start-mouse (list x y))
      (update-display #f #t))
    
    (define (rotate-release x y)
      (update-display #t #t))
    
    (define (move-drag x y)
      (move-atom selected-atom
		 (- x center-x)
		 (- y center-y))
      (update-display #t #t))
    
    (define (addatom-press x y)
      (let ((x1 (- x center-x))
	    (y1 (- y center-y)))
	(add-atom (- x center-x)
		  (- y center-y)
		  current-element)
	(update-display #t #f)))
    
    (define (deleteatom-press x y)
      (select-an-atom x y)
      (delete-atom selected-atom)
      (update-display #t #f))
    
    (define (deletebond-release x y)
      (let ((n selected-atom))
	(select-an-atom x y)
	(delete-bond n selected-atom)
	(update-display #t #f)))
    
    (define (addbond-release x y)
      (let ((n selected-atom))
	(select-an-atom x y)
	(if (not (= n selected-atom))
	    (add-bond n selected-atom bond-order))
	(update-display #t #f)))
    
    (define (do-nothing x y) ())
    
    (define press-function rotate-press)
    (define drag-function rotate-drag)
    (define release-function rotate-release)
    
    ;; For now, pay attention only to the left mouse button
    
    (define (press-function-b x y)
      (if (eq? current-mouse-button 1)
	  (press-function x y)))
    
    (define (drag-function-b x y)
      (if (eq? current-mouse-button 1)
	  (drag-function x y)))
    
    (define (release-function-b x y)
      (if (eq? current-mouse-button 1)
	  (release-function x y)))
    
    
    (define my-frame% mred:frame%)
    
    (define my-canvas%
      (make-class mred:canvas%
		  (private
		    (which-button 0))
		  (rename [super-on-paint on-paint])
		  (public
		    (on-paint
		     (lambda ()
		       (update-display #t #f)
		       (super-on-paint)))
		    (on-event
		     (lambda (event)
		       (let ((which-button
			      (cond ((send event button? 1) 1)
				    ((send event button? 2) 2)
				    (else 3)))
			     (x (send event get-x))
			     (y (send event get-y)))
			 (cond ((send event button-down? -1)
				(set! current-mouse-button which-button)
				(press-function-b x y))
			       ((send event button-up? -1)
				(release-function-b x y)
				(set! current-mouse-button #f))
			       ((and current-mouse-button
				     (send event dragging?))
				(drag-function-b x y))
			       (else #f))))))))
    
    (define this-session ())
    
    (define (update-display full-blown smooth)
      (update-session full-blown
		      smooth
		      (send (ivar this-session canvas) get-width)
		      (send (ivar this-session canvas) get-height)
		      (ivar this-session canvas-dc)
		      (ivar this-session atom-color)
		      (ivar this-session select-pen)))
    
    (define session%
      (class () ()
	(sequence
	  (set! this-session this))
	(public
	  (FRAME-WIDTH 500)
	  (PANEL-HEIGHT 200)
	  (CANVAS-HEIGHT 300)
	  (a-frame
	   (make-object my-frame%
			'() ; No parent frame
			"NanoCAD v0.2" ; The frame's title
			-1 -1 ; Use the default position
			FRAME-WIDTH (+ PANEL-HEIGHT CANVAS-HEIGHT)))
	  (main-panel
	   (make-object mred:vertical-panel% a-frame))
	  (b-panel
	   (make-object mred:horizontal-panel% main-panel))
	  (c-panel
	   (make-object mred:horizontal-panel% main-panel))
	  (rb-panel
	   (let ([p (make-object mred:horizontal-panel% main-panel)])
	     (send p set-label-position wx:const-vertical)
	     p))
	  (canvas
	   (make-object my-canvas% main-panel))
	  (canvas-dc
	   (send canvas get-dc)))
	
	(private
	  (internal-update
	   (lambda (full-blown)
	     (update-session full-blown
			     #f
			     (send canvas get-width)
			     (send canvas get-height)
			     canvas-dc
			     atom-color
			     select-pen)))
	  (carbon-brush
	   (make-object wx:brush% "BLACK" wx:const-solid))
	  (hydrogen-brush
	   (make-object wx:brush% "WHITE" wx:const-solid))
	  (oxygen-brush
	   (make-object wx:brush% "RED" wx:const-solid))
	  (nitrogen-brush
	   (make-object wx:brush% "BLUE" wx:const-solid))
	  
	  (normal-pen
	   (make-object wx:pen% "BLACK" 1 wx:const-solid))
	  (double-bond-pen
	   (make-object wx:pen% "BLACK" 3 wx:const-solid))
	  (triple-bond-pen
	   (make-object wx:pen% "BLACK" 5 wx:const-solid))
	  (force-vector-pen
	   (make-object wx:pen% "RED" 1 wx:const-solid))
	  
	  (load-button
	   (make-object mred:button%
			b-panel
			(lambda (self event)
			  (let ((file-name (wx:file-selector "")))
			    (if (not (null? file-name))
				(let ((inf (open-input-file file-name)))
				  (set-structure (read inf))
				  (close-input-port inf)
				  (set! need-to-resetup-terms #t)
				  (internal-update #t)))))
			"Load"))
	  (save-button
	   (make-object mred:button%
			b-panel
			(lambda (self event)
			  (let ((file-name (wx:file-selector "")))
			    (if (not (null? file-name))
				(let ()
				  (delete-file file-name)
				  (let ((outf (open-output-file file-name))
					(s (get-structure)))
				    (fprintf outf "((~%")
				    (map (lambda (atm)
					   (fprintf outf "  ~s~%" atm))
					 (car s))
				    (fprintf outf " )~% (~%")
				    (map (lambda (atm)
					   (fprintf outf "  ~s~%" atm))
					 (cadr s))
				    (fprintf outf "))~%")
				    (close-output-port outf)
				    (internal-update #t))))))
			"Save"))
	  (save-xyz-button
	   (make-object mred:button%
			b-panel
			(lambda (self event)
			  (let ((file-name (wx:file-selector "")))
			    (if (not (null? file-name))
				(let ()
				  (delete-file file-name)
				  (let ((outf (open-output-file file-name)))
				    (fprintf outf "~a~%Gray Goo~%"
					     (length atom-list))
				    (do ((L atom-list (cdr L)))
				      ((null? L) ())
				      (fprintf outf "~a ~a ~a ~a~%"
					       (car
						(list-ref periodic-table
							  (caar L)))
					       (vector-ref (cadar L) 0)
					       (vector-ref (cadar L) 1)
					       (- (vector-ref (cadar L) 2))))
				    (close-output-port outf)
				    (internal-update #t))))))
			"SaveXYZ"))
	  (clear-button
	   (make-object mred:button%
			b-panel
			(lambda (self event)
			  (set! atom-list ())
			  (set! bond-list ())
			  (set! term-list ())
			  (internal-update #t))
			"Clear"))
	  (emin-button
	   (make-object mred:button%
			b-panel
			(lambda (self event)
			  (emin-step))
			"Emin"))
	  (quit-button
	   (make-object mred:button%
			b-panel
			(lambda (self event)
			  (send canvas-dc end-drawing)
			  (send a-frame show #f))
			"Quit")))
	(private
	  (show-forces-checkbox
	   (make-object mred:check-box%
			c-panel
			(lambda (self event)
			  (set! draw-force-vectors (send event checked?))
			  (internal-update #t))
			"Show Force Vectors"))
	  (use-torsion-checkbox
	   (make-object mred:check-box%
			c-panel
			(lambda (self event)
			  (set! use-torsion-forces (send event checked?)))
			"Use Torsion Forces"))
	  (use-vdw-checkbox
	   (make-object mred:check-box%
			c-panel
			(lambda (self event)
			  (set! use-vdw-forces (send event checked?)))
			"Use VDW Forces")))
	(public
	  (select-pen
	   (lambda (dc n)
	     (case n
	       ((0) (send dc set-pen normal-pen))
	       ((1) (send dc set-pen force-vector-pen))
	       ((2) (send dc set-pen double-bond-pen))
	       ((3) (send dc set-pen triple-bond-pen)))))
	  (atom-color
	   (lambda (dc element)
	     (case element
	       ((0 1 2 3) (send dc set-brush carbon-brush))
	       ((4)         (send dc set-brush hydrogen-brush))
	       ((5 6)     (send dc set-brush oxygen-brush))
	       (else      (send dc set-brush nitrogen-brush))))))
	(private
	  (rb-sub-panel%
	   (class mred:panel% args
	     (inherit set-label-position)
	     (sequence
	       (apply super-init args)
	       (set-label-position wx:const-vertical))))
	  (set-mode
	   (lambda (n)
	     (case n
	       ((0) (set! press-function rotate-press)
		  (set! drag-function rotate-drag)
		  (set! release-function rotate-release))
	       ((1) (set! press-function select-an-atom)
		  (set! drag-function move-drag)
		  (set! release-function do-nothing))
	       ((2) (set! press-function addatom-press)
		  (set! drag-function do-nothing)
		  (set! release-function do-nothing))
	       ((3) (set! press-function deleteatom-press)
		  (set! drag-function do-nothing)
		  (set! release-function do-nothing))
	       ((4) (set! press-function select-an-atom)
		  (set! drag-function do-nothing)
		  (set! release-function addbond-release))
	       ((5) (set! press-function select-an-atom)
		  (set! drag-function do-nothing)
		  (set! release-function deletebond-release)))))
	  (mode-selector
	   (make-object mred:radio-box%
			(make-object rb-sub-panel% rb-panel)
			(lambda (self event)
			  (let ((n (send event get-command-int)))
			    (set-mode n)))
			"Tool"
			-1 -1 -1 -1
			(list "Rotate" "MoveAtom" "AddAtom" "DeleteAtom"
			      "AddBond" "DeleteBond")))
	  (element-selector
	   (make-object mred:radio-box%
			(make-object rb-sub-panel% rb-panel)
			(lambda (self event)
			  (send mode-selector set-selection 2)
			  (set-mode 2)
			  (let ((n (send event get-command-int)))
			    (case n
			      ((0) (set! current-element 0))
			      ((1) (set! current-element 4))
			      ((2) (set! current-element 5))
			      (else (set! current-element 7)))))
			"Atom"
			-1 -1 -1 -1
			(list "Carbon" "Hydrogen" "Oxygen" "Nitrogen")))
	  (bond-order-selector
	   (make-object mred:radio-box%
			(make-object rb-sub-panel% rb-panel)
			(lambda (self event)
			  (send mode-selector set-selection 4)
			  (set-mode 4)
			  (let ((n (send event get-command-int)))
			    (set! bond-order (+ n 1))))
			"Bond"
			-1 -1 -1 -1
			(list "Single" "Double" "Triple")))
	  (zoom-factor
	   (make-object mred:radio-box%
			(make-object rb-sub-panel% rb-panel)
			(lambda (self event)
			  (let ((n (send event get-command-int)))
			    (case n
			      ((0) (set-scale-factor 10.0))
			      ((1) (set-scale-factor 25.0))
			      ((2) (set-scale-factor 50.0))
			      (else (set-scale-factor 100.0))))
			  (set! atom-drawing-radius (* 0.6 scale-factor))
			  (internal-update #t))
			"Zoom"
			-1 -1 -1 -1
			(list "10" "25" "50" "100")))
	  (emin-convergence
	   (make-object mred:radio-box%
			(make-object rb-sub-panel% rb-panel)
			(lambda (self event)
			  (let ((n (send event get-command-int)))
			    (case n
			      ((0) (set! emin-factor stable-emin-factor))
			      (else (set! emin-factor fast-emin-factor)))))
			"Emin"
			-1 -1 -1 -1
			(list "Stable" "Fast"))))
	(sequence
	  (send main-panel border 0)
	  (for-each
	   (lambda (panel)
	     (send panel stretchable-in-y #f)
	     (send panel border 0))
	   (list b-panel c-panel rb-panel))
	  (set! error-msg
		(lambda (txt)
		  (send canvas-dc draw-text txt 10 10)))
	  (set-scale-factor 25.0)
	  (send zoom-factor set-selection 1)
	  (send use-vdw-checkbox set-value #t)
	  (send a-frame show #t))))
    
    (define offscreen-dc (make-object wx:memory-dc%))
    (define offscreen-dc-width -1)
    (define offscreen-dc-height -1)
    
    (define (update-session full-blown
			    smooth
			    canvas-width
			    canvas-height
			    canvas-dc
			    atom-color
			    select-pen)
      (when (and smooth (not (and (= canvas-width offscreen-dc-width)
				  (= canvas-height offscreen-dc-height))))
	(let ([bm (make-object wx:bitmap% canvas-width canvas-height)])
	  (send offscreen-dc select-object bm)
	  (set! offscreen-dc-width canvas-width)
	  (set! offscreen-dc-height canvas-height)))
      (let ([draw-dc (if (and smooth (send offscreen-dc ok?)) offscreen-dc canvas-dc)])
	(set! center-x (* 0.5 canvas-width))
	(set! center-y (* 0.5 canvas-height))
	(send draw-dc clear)
	(if (or #t full-blown)
	    (let ((DL ())
		  (minus-half-radius (* -0.5 atom-drawing-radius)))
	      (if draw-force-vectors (compute-forces))
	      (if (< (length force-list) (length atom-list))
		  (set! force-list
			(map (lambda (atm)
			       (vector 0.0 0.0 0.0))
			     atom-list)))
	      (set! DL (detailed-drawing-list))
	      (map (lambda (z)
		     (if (car z)
			 (let ()  ;; for atoms, let's do both circles and forces
			   (atom-color draw-dc (cadddr z))
			   (send draw-dc draw-ellipse
				 (+ (cadr z) center-x minus-half-radius)
				 (+ (caddr z) center-y minus-half-radius)
				 atom-drawing-radius atom-drawing-radius)
			   (if draw-force-vectors
			       (let ()
				 (select-pen draw-dc 1)
				 (send draw-dc draw-line
				       (+ (cadr z) center-x)
				       (+ (caddr z) center-y)
				       (+ (cadr z) (cadr (cdddr z)) center-x)
				       (+ (caddr z) (caddr (cdddr z)) center-y))
				 (select-pen draw-dc 0))))
			 (let ()
			   (case (cadddr (cddr z))
			     ((1) (select-pen draw-dc 0))
			     ((2) (select-pen draw-dc 2))
			     (else (select-pen draw-dc 3)))
			   (send draw-dc draw-line
				 (+ (cadr z) center-x)
				 (+ (caddr z) center-y)
				 (+ (cadddr z) center-x)
				 (+ (cadddr (cdr z)) center-y))
			   (select-pen draw-dc 0))))
		   DL))
	    (map (lambda (z)
		   (send draw-dc draw-line
			 (+ (car z) center-x)
			 (+ (cadr z) center-y)
			 (+ (caddr z) center-x)
			 (+ (cadddr z) center-y)))
		 (wireframe-drawing-list)))
	(if smooth
	    (send canvas-dc blit 0 0 canvas-width canvas-height offscreen-dc 0 0))))
    
    (make-object session%)))

(define (ncad:go)
  (invoke-unit/sig ncad@ mred:wx^ (mred : mred^)))



