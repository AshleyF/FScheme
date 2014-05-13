(define red '(255 0 0))
(define green '(0 255 0))
(define blue '(0 0 255))

(define ballworld (lambda (fn world)
	(let ((ball (car world))
          (color (cadr world)))
		(fn (car ball) (cadr ball) ; position x/y
            (caddr ball) (cadddr ball) ; delta x/y
            (car color) (cadr color) (caddr color))))) ; red/green/blue
			
(define bounce (lambda (i di)
	(if (or (and (> di 0) (> i 31)) ; heading toward right/bottom and off edge
		    (and (> 0 di) (> 0 i))) ; or heading toward left/top and off edge
				(- di) di))) ; reverse direction

(define ballmove (lambda (ball)
	(ballworld (lambda (x y dx dy r g b) '((
		,(+ x dx) ,(+ y dy) ; increment position
		,(bounce x dx) ,(bounce y dy)) ; bounce off edges
		(,r ,g ,b))) ball))) ; color unchanged

(define balldraw (lambda (ball)
	(ballworld (lambda (x y dx dy r g b)
		'((,x ,y) (,r ,g ,b))) ball))) ; simply draw as single pixel at x/y position

(define init (lambda (_)
	'(((16 0 2 1) ,red)
	  ((6 9 1 -1) ,green)
      ((16 16 -1 2) ,blue))))
(define tick (lambda (input) (map ballmove (car input))))
(define draw (lambda (world) (map balldraw (car world))))