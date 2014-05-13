(define pongworld (lambda (fn world)
	(let ((ball (car world))
	      (mouse (cadr world)))
        (fn (car ball) (cadr ball) ; position x/y
            (caddr ball) (cadddr ball) ; delta x/y
            (cadr mouse))))) ; paddle position y

(define bounce (lambda (i di)
	(if (or (and (> di 0) (> i 30)) ; heading toward right/bottom and off edge
		    (and (> 0 di) (> 1 i))) ; or heading toward left/top and off edge
				(- di) di))) ; reverse direction

(define paddle (lambda (x y color) (map
	(lambda (d) '((,x ,(+ y d)) ,color))
	'(-2 -1 0 1 2))))

(define init (lambda (_) '(1 14 1 1 14)))

(define tick (lambda (world)
	(pongworld (lambda (x y dx dy p)
        (if (and (> x 30) ; off player's edge?
                 (or (< (+ y 3) p) (> (- y 3) p))) ; not hitting paddle?
           '(1 14 1 1 ,p) ; reset - except p, else move ball as usual...
           '(,(+ x dx) ,(+ y dy) ; increment position
		     ,(bounce x dx) ,(bounce y dy) ; bounce off edges
			 ,p
		))) world)))

(define draw (lambda (world)
	(let ((red '(255 0 0))
          (green '(0 255 0))
          (blue '(0 0 255))
          (p (caddddr (car world))))
        (pongworld (lambda (x y _ _ _) (cat (cat
            '(((,x ,y) ,green)) ; ball
            (paddle 0 y red)) ; computer's paddle
            (paddle 31 p blue) ; player's paddle
        )) world))))