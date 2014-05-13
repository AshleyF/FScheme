; logical 'and', 'or', 'not', 'xor'
(define and (macro (a b) '(if ,a (if ,b 1 0) 0)))
(define or (macro (a b) '(if ,a 1 (if ,b 1 0))))
(define not? (lambda (x) (if x 0 1)))
(define xor (lambda (a b) (and (or a b) (not? (and a b)))))

(define nil '())

; map function (f) over list (xs)
(define map (lambda (f xs)    ; apply f to each element of xs
  (if xs                      ; if not empty then
      (cons (f (car xs))      ; cons f of the head...
            (map f (cdr xs))) ; onto result of recursing down the tail
      nil)))                  ; otherwise return empty

(define list (macro (xs) '(map eval (quote ,xs))))

; fold function (f) over list (xs) while accumulating (a)
(define fold (lambda (f a xs)
  (if (not? xs) a
      (fold f (f (car xs) a) (cdr xs)))
  ))

(define reverse (lambda (xs) (fold cons nil xs)))

(define newline (lambda () (display "\r\n")))

(define while
  (macro (test body)
    '(letrec
      ((loop
         (lambda ()
           (if ,test
             (begin ,body (loop))
             nil))))
         (loop))))

; simple continuation to top-level
(define escape nil)
(call/cc (lambda (c) (set! escape c)))

; error mechanism - print message and break out to top-level
(define error (lambda (msg) (begin (display msg) (escape nil))))

(define sum (lambda (xs) (fold + 0 xs)))

(define odd? (lambda (x) (% x 2)))
(define even? (lambda (x) (not? (odd? x))))

(define require (lambda (e) (if e e (amb))))

(define member? (lambda (item lst)
     (if lst
         (if (= item (car lst))
             1
             (member? item (cdr lst)))
         0)))

(define distinct? (lambda (lst)
    (if lst
         (if (member? (car lst) (cdr lst))
             0
             (distinct? (cdr lst)))
         1)))

(define exclude (lambda (items lst)
     (if lst
         (if (member? (car lst) items)
             (exclude items (cdr lst))
             (cons (car lst) (exclude items (cdr lst))))
         ())))

(define >= (lambda (a b) (or (> a b) (= a b))))
(define <= (lambda (a b) (or (< a b) (= a b))))

(define cadr (lambda (xs) (car (cdr xs))))
(define caddr (lambda (xs) (cadr (cdr xs))))
(define cadddr (lambda (xs) (caddr (cdr xs))))
(define caddddr (lambda (xs) (cadddr (cdr xs))))
(define cadddddr (lambda (xs) (caddddr (cdr xs))))
(define caddddddr (lambda (xs) (cadddddr (cdr xs))))

(define fst car)
(define snd cadr)