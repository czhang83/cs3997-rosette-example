#lang rosette/safe


(require rosette/lib/angelic    ; provides `choose*`
         rosette/lib/destruct)  ; provides `destruct`
; Tell Rosette we really do want to use integers.
(current-bitwidth #f)


; DSL Syntax
(struct plus (left right) #:transparent)
(struct mul (left right) #:transparent)
(struct square (arg) #:transparent)
(struct max2 (x y) #:transparent)


; Interpreter for DSL
; recurse on the syntax using pattern matching.
(define (interpret p)
  (destruct p
    [(plus a b)  (+ (interpret a) (interpret b))]
    [(mul a b)   (* (interpret a) (interpret b))]
    [(square a)  (expt (interpret a) 2)]
    [(max2 a b)  (if (> (interpret a) (interpret b)) (interpret a) (interpret b))]
    [_ p]))


(define prog (plus (max2 (square 8) 50) 3))
; (plus (max2 (square 8) 50) 3) evaluates to 67.
(interpret prog)


; Define a symbolic variable called y of type integer.
(define-symbolic y integer?)
(interpret (plus (max2 (square y) 50) 3))

; search for a `y` that makes (max ((y)^2, 50) + 3) = 84
; result is y = -9
(solve 
  (assert 
    (= (interpret (plus (max2 (square y) 50) 3)) 84)))


; Create an unknown expression -- one that can evaluate to several
; possible values.
(define (??expr terminals)
  (define a (apply choose* terminals))
  (define b (apply choose* terminals))
  (choose* (plus a b)
           (mul a b)
           (square a)
           (max a b)
           a))


; Create a sketch representing all programs of the form (plus ?? ??),
; where the ??s are unknown expressions created by ??expr.
(define-symbolic x p q integer?)
(define sketch
  (plus (??expr (list x p q)) (??expr (list x p q))))


; Solve the sketch to find a program equivalent to 10*x,
; but of the form (plus ?? ??). Save the resulting model.
(define M
  (synthesize
    #:forall (list x)
    #:guarantee (assert (= (interpret sketch) (interpret (mul 10 x))))))


; Substitute the bindings in M into the sketch to get back the
; synthesized program.
(evaluate sketch M)