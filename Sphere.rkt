#lang typed/racket

(provide (all-defined-out))
(require racket/flonum "Float3.rkt" "Intersection.rkt" "Ray.rkt")

; A sphere has a point and radius. In more complex ray tracers, the color
; member would be expanded to a "material" type with more variables.
(struct Sphere
  ((point : Float3)
   (radius : Float)
   (color : Float3)))

; Get the intersection data (if any) from a ray-sphere intersection by
; solving the quadratic formula.
(: Sphere-intersect (-> Sphere Ray Intersection))
(define (Sphere-intersect sphere ray)
  (define point-diff
    (Float3-subtract (Ray-point ray) (Sphere-point sphere)))
  (define a
    (Float3-dot (Ray-direction ray) (Ray-direction ray)))
  (define b
    (* 2.0 (Float3-dot (Ray-direction ray) point-diff)))
  (define c
    (- (Float3-dot point-diff point-diff)
       (* (Sphere-radius sphere) (Sphere-radius sphere))))
  (define discriminant
    (flsqrt (- (* b b) (* 4.0 a c))))

  ; t1 and t2 are the two solutions to the quadratic formula.  
  (define t
    (let* ((t1 (* (+ (- b) discriminant) (* 0.5 a)))
           (t2 (- t1 (/ discriminant a))))
      (if (> t2 0.0) t2
          (if (> t1 0.0) t1 INTERSECTION-T-MAX))))
  
  (define point
    (Ray-point-at ray t))
  (define normal
    (Float3-scale-by-scalar
     (Float3-subtract point (Sphere-point sphere))
     (/ 1.0 (Sphere-radius sphere))))
  (Intersection t point normal (Sphere-color sphere)))
