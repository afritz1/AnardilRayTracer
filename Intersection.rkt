#lang typed/racket

(provide (all-defined-out))
(require "Float3.rkt")

; An intersection is built with how far the intersecting ray had to travel (t),
; the intersection point, the normal at the intersection, and the material (in
; this case, it's just a solid color).
(struct Intersection
  ((t : Float)
   (point : Float3)
   (normal : Float3)
   (color : Float3)))

; Max allowed range for an intersection.
(define INTERSECTION-T-MAX 1.0e300)

; Creates an empty intersection with no hit data.
(define (Intersection-empty)
  (Intersection INTERSECTION-T-MAX (Float3 0.0 0.0 0.0)
                (Float3 0.0 0.0 0.0) (Float3 0.0 0.0 0.0)))
