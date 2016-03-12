#lang typed/racket

(provide (all-defined-out))
(require "Float3.rkt")

; A ray is a point and a normalized direction. In more complex ray tracers,
; there would also be a "depth" integer that increments each time the ray
; bounces. The ray would terminate once it hits a non-reflective material or 
; if a maximum depth is reached.
(struct Ray
  ((point : Float3)
   (direction : Float3)))

; Get the point in space at some distance along the ray (p2 = p1 + v*t).
(: Ray-point-at (-> Ray Float Float3))
(define (Ray-point-at ray distance)
  (Float3-add (Ray-point ray)
              (Float3-scale-by-scalar (Ray-direction ray)
                                      distance)))
