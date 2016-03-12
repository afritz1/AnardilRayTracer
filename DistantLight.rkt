#lang typed/racket

(provide (all-defined-out))
(require "Float3.rkt")

; A distant light is a simplification of a light source; its direction is
; constant throughout the scene because it is sufficiently far away enough,
; and the calculation is slightly faster than with a point light.
(struct DistantLight
  ((direction : Float3)
   (color : Float3)))
