#lang typed/racket

(provide (all-defined-out))
(require "Float3.rkt")

(struct Camera
  ((eye : Float3)
   (forward : Float3)
   (right : Float3)
   (up : Float3)))

; Get the "global up" vector, usually normal to the ground.
(: Camera-global-up (-> Float3))
(define (Camera-global-up)
  (Float3 0.0 1.0 0.0))

; Get the vector from the camera eye through the imaginary image plane
; at a given percent offset from the top left corner.
(: Camera-get-ray-direction (-> Camera Float Float Float3))
(define (Camera-get-ray-direction camera xx yy)
  (define top-left
    (Float3-add (Camera-forward camera)
                (Float3-add (Camera-up camera)
                            (Float3-negate (Camera-right camera)))))
  (define right-component
    (Float3-scale-by-scalar (Camera-right camera) (* 2.0 xx)))
  (define down-component
    (Float3-scale-by-scalar (Camera-up camera) (* (- 2.0) yy)))
  (Float3-normalized
   (Float3-add top-left
               (Float3-add right-component down-component))))

; Create a camera looking at a point with a particular orientation.
(: Camera-look-at (-> Float3 Float3 Float3 Float Float Camera))
(define (Camera-look-at eye focus up aspect zoom)
  (define new-forward
    (Float3-scale-by-scalar (Float3-normalized (Float3-subtract focus eye))
                            zoom))
  (define new-right
    (Float3-scale-by-scalar (Float3-normalized (Float3-cross new-forward up))
                            aspect))
  (define new-up
    (Float3-normalized (Float3-cross new-forward new-right)))
  (Camera eye new-forward new-right new-up))
