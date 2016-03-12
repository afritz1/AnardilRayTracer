#lang typed/racket

(provide (all-defined-out))
(require racket/flonum)

; Vector type for 3D operations.
(struct Float3
  ((x : Float)
   (y : Float)
   (z : Float)))

; Get a random color vector with components [0.0, 1.0).
(: Float3-random-color (-> Float3))
(define (Float3-random-color)
  (Float3 (random) (random) (random)))

; Get the length squared of a Float3.
(: Float3-length-squared (-> Float3 Float))
(define (Float3-length-squared v)
  (+ (* (Float3-x v) (Float3-x v))
     (* (Float3-y v) (Float3-y v))
     (* (Float3-z v) (Float3-z v))))

; Get the length of a Float3.
(: Float3-length (-> Float3 Float))
(define (Float3-length v)
  (flsqrt (Float3-length-squared v)))

; Get a normalized copy of the given vector.
(: Float3-normalized (-> Float3 Float3))
(define (Float3-normalized v)
  (let ((length-reciprocal (/ 1.0 (Float3-length v))))
    (Float3 (* (Float3-x v) length-reciprocal)
            (* (Float3-y v) length-reciprocal)
            (* (Float3-z v) length-reciprocal))))

; Get the dot product of two Float3's.
(: Float3-dot (-> Float3 Float3 Float))
(define (Float3-dot v1 v2)
  (+ (* (Float3-x v1) (Float3-x v2))
     (* (Float3-y v1) (Float3-y v2))
     (* (Float3-z v1) (Float3-z v2))))

; Get the cross product of two Float3's.
(: Float3-cross (-> Float3 Float3 Float3))
(define (Float3-cross v1 v2)
  (Float3 (- (* (Float3-y v1) (Float3-z v2))
             (* (Float3-z v1) (Float3-y v2)))
          (- (* (Float3-x v1) (Float3-z v2))
             (* (Float3-z v1) (Float3-x v2)))
          (- (* (Float3-x v1) (Float3-y v2))
             (* (Float3-y v1) (Float3-x v2)))))

; Get the sum of two Float3's.
(: Float3-add (-> Float3 Float3 Float3))
(define (Float3-add v1 v2)
  (Float3 (+ (Float3-x v1) (Float3-x v2))
          (+ (Float3-y v1) (Float3-y v2))
          (+ (Float3-z v1) (Float3-z v2))))

; Get the difference of two Float3's
(: Float3-subtract (-> Float3 Float3 Float3))
(define (Float3-subtract v1 v2)
  (Float3 (- (Float3-x v1) (Float3-x v2))
          (- (Float3-y v1) (Float3-y v2))
          (- (Float3-z v1) (Float3-z v2))))

; Negate (reverse) a Float3.
(: Float3-negate (-> Float3 Float3))
(define (Float3-negate v)
  (Float3 (- (Float3-x v))
          (- (Float3-y v))
          (- (Float3-z v))))

; Scale a Float3 by a scalar multiple.
(: Float3-scale-by-scalar (-> Float3 Float Float3))
(define (Float3-scale-by-scalar v m)
  (Float3 (* (Float3-x v) m)
          (* (Float3-y v) m)
          (* (Float3-z v) m)))

; Scale a Float3 by a vector multiple.
(: Float3-scale-by-vector (-> Float3 Float3 Float3))
(define (Float3-scale-by-vector v1 v2)
  (Float3 (* (Float3-x v1) (Float3-x v2))
          (* (Float3-y v1) (Float3-y v2))
          (* (Float3-z v1) (Float3-z v2))))

; Reflect a Float3 around a "normal" vector.
(: Float3-reflect (-> Float3 Float3 Float3))
(define (Float3-reflect view normal)
  (define vn-dot (Float3-dot view normal))
  (define vn-sign (if (> vn-dot 0.0) 1.0 -1.0))
  (define local-normal (Float3-scale-by-scalar normal vn-sign))
  (Float3-subtract (Float3-scale-by-scalar local-normal (* 2.0 vn-dot))
                   view))

; Get a random point within an imaginary sphere centered at the origin.
(: Float3-random-point-in-sphere (-> Float Float3))
(define (Float3-random-point-in-sphere radius)
  (define (random-negative-1-to-1)
    (- (* 2.0 (random)) 1.0))
  (let ((random-point-in-cube
         (Float3 (random-negative-1-to-1)
                 (random-negative-1-to-1)
                 (random-negative-1-to-1))))
    (Float3-scale-by-scalar (Float3-normalized random-point-in-cube)
                            (* radius (random)))))

; Get a random direction in a hemisphere, given a normal.
(: Float3-random-hemisphere-direction (-> Float3 Float3))
(define (Float3-random-hemisphere-direction normal)  
  ; Get a random direction in a sphere and normalize it.
  (define random-direction
    (Float3-normalized (Float3-random-point-in-sphere 1.0)))
  
  ; If the random direction is facing the wrong way, flip it.
  (if (>= (Float3-dot random-direction normal) 0.0)
      random-direction
      (Float3-negate random-direction)))

; Return a Float3 with its components clamped between two values.
(: Float3-clamped (-> Float3 Float Float Float3))
(define (Float3-clamped v low high)
  (Float3 (min high (max low (Float3-x v)))
          (min high (max low (Float3-y v)))
          (min high (max low (Float3-z v)))))

; Convert a Float3 to ARGB color format for display. The "255" value
; indicates full opacity in the alpha channel.
(: Float3-to-argb (-> Float3 Bytes))
(define (Float3-to-argb v)
  (bytes 255
         (exact-round (* 255.0 (Float3-x v)))
         (exact-round (* 255.0 (Float3-y v)))
         (exact-round (* 255.0 (Float3-z v)))))
