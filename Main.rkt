#lang typed/racket

; "AnardilRayTracer", by Aaron Fritz
; This is designed to be run from the command line. If running from 
; within DrRacket, remove the "(time (main))" function call at the 
; bottom.

(require typed/racket/draw "Camera.rkt" "DistantLight.rkt"
         "Float3.rkt" "Ray.rkt" "Sphere.rkt" "World.rkt")

(define SCREEN-WIDTH : Positive-Integer 1920)
(define SCREEN-HEIGHT : Positive-Integer 1080)
(define SCREEN-ASPECT : Float
  (exact->inexact (/ SCREEN-WIDTH SCREEN-HEIGHT)))

(define SPHERE-COUNT : Integer 20)

; The radius of the world defines how far away random shapes can be 
; made from the origin.
(define WORLD-RADIUS : Float 10.0)

; Make a camera for the main function.
(: make-camera (-> Camera))
(define (make-camera)
  (define eye (Float3 0.0 0.0 12.0))
  (define focus (Float3 0.0 0.0 0.0))
  (define up (Camera-global-up))
  (define aspect SCREEN-ASPECT)
  (define zoom 1.35)
  (Camera-look-at eye focus up aspect zoom))

; Make a random-sized sphere near the origin.
(: make-random-sphere (-> Sphere))
(define (make-random-sphere)
  (let ((point (Float3-random-point-in-sphere WORLD-RADIUS))
        (radius (+ 0.5 (random)))
        (color (Float3-random-color)))
    (Sphere point radius color)))

; Make a world for the main function.
(: make-world (-> World))
(define (make-world)
  ; Use a list comprehension to make a list of spheres.
  (define spheres
    (for/list: : (Listof Sphere) ((i SPHERE-COUNT))
      (make-random-sphere)))
  (define sun
    (DistantLight (Float3-normalized (Float3 1.0 1.0 1.0))
                  (Float3 1.0 0.80 0.65)))
  (define background-color (Float3 0.45 0.65 1.0))
  (World spheres sun background-color))

(define (main)
  (define camera (make-camera))
  (define world (make-world))
  (let ((image (make-bitmap SCREEN-WIDTH SCREEN-HEIGHT)))
    (for ((y (range SCREEN-HEIGHT)))
      ; Display progress in rows.
      (printf "~s / ~s~n" y SCREEN-HEIGHT)
      
      ; Percentage from top of the screen to the bottom.
      (define yy (exact->inexact (/ y SCREEN-HEIGHT)))
      
      (for ((x (range SCREEN-WIDTH)))
        ; Percentage from left of the screen to the right.
        (define xx (exact->inexact (/ x SCREEN-WIDTH)))
        
        ; Ray through the pixel.
        (define ray
          (Ray (Camera-eye camera)
               (Camera-get-ray-direction camera xx yy)))
        
        ; Color of the world at the pixel before clamping.
        (define color (World-ray-trace world ray))
        
        ; The output image only accepts bytes for color, so convert the
        ; color vector to bytes and set the pixel.
        (send image set-argb-pixels x y 1 1
              (Float3-to-argb
               (Float3-clamped color 0.0 1.0)))))
    (send image save-file "image.png" 'png))
  (printf "Done!~n"))

; Run and time the main program function.
(time (main))
