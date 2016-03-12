#lang typed/racket

(provide (all-defined-out))
(require racket/flonum "Constants.rkt" "DistantLight.rkt"
         "Float3.rkt" "Intersection.rkt" "Ray.rkt" "Sphere.rkt")

; The world has all the shapes, lights, and properties needed for the ray
; tracer to calculate the color for a pixel. In a more complex ray tracer,
; there would be more variety of shape types, and the lights might become
; shapes themselves, which allows for soft shadows.
(struct World
  ((spheres : (Listof Sphere))
   (sun : DistantLight)
   (background-color : Float3)))

; Quality of ambient occlusion. 1 sample is the minimum quality, and past
; a thousand or more samples it is sufficiently converged.
(define WORLD-AMBIENT-SAMPLES : Integer 64)

; Default specularity for every shape's material.
(define WORLD-SHAPE-SPECULARITY : Float 0.30)

; Default "shininess" for every shape's material.
(define WORLD-SHAPE-SHININESS : Float 16.0)

; The distance from an intersection at which point ambient occlusion is zero.
; Increasing this value increases the amount of ambient occlusion visible.
; The ambient occlusion percentage at a point is at maximum when two shapes
; are very close together, or when the intersection is in a corner.
(define WORLD-MAX-OCCLUSION-DISTANCE : Float 5.0)

; Find the nearest hit shape by a given ray. In more complex ray tracers,
; this method would be expanded to include an acceleration structure
; traversal which, in the simplest sense, walks through a binary tree
; instead of iterating through a list, thereby trimming away unnecessary
; shape intersections and greatly speeding up the process.
(: World-nearest-hit (-> World Ray Intersection))
(define (World-nearest-hit world ray)  
  (let loop ((nearest-shape (Intersection-empty))
             (spheres (World-spheres world)))
    (if (null? spheres)
        nearest-shape
        (let ((current-try (Sphere-intersect (car spheres) ray)))
          (loop
           (if (< (Intersection-t current-try)
                  (Intersection-t nearest-shape))
               current-try nearest-shape)
           (cdr spheres))))))

; Get the percentage of ambient light visible at a point. Ambient light is
; only affected by geometry in this simplified model. More complex ray
; tracers take light bouncing into consideration as well.
(: World-get-ambient-percent (-> World Float3 Float3 Float))
(define (World-get-ambient-percent world point normal)
  ; Offset the point from the intersection surface slightly to avoid
  ; self-intersections due to float imprecision.
  (define point-offset
    (Float3-add point (Float3-scale-by-scalar normal EPSILON)))
  (let loop ((n 0)
             (percent 0.0))
    ; Get some number of samples of ambient occlusion at the point.
    (if (= n WORLD-AMBIENT-SAMPLES)
        (exact->inexact (/ percent WORLD-AMBIENT-SAMPLES))
        ; Cast a ray in a random direction in a hemisphere and see what
        ; geometry (if any) is hit.
        (let* ((hemisphere-direction
                (Float3-random-hemisphere-direction normal))
               (hemisphere-ray
                (Ray point-offset hemisphere-direction))
               (current-try (World-nearest-hit world hemisphere-ray))
               (new-percent
                (if (> (Intersection-t current-try)
                       WORLD-MAX-OCCLUSION-DISTANCE)
                    1.0
                    (exact->inexact (/ (Intersection-t current-try)
                                       WORLD-MAX-OCCLUSION-DISTANCE)))))
          (loop (+ n 1)
                (+ percent new-percent))))))

; Calculate the total shaded color at a point.
(: World-get-color-at (-> World Ray Intersection Float3))
(define (World-get-color-at world ray intersection)
  ; Get the ambient lighting component.
  (define ambient-color
    (Float3-scale-by-scalar
     (Float3-scale-by-vector (Intersection-color intersection)
                             (World-background-color world))
     (World-get-ambient-percent world (Intersection-point intersection)
                                (Intersection-normal intersection))))
  ; Get the diffuse lighting component.
  (define diffuse-color
    (let* ((light (World-sun world))
		(view (Float3-negate (Ray-direction ray)))
           (vn-dot (Float3-dot view (Intersection-normal intersection)))
           (vn-sign (if (> vn-dot 0.0) 1.0 -1.0))
           (local-normal
            (Float3-scale-by-scalar (Intersection-normal intersection)
                                    vn-sign))
           (light-direction
            (DistantLight-direction light))
           (point-offset
            (Float3-add (Intersection-point intersection)
                        (Float3-scale-by-scalar local-normal EPSILON)))
           (shadow-ray
            (Ray point-offset light-direction))
           (shadow-try
            (World-nearest-hit world shadow-ray)))
      ; If there's a shape in the way of the light, then no diffuse component.
      ; Otherwise, calculate the primary and highlight colors.
      (if (< (Intersection-t shadow-try) INTERSECTION-T-MAX)
          (Float3 0.0 0.0 0.0)
          (let* ((ln-reflect
                  (Float3-normalized
                   (Float3-reflect light-direction local-normal)))
                 (ln-dot
                  (Float3-dot light-direction local-normal))
                 (ln-reflect-view-dot
                  (Float3-dot ln-reflect view))
                 (primary-color
                  (Float3-scale-by-scalar
                   (Float3-scale-by-vector
                    (Intersection-color intersection)
                    (DistantLight-color light))
                   (max 0.0 ln-dot)))
                 (highlight-color
                  (Float3-scale-by-scalar
                   (Float3-scale-by-scalar (DistantLight-color light)
                                           WORLD-SHAPE-SPECULARITY)
                   (flexpt (max 0.0 ln-reflect-view-dot)
                         WORLD-SHAPE-SHININESS))))
            (Float3-add primary-color highlight-color)))))
  (Float3-add ambient-color diffuse-color))

; Trace a ray through the world and obtain a color.
(: World-ray-trace (-> World Ray Float3))
(define (World-ray-trace world ray)
  (define nearest-hit (World-nearest-hit world ray))
  (if (< (Intersection-t nearest-hit) INTERSECTION-T-MAX)
      (World-get-color-at world ray nearest-hit)
      (World-background-color world)))
