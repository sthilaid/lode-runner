;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; filename: texture.scm
;;
;; description: This file contains runtime functions related to the
;; texture abastraction defined in texture-macro.scmtexture-macro.scm.
;;
;; author: David St-Hilaire
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-type texture id width height)

;; A gensym equivalent that generates fresh integer texture id or in
;; opengl terms, a fresh texture name.
(define genTexture (let ((i 5))
                     (lambda () (set! i (+ i 1)) i)))

(define debug-textures '())

(define (setup-texture-render-env tex-id)
  (glBlendFunc GL_SRC_ALPHA GL_ONE_MINUS_SRC_ALPHA)
  (glColor4f 0. 0. 0. 1.)
  (glEnable GL_TEXTURE_2D)
  (glTexEnvi GL_TEXTURE_ENV GL_TEXTURE_ENV_MODE GL_ADD)
  (glBindTexture GL_TEXTURE_2D tex-id))

;; Function which will dray the texture of given string name to the 2d
;; (x,y) plane coordinate.
(define (draw-texture texture-obj x y #!key (width #f) (height #f))
  (let* ((tex-id      (texture-id texture-obj))
         (tex-width   (texture-width texture-obj))  
         (tex-height  (texture-height texture-obj))
         (width       (if width  width  tex-width))
         (height      (if height height tex-height))
         (tex-x-max   (exact->inexact (/ width  tex-width)))
         (tex-y-max   (exact->inexact (/ height tex-height))))
    (setup-texture-render-env tex-id)
    (glBegin GL_QUADS)
    (begin
      (glTexCoord2f 0.0       0.0)       (glVertex2i x           y)
      (glTexCoord2f 0.0       tex-y-max) (glVertex2i x           (+ y height))
      (glTexCoord2f tex-x-max tex-y-max) (glVertex2i (+ x width) (+ y height))
      (glTexCoord2f tex-x-max 0.0)       (glVertex2i (+ x width) y))
    (glEnd)
    (glDisable GL_TEXTURE_2D)))

;; points expected to be in a list such that '(x y)
(define (draw-texture-points geometric-type  texture-obj points)
  (define (->texture-coord tex-size ref-coord coord)
    (exact->inexact (/ (- coord ref-coord ) tex-size)))
  
  (if (not (pair? points)) (error "Cannot render empty textured polygon."))
  (let* ((tex-id      (texture-id texture-obj))
         (tex-width   (texture-width texture-obj))
         (tex-height  (texture-height texture-obj))
         (x-init      (caar points))
         (y-init      (cadar points)))
    (setup-texture-render-env tex-id)
    (glBegin geometric-type)
    (let loop ((points points) (u 0.) (v 0.))
    (if (pair? points)
        (let* ((p (car points))
               (x (car p))
               (y (cadr p))
               (u (->texture-coord tex-width  x-init x))
               (v (->texture-coord tex-height y-init y)))
          (glTexCoord2f u v)
          (glVertex2i x y)
          (loop (cdr points) u v))))
    (glEnd)
    (glDisable GL_TEXTURE_2D)))

(define (draw-triangles texture-obj points)
  (draw-texture-points GL_TRIANGLES texture-obj points))
(define (draw-triangle-strip texture-obj points)
  (draw-texture-points GL_TRIANGLE_STRIP texture-obj points))
(define (draw-triangle-fan texture-obj points)
  (draw-texture-points GL_TRIANGLE_FAN texture-obj points))

(define (draw-quads texture-obj points)
  (draw-texture-points GL_QUADS texture-obj points))
(define (draw-quad-strip texture-obj points)
  (draw-texture-points GL_QUAD_STRIP texture-obj points))
(define (draw-texture-poly texture-obj points)
  (draw-texture-points GL_POLYGON texture-obj points))

(define (draw-line-loop texture-obj points)
  (draw-texture-points GL_LINE_LOOP texture-obj points))