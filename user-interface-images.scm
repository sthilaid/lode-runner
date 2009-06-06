(include "declarations.scm")
(include "opengl-header.scm")
(include "texture-macro.scm")
(include "font-macro.scm")

;; Stage objects

(define-symmetric-font "wall" 8 8 loop-x loop-y)
(define-symmetric-font "ladder" 16 16 loop-y)
(define-symmetric-font "handbar" 8 8 loop-x)

;; Main game objects

(define-symmetric-font "gold" 16 16)
(define-symmetric-font "player" 16 24)

;; Font
(define-symmetric-font "bb_fonts" 8 8 static)