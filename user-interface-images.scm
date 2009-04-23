(include "declarations.scm")
(include "opengl-header.scm")
(include "texture-macro.scm")
(include "font-macro.scm")

;; Stage objects

(define-symmetric-font "wall" 16 16 loop-x loop-y)
(define-symmetric-font "ladder" 32 32 loop-y)
(define-symmetric-font "handbar" 2 2 loop-x)

;; Main game objects

(define-symmetric-font "gold" 16 16)

;; Font

(define-symmetric-font "bb_fonts" 8 8 static)