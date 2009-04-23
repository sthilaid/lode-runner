(include "declarations.scm")
(include "scm-lib-macro.scm")
(include "class.scm")


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Geometric Shapes definitions
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;; Rectangle structure used in collision detection ;;;;
(define-class point () (slot: x) (slot: y))
(define-class rect (point) (slot: width) (slot: height))

;; these are translation of the pos2d lib from scm-lib into oo
(define (point-add p1 p2) (new point
                               (+ (point-x p1) (point-x p2))
                               (+ (point-y p1) (point-y p2))))
(define (point-sub p1 p2) (new point
                               (- (point-x p1) (point-x p2))
                               (- (point-y p1) (point-y p2))))
(define (point-scalar-prod p1 p2)
  (+ (* (point-x p1) (point-x p2)) (* (point-y p1) (point-y p2))))
(define (point-cartesian-distance p1 p2)
  (sqrt (+ (expt (- (point-x p1) (point-x p2)) 2)
           (expt (- (point-y p1) (point-y p2)) 2))))

(define (point-complexify p) (make-rectangular (point-x p) (point-y p)))

(define (point= p1 p2)
  (and (= (point-x p1) (point-x p2))
       (= (point-y p1) (point-y p2))))

(define (inverse-dir dir . options)
  (let ((x-fact (if (memq 'x options) -1 1))
        (y-fact (if (memq 'y options) -1 1)))
    (new point (* x-fact (point-x dir)) (* y-fact (point-y dir)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Game objects
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Abstract classes
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (define-class colored ()   (slot: color))
(define-class statefull () (slot: state))
(define-class moving ()    (slot: velocity))

;; game-object has x,y pos and with and height for bbox
(define-class game-object (rect) (slot: id)
  (constructor: (lambda (self id x y w h)
                  (set-fields! self game-object
                               ((x x)
                                (y y)
                                (width w)
                                (height h)
                                (id id))))))

(define-class stage (game-object)
  (slot: neighbours)
  (constructor: (lambda (self id x y w h)
                  (init! cast: '(game-object * * * * *) self id x y w h)
                  (stage-neighbours-set! self '()))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Leaf Classes (which produce instances)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Generic function which changes correctly the states of the
;; statefull objects
(define-generic change-state!)

;; Stage objects
(define-class wall  (stage)
  (constructor:
   (lambda (self x y w h) (init! cast: '(stage * * * * *)
                                 self (gensym 'wall) x y w h))))
(define-class ladder (stage)
  (constructor:
   (lambda (self x y w h) (init! cast: '(stage * * * * *)
                                 self (gensym 'ladder) x y w h))))
(define-class hand-bar (stage)
  (constructor:
   (lambda (self x y w h) (init! cast: '(stage * * * * *)
                                 self (gensym 'hand-bar) x y w h))))

;; Leaf objects (which the collision res is not trivial)
(define-class robot (game-object moving statefull))
(define-class gold  (game-object)
  (constructor: (lambda (self x y) (init! cast: '(game-object * * * * *)
                                          self (gensym 'gold) x y 11 13))))
(define-class player (game-object moving statefull)
  (constructor: (lambda (self x0 y0 initial-velocity)
                  (change-state! self 'standing)
                  (set-fields! self player
                    ((x x0)
                     (y y0)
                     (velocity initial-velocity))))))


(define-method (change-state! (p player) new-state)
  (case new-state
    ('standing-right
     (set-fields! p player ((state new-state)
                            (width  16)
                            (height 16))))
    ('standing-up
     (set-fields! p player ((state new-state)
                            (width  11)
                            (height 17))))
    ('standing-left
     (set-fields! p player ((state new-state)
                            (width  16)
                            (height 16))))
    ('jumping
     (set-fields! p player ((state new-state)
                            (height 16)
                            (width  20))))
    ('lader-left
     (set-fields! p player ((state new-state)
                            (width  15)
                            (height 20))))
    ('lader-right
     (set-fields! p player ((state new-state)
                            (width  15)
                            (height 20))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Collision detection
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-generic detect-collision?)

;; Simple rectangular collision detection. Not optimized.
(define-method (detect-collision? (r1 rect) (r2 rect))
  (let* ((r1-x1 (rect-x r1))
         (r1-x2 (+ r1-x1 (rect-width r1)))
         (r1-x-min (min r1-x1 r1-x2))
         (r1-x-max (max r1-x1 r1-x2))
         (r1-y1 (rect-y r1))
         (r1-y2 (+ r1-y1 (rect-height r1)))
         (r1-y-min (min r1-y1 r1-y2))
         (r1-y-max (max r1-y1 r1-y2))

         (r2-x1 (rect-x r2))
         (r2-x2 (+ r2-x1 (rect-width r2)))
         (r2-x-min (min r2-x1 r2-x2))
         (r2-x-max (max r2-x1 r2-x2))
         (r2-y1 (rect-y r2))
         (r2-y2 (+ r2-y1 (rect-height r2)))
         (r2-y-min (min r2-y1 r2-y2))
         (r2-y-max (max r2-y1 r2-y2)))
    (not (or (< r1-x-max r2-x-min)
             (> r1-x-min r2-x-max)
             (< r1-y-max r2-y-min)
             (> r1-y-min r2-y-max)))))

(define-method (detect-collision? (point point) (rect rect))
  (let* ((rect-x1 (rect-x rect))
         (rect-x2 (+ rect-x1 (rect-width rect)))
         (rect-x-min (min rect-x1 rect-x2))
         (rect-x-max (max rect-x1 rect-x2))
         (rect-y1 (rect-y rect))
         (rect-y2 (+ rect-y1 (rect-height rect)))
         (rect-y-min (min rect-y1 rect-y2))
         (rect-y-max (max rect-y1 rect-y2))
         (point-x (point-x point))
         (point-y (point-y point)))
    (and (>= point-x rect-x-min)
         (<= point-x rect-x-max)
         (>= point-y rect-y-min)
         (<= point-y rect-y-max))))
(define-method (detect-collision? (rect rect) (point point))
  (detect-collision? point rect))

(define tests (list (new wall 10 10 50 50)
                    (new gold 100 100)
                    (new gold 100 100)
                    (new gold 100 100)
                    (new gold 100 100)
                    (new gold 100 100)
;;                     (new gold 100 100)
;;                     (new gold 100 100)
;;                     (new gold 100 100)
;;                     (new gold 100 100)
;;                     (new gold 100 100)
;;                     (new gold 100 100)


                    ))

(define (advance-frame!)
  tests)

(define-generic render)

(define-method (render (w wall))
  (draw-textured-object wall 'brown 'wall
                        (wall-x w) (wall-y w) (wall-width w) (wall-height w)))

(define-method (render (g gold))
  (draw-textured-object gold 'regular 'gold
                        (game-object-x g) (game-object-y g)
                        (game-object-width g) (game-object-height g)))