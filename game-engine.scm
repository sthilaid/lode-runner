(include "declarations.scm")
(include "scm-lib-macro.scm")
(include "class.scm")

(define player-movement-speed 0.3)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Geometric Shapes definitions
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;; Rectangle structure used in collision detection ;;;;
(define-class point () (slot: x) (slot: y))
(define-class rect (point) (slot: width) (slot: height))
(define-class triangle () (slot: p1) (slot: p2) (slot: p3))

;; these are translation of the pos2d lib from scm-lib into oo
(define (point-add p1 p2) (new point
                               (+ (point-x p1) (point-x p2))
                               (+ (point-y p1) (point-y p2))))
(define (point-sub p1 p2) (new point
                               (- (point-x p1) (point-x p2))
                               (- (point-y p1) (point-y p2))))
;; Dirty versions: modifies the first of the 2 given points with the result
(define (point-add! p1 p2)
  (point-x-set! p1 (+ (point-x p1) (point-x p2)))
  (point-y-set! p1 (+ (point-y p1) (point-y p2))))
(define (point-sub! p1 p2)
  (point-x-set! p1 (- (point-x p1) (point-x p2)))
  (point-y-set! p1 (- (point-y p1) (point-y p2))))
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

(define (triangle->list t)
  (list (triangle-p1 t) (triangle-p2 t) (triangle-p3 t)))

(define (list->triangle lst)
  (new triangle (car lst) (cadr lst) (caddr lst)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Grid
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define grid-width  48)
(define grid-height 31)
(define grid-cell-w 8)
(define grid-cell-h 8)

(define (make-grid) (make-matrix2d grid-width grid-height))
(define make-grid-cell cons)
(define grid-cell-i car)
(define grid-cell-j cdr)
(define (grid-cell-eq? c1 c2) (and (= (grid-cell-i c1) (grid-cell-i c2))
                                   (= (grid-cell-j c1) (grid-cell-j c2))))

;; updates the game-object's grid cells
(define (grid-update grid obj)
  (let ((old-grid-cells (game-object-grid-cells obj))
        (new-grid-cells (get-grid-cells obj)))
    (for-each
     (lambda (invalid-cell)
       (let* ((i (grid-cell-i invalid-cell))
              (j (grid-cell-j invalid-cell))
              (grid-objects (matrix2d-get grid i j)))
         (matrix2d-set! grid i j (set-remove eq? obj grid-objects))))
     (set-substract grid-cell-eq? old-grid-cells new-grid-cells))
    (for-each
     (lambda (new-cell)
       (let* ((i (grid-cell-i new-cell))
              (j (grid-cell-j new-cell))
              (grid-objects (matrix2d-get grid i j)))
         (matrix2d-set! grid i j (set-add eq? obj grid-objects))))
     (set-substract grid-cell-eq? new-grid-cells old-grid-cells))))

(define (grid-coord->world-coord rect)
  (values (* (point-x rect)     grid-cell-w)
          (* (point-y rect)     grid-cell-h)
          (* (rect-width rect)  grid-cell-w)
          (* (rect-height rect) grid-cell-h)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Game objects
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Abstract classes
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (define-class colored ()   (slot: color))
(define-class statefull () (slot: state))
(define-class moving ()    (slot: velocity))

;; game-object has x,y pos and with and height for bbox
(define-class game-object (rect) (slot: id) (slot: grid-cells)
  (constructor: (lambda (self id x y w h)
                  (set-fields! self game-object
                               ((x x)
                                (y y)
                                (width w)
                                (height h)
                                (id id)
                                (grid-cells (empty-set)))))))

(define-class stage (game-object)
  (slot: neighbours)
  (constructor: (lambda (self id x y w h)
                  (init! cast: '(game-object * * * * *) self id x y w h)
                  (stage-neighbours-set! self '()))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Leaf Classes (which produce instances)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Stage objects
(define-class wall  (stage)
  (constructor:
   (lambda (self x y w h) (init! cast: '(stage * * * * *)
                                 self (gensym 'wall) x y w h))))

(define-class ladder (stage)
  (constructor:
   (lambda (self x y h) (init! cast: '(stage * * * * *)
                               self (gensym 'ladder) x y 2 h))))
(define-class handbar (stage)
  (constructor:
   (lambda (self x y w) (init! cast: '(stage * * * * *)
                               self (gensym 'handbar) x y w 1))))

(define-class gold  (game-object)
  (constructor: (lambda (self x y) (init! cast: '(game-object * * * * *)
                                          self (gensym 'gold) x y 2 2))))

(define-class human-like (game-object moving statefull)
  (slot: facing-direction)
  (slot: walk-cycle-state)
  (constructor: (lambda (self x0 y0 initial-velocity id)
                  (init! cast: '(game-object * * * * *)
                         self id x0 y0 2 3)
                  (set-fields! self human-like
                    ((state 'standing-up)
                     (velocity initial-velocity)
                     (facing-direction 'right)
                     (walk-cycle-state 0))))))

(define-class robot (human-like)
  (constructor: (lambda (self x0 y0 initial-velocity)
                  (init! cast: '(human-like * * * *)
                         self x0 y0 initial-velocity (gensym 'robot)))))

(define-class player (human-like)
  (slot: walk-cycle-state)
  (constructor: (lambda (self x0 y0 initial-velocity)
                  (init! cast: '(human-like * * * *)
                         self x0 y0 initial-velocity 'player))))


(define-class level ()
  (slot: name)
  (slot: grid)
  (slot: objects)
  (slot: obj-cache))

(define (level-cache-add! obj level)
  (table-set! (level-obj-cache level) (game-object-id obj) obj))
(define (level-cache-remove! obj level)
  (table-set! (level-obj-cache level) (game-object-id obj)))
(define (level-delete! obj lvl)
  (update! lvl level objects (lambda (objs) (list-remove (lambda (obj) (eq? (game-object-id obj id))) objs)))
  (level-cache-remove! id level))

(define (level-get id level)
  (cond ((table-ref (level-obj-cache level) id #f) => identity)
        ((exists (lambda (obj) (eq? (game-object-id obj) id)) (level-objects level))
         => (lambda (obj) (begin (level-cache-add! obj level)
                                 obj)))
        (else #f)))


(define (get-grid-cells obj)
  (let ((x     (game-object-x  obj))
        (y     (game-object-y  obj))
        (w-max (game-object-width  obj))
        (h-max (game-object-height obj)))
    (let loop ((w 0) (h 0) (cells '()))
      (if (< h h-max)
          (if (< w w-max)
              (loop (+ w 1) h (cons (make-grid-cell (+ x w) (+ y h))
                                    cells))
              (loop 0 (+ h 1) cells))
          cells))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Collision detection
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (detect-collision obj1 obj2)
  (exists grid-cell-eq?
          (game-object-grid-cells obj1)
          (game-object-grid-cells obj2)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Movement
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-generic change-state!)

(define (walk-cycle p)
  (define cycle-length 20)
  (define cycling-delta 5)
  (update! p player walk-cycle-state
           (lambda (s) (modulo (+ s 1) cycle-length)))
  (let ((new-state (case (quotient (player-walk-cycle-state p) cycling-delta)
                     ((0) 'standing-up)
                     ((1) 'standing-left)
                     ((2) 'standing-up)
                     ((3) 'standing-right)
                     (else (error "Invalid player walk cycle state")))))
    (player-state-set! p new-state)))
(define-method (change-state! (p player))
  (let ((v (moving-velocity p)))
    (cond ((zero? (point-y v)) (walk-cycle p))
          )))

(define-method (change-state! (obj game-object))
  'do-nothing)


(define-generic move!)

(define-method (move! (obj human-like) velocity level)
  (define (get-direction velocity)
    (if (< (point-x velocity) 0) 'left 'right))
  (human-like-facing-direction-set! obj (get-direction velocity))
  (call-next-method)
  ;;(move! cast: '(moving * *) obj velocity level)
  )

(define-method (move! (obj moving) velocity level)
  (change-state! obj)
  (moving-velocity-set! obj velocity)
  (point-add! obj velocity)
  (let ((colliding-objects
         (filter (lambda (other-obj) (detect-collision obj other-obj))
                 (level-objects level))))
    (for-each (lambda (other-obj)
                (pp `(collision between ,(game-object-id obj)
                                and     ,(game-object-id other-obj) occured)))
              colliding-objects)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; frame update (game loop)
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(define (process-key key-sym level)
  ;; the keysym are defined in the user-interface module
  (let ((player (level-get 'player level)))
   (case key-sym
     [(left)  (move! player (new point (- player-movement-speed) 0) level)]
     [(right) (move! player (new point player-movement-speed     0) level)])))

(define (advance-frame! level keys)
  (for-each (flip process-key level) keys))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Object rendering
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(define-generic render)

(define (render-object obj texture color char)
  (receive (x y w h) (grid-coord->world-coord obj)
    (draw-textured-object texture color char x y w h)))

(define-method (render (w wall))
  (render-object w wall 'pink 'wall))

(define-method (render (g gold))
  (render-object g gold 'regular 'gold))

(define-method (render (hb handbar))
  (render-object hb handbar 'regular 'bar))

(define-method (render (p player))
  (render-object p player (player-facing-direction p) (player-state p)))

(define-method (render (l ladder))
  (render-object l ladder 'regular 'ladder))
