(include "declarations.scm")
(include "scm-lib-macro.scm")
(include "class.scm")

;; FPS is calculated in the user interface
(define FPS (create-bounded-simple-moving-avg 5))
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
(define (point-scalar-mult p s) (new point
                                     (* s (point-x p))
                                     (* s (point-y p))))
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

(define point-zero (new point 0 0))
(define (point-zero? p) (point= p point-zero))

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

(define (grid-get grid x y) (matrix2d-get grid x y))

;; updates the game-object's grid cells
(define (grid-update grid obj)
  (let ((old-grid-cells (game-object-grid-cells obj))
        (new-grid-cells (get-grid-cells obj)))
    ;; remove old cells
    (for-each
     (lambda (invalid-cell)
       (let* ((i (grid-cell-i invalid-cell))
              (j (grid-cell-j invalid-cell))
              (grid-objects (matrix2d-get grid i j)))
         (matrix2d-set! grid i j (set-remove eq? obj grid-objects))))
     (set-substract grid-cell-eq? old-grid-cells new-grid-cells))
    ;; add new cells
    (for-each
     (lambda (new-cell)
       (let* ((i (grid-cell-i new-cell))
              (j (grid-cell-j new-cell))
              (grid-objects (matrix2d-get grid i j)))
         (matrix2d-set! grid i j (set-add eq? obj grid-objects))))
     (set-substract grid-cell-eq? new-grid-cells old-grid-cells))

    ;; The the objects cells to the new ones
    (game-object-grid-cells-set! obj new-grid-cells)))

(define (grid-coord->world-coord rect)
  (values (* (point-x rect)     grid-cell-w)
          (* (point-y rect)     grid-cell-h)
          (* (rect-width rect)  grid-cell-w)
          (* (rect-height rect) grid-cell-h)))

(define (validate-grid-bounds! obj)
  (update! obj rect x
           (lambda (x) (max 0 (min (- grid-width  (rect-width  obj)) x))))
  (update! obj rect y
           (lambda (y) (max 0 (min (- grid-height (rect-height obj)) y)))))

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
                                (id id)))
                  (game-object-grid-cells-set! self (get-grid-cells self)))))

;; (define game-object-grid-cells-set!
;;   (let ((old-fn game-object-grid-cells-set!))
;;     (lambda (obj new-cells)
;;       (pp `(setting cells to ,(game-object-id obj) to ,new-cells))
;;       (old-fn obj new-cells))))

;; The internal positions are flonums but, the received positions are
;; fixnums
(define game-object-x
  (let ((old-fn game-object-x))
    (lambda (obj) (let ((x (old-fn obj)))
                    (if (flonum? x) (##flonum->fixnum x) x)))))
(define game-object-y
  (let ((old-fn game-object-y))
    (lambda (obj) (let ((y (old-fn obj)))
                    (if (flonum? y) (##flonum->fixnum y) y)))))

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
  (slot: can-climb-up?) ; #f or contains the x pos of the colliding ladder
  (slot: can-go-down?)  ; #f or contains the x pos of the colliding ladder
  (slot: can-walk?)     ; #f, #t
  (slot: facing-direction)
  (slot: walk-cycle-state)
  (constructor: (lambda (self x0 y0 initial-velocity id)
                  (init! cast: '(game-object * * * * *)
                         self id x0 y0 2 3)
                  (set-fields! self human-like
                    ((state 'standing-up)
                     (velocity initial-velocity)
                     (can-climb-up? #f)
                     (can-go-down? #f)
                     (can-walk? #t)
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
  (update! lvl level objects
           (lambda (objs)
             (list-remove (lambda (obj) (eq? (game-object-id obj id))) objs)))
  (level-cache-remove! id level))

(define (level-get id level)
  (cond ((table-ref (level-obj-cache level) id #f) => identity)
        ((exists (lambda (obj) (eq? (game-object-id obj) id))
                 (level-objects level))
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

;; The result should be considered a boolean value
(define (detect-collision? obj1 obj2)
  (and (not (eq? obj1 obj2))
       (exists (lambda (o1-cell)
                 (exists (lambda (o2-cell) (grid-cell-eq? o1-cell o2-cell))
                         (game-object-grid-cells obj2)))
               (game-object-grid-cells obj1))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Collision resolution
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-generic resolve-collision)

(define-method (resolve-collision x y)
  'todo)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Movement
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-generic change-state!)

;;; Human-like state machine management

(define (get-direction velocity)
    (cond ((< (point-x velocity) 0) 'left)
          ((> (point-x velocity) 0) 'right)
          (else (human-like-facing-direction obj))))

(define (walk-cycle! p)
  (let* ((cycling-delta 5)
         (cycle-length (* 4 cycling-delta)))
    (update! p player walk-cycle-state
             (lambda (s) (modulo (+ s 1) cycle-length)))
    (let ((next-state
           (case (quotient (player-walk-cycle-state p) cycling-delta)
             ((0) 'standing-up)
             ((1) 'standing-left)
             ((2) 'standing-up)
             ((3) 'standing-right)
             (else (error "Invalid player walk cycle state")))))
      (human-like-facing-direction-set! p
                                        (get-direction
                                         (human-like-velocity p)))
      (human-like-state-set! p next-state))))

(define (ascend-cycle! p)
  (let* ((cycling-delta 5)
         (cycle-length (* 2 cycling-delta)))
    (update! p player walk-cycle-state
             (lambda (s) (modulo (+ s 1) cycle-length)))
    (let ((next-state
           (case (quotient (player-walk-cycle-state p) cycling-delta)
             ((0) 'left)
             ((1) 'right)
             (else (error "Invalid player walk cycle state")))))
      (human-like-facing-direction-set! p next-state)
      (human-like-state-set! p 'ladder))))

(define (reset-walk-cycle! hum-like)
  ;; leave the direction unchanged...
  (human-like-walk-cycle-state-set! hum-like 0)
  (human-like-state-set! hum-like 'waiting))

(define-method (change-state! (p human-like))
  (let* ((v (moving-velocity p)))
    (cond ((not (zero? (point-x v))) (walk-cycle! p))
          ((not (zero? (point-y v))) (ascend-cycle! p))
          (else (reset-walk-cycle! p)))))

(define-method (change-state! (obj game-object))
  'do-nothing)


(define-generic move!)

;; may change the velocity if the object is falling
(define (allowed-to-move!? hum)
  (let* ((v (human-like-velocity hum))
         (v-x (point-x v))
         (v-y (point-y v)))
    (cond
     ((or (and (> v-y 0)
               (human-like-can-climb-up? hum))
          (and (< v-y 0)
               (human-like-can-go-down? hum)))
      => (lambda (x) (begin (human-like-x-set! hum x)
                            (human-like-velocity-set! hum (new point 0 v-y))
                            #t)))
     ((not (zero? v-x))
      (if (not (human-like-can-walk? hum))
          ;; the object falls!
          (begin
            (human-like-velocity-set!
             hum
             ;; not clean  to use player-movement-speed here!
             (new point 0 (- player-movement-speed)))))
      #t)
     (else
      (human-like-velocity-set! hum point-zero)
      #f))))

(define-method (move! (obj human-like) level)
  (let* ((velocity (moving-velocity obj))
         (speed-correction   (if (eq? (FPS) 'N/A) 1. (/ 60. (FPS))))
         (effective-velocity (point-scalar-mult velocity speed-correction)))
    (if (and (not (point-zero? effective-velocity))
             (allowed-to-move!? obj))
        (begin
          (change-state! obj)
          (point-add! obj effective-velocity)
          (validate-grid-bounds! obj)
          (grid-update (level-grid level) obj)
          (let ((colliding-objects (filter (flip detect-collision? obj)
                                           (level-objects level)))
                #;(floor-objects (fold-l (lambda (acc x)
                (set-add grid-cell-eq? x acc))
                '()
                )))
            (pp `(colliding ,(game-object-id obj)
                            with ,(map game-object-id colliding-objects)))
            (for-each ((currify resolve-collision 2) obj) colliding-objects)
            (cond ((exists ladder? colliding-objects)
                   => (lambda (l) (human-like-can-climb-up?-set! obj
                                                                 (ladder-x l))))
                  (else (human-like-can-climb-up?-set! obj #f)))))
        ;; resets the object state
        (change-state! obj))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; frame update (game loop)
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(define (process-key key-sym level)
  ;; the keysym are defined in the user-interface module
  (let ((player (level-get 'player level)))
   (case key-sym
     [(left)  (player-velocity-set! player
                                    (new point (- player-movement-speed) 0))]
     [(right) (player-velocity-set! player
                                    (new point player-movement-speed 0))]
     [(up)    (player-velocity-set! player
                                    (new point 0 player-movement-speed))]
     [(down)  (player-velocity-set! player
                                    (new point 0 (- player-movement-speed)))])))

(define (advance-frame! level keys)
  (let ((player (level-get 'player level)))
    (player-velocity-set! player point-zero))
  (for-each (flip process-key level) keys)
  ;; not sure what is the good approach at moving objects
  (for-each (flip move! level)
            (filter moving? (level-objects level)))
  )


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Object rendering
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (render-object obj texture color char)
  (receive (x y w h) (grid-coord->world-coord obj)
    (draw-textured-object texture color char x y w h)))

(define-generic render)

(define-method (render (lvl level))
  (let* ((first-layer? human-like?)
         (instance< (lambda (i1 i2) (and (first-layer? i1)
                                         (not (first-layer? i2)))))
         (instance= (lambda (i1 i2) (xor (first-layer? i1) (first-layer? i2))))
         (instance> (lambda (i1 i2) (and (not (first-layer? i1))
                                         (first-layer? i2)))))
    ;; elements sorted in reverse order such taht first layer objs are
    ;; drawn last
    (for-each render (quick-sort instance> instance= instance<
                                 (level-objects lvl)))))

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
