(include "declarations.scm")
(include "scm-lib-macro.scm")
(include "class.scm")

;; FPS is calculated in the user interface
(define FPS (create-bounded-simple-moving-avg 5))

;; Note: The player movement speed must be multipliable to give 1 so
;; accepteble values are 1/8 (*8), 2/8 (*4), 4/8 (*2) and 1. This
;; ensures that the player always falls in holes on the ground...
(define player-movement-speed 2/8)


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

;; The grid height should be 31, but we added a bottom wall below the
;; level, to avoid having a special case when the player falls to the
;; bottom of the level. The row of y = 0 won't be displayed...
(define grid-width  48)
(define grid-height 32) 
(define grid-cell-w 8)
(define grid-cell-h 8)

(define (make-grid) (make-matrix2d grid-width grid-height (empty-set)))
(define make-grid-cell cons)
(define grid-cell-i car)
(define grid-cell-j cdr)
(define (grid-cell-eq? c1 c2) (and (= (grid-cell-i c1) (grid-cell-i c2))
                                   (= (grid-cell-j c1) (grid-cell-j c2))))

(define (grid-get grid cell) (matrix2d-get grid
                                           (grid-cell-i cell)
                                           (grid-cell-j cell)))

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
     new-grid-cells)

    ;; The the objects cells to the new ones
    (game-object-grid-cells-set! obj new-grid-cells)))

(define (grid-coord->world-coord rect)
  ;; objects are dropped down one level such that the bottom wall
  ;; doesn't get shown (see level-loader).
  (values (* (point-x rect)       grid-cell-w)
          (* (- (point-y rect) 1) grid-cell-h)
          (* (rect-width rect)    grid-cell-w)
          (* (rect-height rect)   grid-cell-h)))

(define (validate-grid-bounds! obj)
  (update! obj rect x
           (lambda (x) (max 0 (min (- grid-width  (rect-width  obj)) x))))
  (update! obj rect y
           (lambda (y) (max 0 (min (- grid-height (rect-height obj)) y)))))



(define (get-grid-cells-at x-min x x-max y-min y y-max)
  (let loop ((w 0) (h 0) (cells '()))
      (if (< (+ y-min h) y-max)
          (if (< (+ x-min w) x-max)
              (loop (+ w 1) h (cons (make-grid-cell (+ x-min w) (+ y-min h))
                                    cells))
              (loop 0 (+ h 1) cells))
          cells)))

(define (get-grid-cells obj)
  (let* ((x     (rect-x  obj))
         (y     (rect-y  obj))
         (w-max (rect-width  obj))
         (h-max (rect-height obj))
         (x-min (floor x))
         (y-min (floor y))
         (x-max (+ x w-max))
         (y-max (+ y h-max)))
    (get-grid-cells-at x-min x x-max y-min y y-max)))

(define (get-grid-cells-below obj)
  (let* ((x     (rect-x  obj))
         (w-max (rect-width  obj))
         (x-min (floor x))
         (x-max (+ x w-max))
         (y     (- (rect-y  obj) 1))
         (y-min (floor y))
         (y-max (+ y-min 1)))
    (get-grid-cells-at x-min x x-max y-min y y-max)))


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
;; (define game-object-x
;;   (let ((old-fn game-object-x))
;;     (lambda (obj) (let ((x (old-fn obj)))
;;                     (if (flonum? x) (##flonum->fixnum (round x)) x)))))
;; (define game-object-y
;;   (let ((old-fn game-object-y))
;;     (lambda (obj) (let ((y (old-fn obj)))
;;                     (if (flonum? y) (##flonum->fixnum (round y)) y)))))

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

(define-class hole  (stage)
  (constructor:
   (lambda (self x y) (init! cast: '(stage * * * * *)
                             self (gensym 'hole) x y 1 1))))

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

(define (fix-obj-y-pos! obj) (update! obj game-object y floor))

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
  (slot: obj-cache)
  (slot: score))

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

(define (get-objects-below obj level)
  (fold-l (curry2* set-union eq?)
          '()
          (map (curry2 grid-get (level-grid level))
               (get-grid-cells-below obj))))

(define (get-holes-below obj level)
  ;; assuming that there are no hole instances
  (let ((holes (filter (compose null? (curry2 grid-get (level-grid level)))
                       (get-grid-cells-below obj))))
    (if (null? holes)
        #f
        (map (lambda (cell) (new hole (grid-cell-i cell) (grid-cell-j cell)))
             holes))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Collision detection
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; The result should be considered a boolean value
;; (define (detect-collision? obj1 obj2)
;;   (and (not (eq? obj1 obj2))
;;        (exists (lambda (o1-cell)
;;                  (exists (lambda (o2-cell) (grid-cell-eq? o1-cell o2-cell))
;;                          (game-object-grid-cells obj2)))
;;                (game-object-grid-cells obj1))))

(define (detect-collisions obj level)
;;   (pp `(,(game-object-id obj) cells: ,(game-object-grid-cells obj)
;;         test: ,(get-grid-cells obj)))
  (filter (lambda (x) (not (eq? x obj)))
          (fold-l (curry2* set-union eq?)
                  '()
                  (map (curry2 grid-get (level-grid level))
                       (game-object-grid-cells obj)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Collision resolution
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-generic resolve-collision)

(define-method (resolve-collision (l ladder) (h human-like) level k)
  (human-like-can-climb-up?-set! h (ladder-x l)))
(define-method (resolve-collision (h human-like) (l ladder) level k)
  (resolve-collision l h level k))

(define-method (resolve-collision (h human-like) (w wall) level k)
  (let* ((velo (human-like-velocity h))
         (vx (point-x velo))
         (vy (point-y velo)))
    (cond
     ((not (zero? vx))
      (let* ((h.x (human-like-x h))
             (h.w (human-like-width h))
             (w.x (wall-x w))
             (w.w (wall-width w))
             (original-direction (get-direction velo))
             (new-velo (new point (if (< vx 0)
                                      (+ w.x w.w (- h.x))
                                      (- (+ h.x h.w (- w.x))))
                            0)))
        (human-like-can-walk?-set! h #t)
        (human-like-velocity-set! h new-velo)
        (move! h level
               (lambda (r)
                 (human-like-facing-direction-set! h original-direction)
                 (k r)))))
        )))
(define-method (resolve-collision (w wall) (h human-like) level k)
  (resolve-collision h w level))

(define-method (resolve-collision x y lvl k)
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

(define (fall-cycle! p)
  (human-like-state-set! p 'jumping))

(define (reset-walk-cycle! hum-like)
  ;; leave the direction unchanged...
  (human-like-walk-cycle-state-set! hum-like 0)
  (human-like-state-set! hum-like 'waiting))

(define-method (change-state! (p human-like))
  (let* ((v (moving-velocity p)))
    (cond ((not (zero? (point-x v))) (walk-cycle! p))
          ((not (human-like-can-walk? p)) (fall-cycle! p))
          ((not (zero? (point-y v))) (ascend-cycle! p))
          (else (reset-walk-cycle! p)))))

(define-method (change-state! (obj game-object))
  'do-nothing)

;;; Movement implementation

;; (move! obj level k) where k is the continuation of the move! call
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
     ((not (human-like-can-walk? hum))
      ;; FIXME: not clean  to use player-movement-speed here!
      (human-like-velocity-set! hum
                                (new point 0 (- player-movement-speed)))
      #t)
     ;; its ok to walk if (not (not human-like-can-walk? hum))  hehe
     ((not (zero? v-x)) #t)
     (else
      (human-like-velocity-set! hum point-zero)
      #f))))

(define-method (move! (obj human-like) level k)
  (let* ((velocity (moving-velocity obj)))
    (if (and (not (point-zero? velocity))
             (allowed-to-move!? obj))
        ;; the allowed-to-move!? might have changed the velocity!
        (let ((modified-velocity (moving-velocity obj)))
          (change-state! obj)
          (point-add! obj modified-velocity)
          (pp `(player moved to (,(point-x obj) ,(point-y obj))
                       with velocity: (,(point-x modified-velocity)
                                       ,(point-y modified-velocity))))
          (validate-grid-bounds! obj) ; make sure player stays in level bounds
          (pp `(after validation (,(point-x obj) ,(point-y obj))))
          (grid-update (level-grid level) obj)
          (let ((objects-below     (get-objects-below obj level)))
            
            ;; Object state reset
            (human-like-can-walk?-set! obj #f)
            (human-like-can-climb-up?-set! obj #f)
            (human-like-can-go-down?-set! obj #f)

            ;; must be performed *before* the collision detection
            ;; because of the position may be ajusted 
            (for-each
             (lambda (x)
               (cond ((instance-of? x 'ladder)
                      (human-like-can-go-down?-set! obj (ladder-x x))
                      (human-like-can-walk?-set! obj #t))
                     ((instance-of? x 'wall)
                      (if (<= (point-y modified-velocity) 0)
                          (fix-obj-y-pos! obj))
                      (human-like-can-walk?-set! obj #t))))
                      objects-below)

            ;; Perform collision detection / resolution
            (let loop ((colliding-objects (detect-collisions obj level)))
              (if (and (> (point-y modified-velocity) 0)
                       (not (exists ladder? colliding-objects)))
                  (begin
                    (pp `(Going-Down! from: ,(point-y obj)
                                      to: ,(- (floor (point-y obj))
                                              (point-y obj))))
                    (human-like-velocity-set!
                     obj
                     ;; negative y value
                     (new point 0 (- (floor (point-y obj)) (point-y obj))))
                    ;;(human-like-can-go-down?-set! obj #t)
                    (move! obj level k))
                  (for-each
                   (lambda (col-obj) (resolve-collision obj col-obj level k))
                   colliding-objects)))))
        ;; resets the object state
        (change-state! obj))
    (k #t)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; frame update (game loop)
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-generic animate)
(define-method (animate (p player) level)
  (call/cc (lambda (k) (if (not (player-can-walk? p))
                           (player-velocity-set!
                                   p (new point 0 (- player-movement-speed))))
                   (move! p level k))))
(define-method (animate (x game-object) level)
  'do-nothing)

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
  ;; not sure what is the good approach at moving objects. The player
  ;; speed is reset every frame.
  (let ((player (level-get 'player level)))
    (player-velocity-set! player point-zero))
  (for-each (flip process-key level) keys)
  (for-each (flip animate level) (filter human-like? (level-objects level)))
  )


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Object rendering
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (render-object obj texture color char)
  (receive (x y w h) (grid-coord->world-coord obj)
    (draw-textured-object texture color char x y w h)))

(define (render-grid)
  (for j 0 (< j grid-height)
       (for i 0 (< i grid-width)
            (draw-grid-point (* i grid-cell-w) (* j grid-cell-h)))))

(define-generic render)

(define-method (render (lvl level))
  (for-each render (level-objects lvl))
  (render (level-get 'player lvl)))

(define-method (render (w wall))
  (render-object w wall 'pink 'wall))

(define-method (render (h hole))
  'do-nothing-hehe)

(define-method (render (g gold))
  (render-object g gold 'regular 'gold))

(define-method (render (hb handbar))
  (render-object hb handbar 'regular 'bar))

(define-method (render (p player))
  (render-object p player (player-facing-direction p) (player-state p)))

(define-method (render (l ladder))
  (render-object l ladder 'regular 'ladder))
