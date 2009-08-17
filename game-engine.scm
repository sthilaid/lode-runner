(include "declarations.scm")
(include "scm-lib-macro.scm")
(include "class.scm")

;; FPS is calculated in the user interface
(define FPS (create-bounded-simple-moving-avg 5 init-value: 60.))

;; Note: The player movement speed must be multipliable to give 1 so
;; accepteble values are 1/8 (*8), 2/8 (*4), 4/8 (*2) and 1. This
;; ensures that the player always falls in holes on the ground...
(define player-movement-speed 2/8)

(enum background-layer
      stage-layer
      foreground-layer
      human-like-layer
      top-layer)


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
  ;; 1 is the min y value because of the presence of the fake wall
  ;; below the level
  (update! obj rect y
           (lambda (y) (max 1 (min (- grid-height (rect-height obj)) y)))))



;; Will return all the grid cells in the range [x,x-max][y,y-max]
;; considering that cells start a [x-min,y-min]. Allows to use non
;; grid orthogonal x and y values.
(define (get-grid-cells-at x-min x x-max y-min y y-max)
  (let loop ((w 0) (h 0) (cells '()))
      (if (< (+ y-min h) y-max)
          (if (< (+ x-min w) x-max)
              (loop (+ w 1) h (cons (make-grid-cell (+ x-min w) (+ y-min h))
                                    cells))
              (loop 0 (+ h 1) cells))
          cells)))

(define (get-grid-cells obj #!key
                        (x-offset 0)
                        (y-offset 0)
                        (width (rect-width obj))
                        (height (rect-height obj)))
  (let* ((x (+ (rect-x  obj) x-offset))
         (x-min (floor x))
         (x-max (+ x width))
         ;; snap to grid in y the object by having y-min = y
         (y-min (floor (+ (rect-y  obj) y-offset)))
         (y y-min)
         (y-max (+ y height)))
    (get-grid-cells-at x-min x x-max y-min y y-max)))

(define (get-grid-cells-below obj)
  (get-grid-cells obj y-offset: -1 height: 1))


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
(define-class game-object (rect)
  (slot: id)
  (slot: grid-cells)
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

(define-class hole  (stage statefull)
  (slot: contained-object?)
  (slot: appear-cycle-state)
  (constructor:
   (lambda (self x y w h) (init! cast: '(stage * * * * *)
                                 self (gensym 'hole) x y w h)
           (set-fields! self hole
             ((contained-object? #f)
              (state 0)
              (appear-cycle-state 0))))))

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
  (slot: can-use-rope?) ; #f or contains the y pos of the colliding handbar
  (slot: can-walk?)        ; boolean
  (slot: droped-rope?)     ; boolean
  (slot: stuck-in-hole?)   ; boolean
  (slot: facing-direction) ; left / right
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
                     (can-use-rope? #f)
                     (droped-rope? #f)
                     (stuck-in-hole? #f)
                     (facing-direction 'right)
                     (walk-cycle-state 0))))))

(define (fix-obj-y-pos! obj) (update! obj game-object y floor))

(define-class robot (human-like)
  (constructor: (lambda (self x0 y0 initial-velocity)
                  (init! cast: '(human-like * * * *)
                         self x0 y0 initial-velocity (gensym 'robot)))))

(define-class player (human-like)
  (slot: walk-cycle-state)
  (constructor: (lambda (self x0 y0)
                  (init! cast: '(human-like * * * *)
                         self x0 y0 (new point 0 0) 'player))))

(define-class gui (game-object)
  (slot: visible?)
  ;;(slot: zoom-ratio)
  (constructor: (lambda (self id x y visible?)
                  ;; FIXME: usage of zeros can be potentially buggy?
                  (init! cast: '(game-object * * * * *)
                         self id x y 0 0)
                  (set-fields! self gui ((visible? visible?))))))

(define-class label (gui)
  (slot: text)
  (slot: color)
  (slot: properties)
  (constructor: (lambda (self text x y color property-list)
                  (init! cast: '(gui * * * *)
                         self (gensym 'label) x y #t)
                  (set-fields! self label
                    ((text text)
                     (properties property-list)
                     (color color))))))

(define-class level ()
  (slot: name)
  (slot: grid)
  (slot: objects)
  (slot: obj-cache)
  (slot: score)
  (slot: player-start-pos)
  (slot: gold-left)
  (slot: lives)
  (slot: time-left)
  (slot: paused?)
  (constructor: (lambda (self name grid objects start-pos gold-left)
                  (set-fields! self level
                    ((name name)
                     (grid grid)
                     (objects objects)
                     (obj-cache (make-table test: eq?))
                     (score 0)
                     (player-start-pos start-pos)
                     (gold-left gold-left)
                     (lives 3)
                     (time-left 60.)
                     (paused? #f))))))

;; internal funcions
(define (level-cache-add! obj level)
  (table-set! (level-obj-cache level) (game-object-id obj) obj))
(define (level-cache-remove! obj level)
  (table-set! (level-obj-cache level) (game-object-id obj)))

;;; Level interface
(define (level-add! obj lvl)
  ;; insert the object within the layer ordered list
  (level-objects-set! lvl (insert-in-ordered-list < obj (level-objects lvl)
                                                  accessor: get-layer))
  (grid-update (level-grid lvl) obj))

(define (level-delete! obj lvl)
  (update! lvl level objects
           (lambda (objs) (list-remove eq? obj objs)))
  (level-cache-remove! obj lvl)
  (set-fields! obj game-object ((width 0) (height 0)))
  (grid-update (level-grid lvl) obj))

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

(define (within-grid-bounds? p)
  (let ((x (point-x p))
        (y (point-y p)))
    (and (< x grid-width)
         (>= x 0)
         (< y grid-height)
         (>= y 0))))

(define (generate-hole p direction lvl)
  (let* ((hole-x-offset (cadr (assq direction `((left -2) (right 2)))))
         (hole-already-present?
          ;; expecting to have only one grid cell in the
          ;; get-grid-cells call!
          (exists (flip instance-of? 'hole)
                  (grid-get (level-grid lvl)
                            (car (get-grid-cells p
                                                 x-offset: hole-x-offset
                                                 y-offset: -1
                                                 height:   1))))))
    (if (not hole-already-present?)
        (let* ((x (+ (player-x p) hole-x-offset))
               (y (- (player-y p) 2))
               (h (new hole x y 2 2)))
          (and (within-grid-bounds? h)
               (level-add! h lvl))))))

;;; Text label property functions

;; hehe, just to look better in the instantiation code
(define property-list list)

(define (lifetime framecount)
  (let ((x 0))
   (lambda (label level)
     (set! x (+ x 1))
     (if (>= x framecount)
         (die label level)))))

(define (flash frame-interval)
  (let ((x 0))
    (lambda (label level)
      (set! x (modulo (+ x 1) frame-interval))
      (if (zero? x)
          (update! label label visible? (lambda (v?) (not v?)))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Collision detection
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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

;; Handbar collisions
(define-method (resolve-collision (hb handbar) (h human-like) level k)
  (human-like-can-use-rope?-set! h (handbar-y hb))
  ;; the human can fall from where he is...
  (if (not (human-like-can-go-forward? h))
      (human-like-can-go-down?-set! h (human-like-x h))))
(define-method (resolve-collision (h human-like) (hb handbar) level k)
  (resolve-collision hb h level k))

;; Ladder collisions
(define-method (resolve-collision (l ladder) (h human-like) level k)
  (human-like-can-climb-up?-set! h (ladder-x l)))
(define-method (resolve-collision (h human-like) (l ladder) level k)
  (resolve-collision l h level k))

(define-method (resolve-collision (h human-like) (g gold) level k)
  (die g level))
(define-method (resolve-collision (g gold) (h human-like) level k)
  (resolve-collision h g level k))

;; Wall collisions
(define-method (resolve-collision (h human-like) (w wall) level k)
  (let* ((velo (human-like-velocity h))
         (vx (point-x velo))
         (vy (point-y velo)))
    (cond
     ((and (not (zero? vx))
           (not (human-like-stuck-in-hole? h))
           (human-like-can-walk? h))
      (let* ((h.x (human-like-x h))
             (h.w (human-like-width h))
             (w.x (wall-x w))
             (w.w (wall-width w))
             (original-direction (get-direction velo))
             (new-velo (new point (if (< vx 0)
                                      (+ w.x w.w (- h.x))
                                      (- (+ h.x h.w (- w.x))))
                            0)))
        ;; ensure here that the walk flag is set to #t to walk back
        ;;(human-like-can-walk?-set! h #t)
        ;; and move back the character to be in front of the wall
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
;;; Object state management
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-generic change-state!)

;;; Human-like state machine management

(define (get-direction velocity)
    (cond ((< (point-x velocity) 0) 'left)
          ((> (point-x velocity) 0) 'right)
          (else (human-like-facing-direction obj))))

(define (human-like-can-go-forward? h)
  (and (not (human-like-stuck-in-hole? h))
       (or (human-like-can-walk? h)
           (and (human-like-can-use-rope? h)
                (not (human-like-droped-rope? h))))))

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

(define (rope-cycle! p)
  (let* ((cycling-delta 5)
         (cycle-length (* 4 cycling-delta)))
    (update! p player walk-cycle-state
             (lambda (s) (modulo (+ s 1) cycle-length)))
    (let ((next-state
           (case (quotient (player-walk-cycle-state p) cycling-delta)
             ((0) 'rope-1-right)
             ((1) 'rope-2-right)
             ((2) 'rope-1-left)
             ((3) 'rope-2-left) 
             (else (error "Invalid player rope cycle state")))))
      (human-like-facing-direction-set! p (get-direction
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

(define (dying-cycle! p)
  (let* ((cycling-delta 10)
         (cycle-length (* 2 cycling-delta)))
    (update! p player walk-cycle-state
             (lambda (s) (modulo (+ s 1) cycle-length)))
    (let ((next-state
           (case (quotient (player-walk-cycle-state p) cycling-delta)
             ((0) 'left)
             ((1) 'right)
             (else (error "Invalid player walk cycle state")))))
      (human-like-facing-direction-set! p next-state)
      (human-like-state-set! p 'jumping))))

(define (reset-walk-cycle! hum-like)
  ;; leave the direction unchanged...
  (human-like-walk-cycle-state-set! hum-like 0)
  (human-like-state-set! hum-like 'waiting))

(define-method (change-state! (p human-like) level)
  (let* ((v (moving-velocity p)))
    (cond
     ((human-like-stuck-in-hole? p) (dying-cycle! p))
     ((and (not (zero? (point-x v)))
           (human-like-can-walk? p))
      (walk-cycle! p))
     ((human-like-can-use-rope? p)
      (if (not (zero? (point-x v)))
          (rope-cycle! p)
          'keep-same-state^_^))
     ((not (human-like-can-go-forward? p)) (fall-cycle! p))
     ((not (zero? (point-y v))) (ascend-cycle! p))
     (else (reset-walk-cycle! p)))))

;;; hole state management

(define-method (change-state! (h hole) level)
  (let* ((cycling-delta 10)
         (cycle-length (* 10 cycling-delta)))
    (if (>= (hole-appear-cycle-state h) cycle-length)
        (die h level)
        (begin
          (update! h hole appear-cycle-state (curry2 + 1))
          (hole-state-set! h (/ (hole-appear-cycle-state h)
                                cycle-length))))))

(define-method (change-state! (obj game-object) level)
  'do-nothing)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Movement
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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
     ;; Gravity simulation here...
     ((not (human-like-can-go-forward? hum))
      ;; FIXME: not clean  to use player-movement-speed here!
      (human-like-velocity-set! hum
                                (new point 0 (- player-movement-speed)))
      #t)
     ;; Can the player move in x?
     ((and (human-like-can-go-forward? hum)
           (not (zero? v-x)))
      ;; set the y position of the player to correspond to the rope
      ;; FIXME: This operation (- y 2) might be dangerous...?
      (cond ((human-like-can-use-rope? hum)
             => (lambda (y) (human-like-y-set! hum (- y 2)))))
      #t)
     (else
      (human-like-velocity-set! hum point-zero)
      #f))))

(define-method (move! (obj human-like) level k)
  (let* ((velocity (moving-velocity obj)))
    (if (and (not (point-zero? velocity))
             (allowed-to-move!? obj))
        ;; the allowed-to-move!? might have changed the velocity!
        (let ((modified-velocity (moving-velocity obj)))
          (change-state! obj level)
          (point-add! obj modified-velocity)
;;           (pp `(player moved to (,(point-x obj) ,(point-y obj))
;;                        with velocity: (,(point-x modified-velocity)
;;                                        ,(point-y modified-velocity))))
          (validate-grid-bounds! obj) ; make sure player stays in level bounds
;;           (pp `(after validation (,(point-x obj) ,(point-y obj))))
          (grid-update (level-grid level) obj)
          (let* ((objects-below (get-objects-below obj level)))
;;             (pp `(,(map (lambda (x) (cons (game-object-id x)
;;                                           (get-class-id x)))
;;                         objects-below)))
            
            ;; Object state reset
            (set-fields! obj human-like
              ((can-walk?     #f)
               (can-climb-up? #f)
               (can-go-down?  #f)
               (can-use-rope? #f)
               (stuck-in-hole? #f)))

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
                      (human-like-can-walk?-set! obj #t)
                      ;; reset the drop rope flag when above a wall
                      (human-like-droped-rope?-set! obj #f))))
             objects-below)

            ;; Perform collision detection / resolution
            (let* ((colliding-objects (detect-collisions obj level))
                   (above-a-hole?
                    (cond ((exists (flip instance-of? 'hole)
                                   (set-union eq?
                                              objects-below
                                              colliding-objects))
                           => (lambda (h)
                                (and (= (human-like-x obj) (hole-x h))
                                     h)))
                          (else #f))))
              ;; if above a hole, human-like falls!
              (update! obj human-like can-walk?
                       (lambda (can-walk?) (and can-walk?
                                                (not above-a-hole?))))
              (if above-a-hole?
                  (begin
                    (hole-contained-object?-set! above-a-hole? obj)
                    (human-like-stuck-in-hole?-set! obj #t)))
              (for-each (lambda (col-obj)
                          (resolve-collision obj col-obj level k))
                        colliding-objects))))
        ;; resets the object state
        (change-state! obj level))
    (k #t)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Objects death... sniff...
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-generic die)

(define-method (die (g gold) level)
  (let ((score-value 200))
    (update! level level score (lambda (x) (+ x score-value)))
    (update! level level gold-left (lambda (x) (- x 1)))
    (if (zero? (level-gold-left level))
        'make-visible-the-escape-ladder!) ;; <- TODO
    (level-add! (new label
                     (number->string score-value)
                     (point-x g) (point-y g)
                     'red
                     (property-list (lifetime 120)
                                    (flash 10)))
                level)
    (call-next-method)))

(define-method (die (p player) level)
  (update! level level lives (lambda (x) (- x 1)))
  (if (zero? (level-lives level))
      (game-over!)
      (let ((start-pos (level-player-start-pos level)))
        (level-add! (new player (point-x start-pos) (point-y start-pos))
                    level)))
  (call-next-method))

(define-method (die (h hole) level)
  (cond ((hole-contained-object? h)
         => (lambda (obj)
              (let ((coll-obj (detect-collisions h level)))
                ;; ensure that the object is still in the hole...
                (if (memq obj coll-obj)
                    (die obj level))))))
  (call-next-method))

(define-method (die (obj game-object) level)
  (pp `(,(game-object-id obj) died!))
  (level-delete! obj level))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; frame update (game loop)
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-generic animate)

(define-method (animate (p player) level)
  (call/cc (lambda (k)
             (if (not (human-like-can-go-forward? p))
                 (player-velocity-set!
                  p (new point 0 (- player-movement-speed))))
             (move! p level k))))

(define-method (animate (h hole) level)
  (change-state! h level))

(define-method (animate (l label) level)
  (for-each (lambda (p) (p l level))
            (label-properties l)))

(define-method (animate (x game-object) level)
  'do-nothing)

(define (process-game-key key-sym level)
  ;; the keysym are defined in the user-interface module
  (let ((player (level-get 'player level)))
    (if player
        (case key-sym
          [(left)
           (player-velocity-set! player
                                 (new point (- player-movement-speed) 0))]
          [(right)
           (player-velocity-set! player
                                 (new point player-movement-speed 0))]
          [(up)
           (player-velocity-set! player
                                 (new point 0 player-movement-speed))]
          [(down)
           (if (human-like-can-use-rope? player)
               (player-droped-rope?-set! player #t))
           (player-velocity-set! player
                                 (new point 0 (- player-movement-speed)))]
          [(shoot-left)  (generate-hole player 'left  level)]
          [(shoot-right) (generate-hole player 'right level)]))))

(define (advance-frame! level keys)
  ;; not sure what is the good approach at moving objects. The player
  ;; speed is reset every frame.
  (cond ((level-get 'player level) =>
         (lambda (player)
           (player-velocity-set! player point-zero))))

  (if (not (level-paused? level))
      (begin
        (update! level level time-left (lambda (dt) (- dt (fl/ 1. (FPS)))))
        (if (<= (level-time-left level) 0.)
            (game-over! 'timeout)
            (begin
              (for-each (flip process-game-key level) keys)
              (for-each (flip animate level) (level-objects level)))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Object rendering
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-generic get-layer)
(define-method (get-layer (h human-like))
  human-like-layer)
;; holes must be drawn above the stage objects (walls)
(define-method (get-layer (h hole))
  foreground-layer)
(define-method (get-layer (s stage))
  stage-layer)
(define-method (get-layer (obj game-object))
  foreground-layer)

(define (render-object obj texture color char)
  (receive (x y w h) (grid-coord->world-coord obj)
    (draw-textured-object texture color char x y w h)))

(define (render-grid)
  (for j 0 (< j grid-height)
       (for i 0 (< i grid-width)
            (draw-grid-point (* i grid-cell-w) (* j grid-cell-h)))))

(define (format-score x)
  (if (= x 0)
      "000000"
      (let ((pad (max (- 6
                         (##flonum->fixnum (round (/ (log x) (log 10))))
                         1)
                      0)))
        (string-append (make-string pad #\0) (format "~D" x)))))

(define (render-title-bar level)
  (let ((x 0)
        (y (* grid-height grid-cell-h))
        (w 384)
        (h 8))
   (draw-textured-object title_bar 'black 'bar x y w h)
   (render-string 32 y (format-score (level-score level)) 'white)
   (render-string 88 y (format "~0,2F" (level-time-left level)) 'white)
   (render-string 162 y (format-score 999999) 'red)
   (render-string 232 y "01" 'white)
   (render-string 281 y (number->string (level-lives level)) 'white)
   (render-string 321 y (format-score 0) 'white)))

(define-generic render)

(define-method (render (lvl level))
  (render-title-bar lvl)
  
  ;; it is expected that the object list is ordered with increasing
  ;; layer order...
  (for-each render (level-objects lvl)))

(define-method (render (w wall))
  (render-object w wall 'pink 'wall))

(define-method (render (hl hole))
  (receive (x y w h) (grid-coord->world-coord hl)
           (let ((height (##flonum->fixnum
                          (exact->inexact (* h (hole-state hl))))))
      (render-hole x y w h)
      (draw-textured-object wall 'pink 'wall x y w height))))

(define-method (render (g gold))
  (render-object g gold 'regular 'gold))

(define-method (render (hb handbar))
  (render-object hb handbar 'regular 'bar))

(define-method (render (p player))
  (render-object p player (player-facing-direction p) (player-state p)))

(define-method (render (l ladder))
  (render-object l ladder 'regular 'ladder))

(define-method (render (l label))
  (if (gui-visible? l)
      (receive (x y w h) (grid-coord->world-coord l)
        (render-string x y (label-text l) (label-color l)))))
