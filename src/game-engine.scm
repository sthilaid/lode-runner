(include "declarations.scm")
(include "scm-lib_.scm")
(include "class.scm")
(include "state-machine.scm")

;; FPS is calculated in the user interface
(define FPS (create-bounded-simple-moving-avg 5 init-value: 60.))

;; Note: The player movement speed must be multipliable to give 1 so
;; accepteble values are 1/8 (*8), 2/8 (*4), 4/8 (*2) and 1. This
;; ensures that the player always falls in holes on the ground...
(define player-movement-speed 1/4)
(define robot-movement-speed 1/8)
(define hole-generation-dt 1.)
(define hole-pass-through-dt 2.)

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

(define (point-norm p)
  (sqrt (+ (expt (point-x p) 2) (expt (point-y p) 2))))

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

(define (point->list p) (list (point-x p) (point-y p)))
(define (list->point l) (new point (car l) (cadr l)))

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

(define (grid-get grid cell)
  (if (and (< (grid-cell-i cell) grid-width)
           (< (grid-cell-j cell) grid-height))
      (matrix2d-get grid
                    (grid-cell-i cell)
                    (grid-cell-j cell))
      '()))

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
  (new rect
       (* (point-x rect)       grid-cell-w)
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
  (slot: appear-stage) ; \in {disappearing empty reappearing}
  (slot: appear-cycle-state)
  (slot: can-pass-through?) ; contains the object creation time or #f
  (slot: contained-object)
  (slot: creator) ; object that created the hole (or #f)
  (constructor:
   (lambda (self x y w h creator lvl) (init! cast: '(stage * * * * *)
                                             self (gensym 'hole) x y w h)
           (set-fields! self hole
             ((state 0)
              (appear-cycle-state 0)
              (contained-object #f)
              (can-pass-through? (level-current-time lvl))
              (creator creator)))
           (hole-disappearing! self))))

(define-class ladder (stage)
  (constructor:
   (lambda (self x y h) (init! cast: '(stage * * * * *)
                               self (gensym 'ladder) x y 2 h))))

(define-class clear-ladder (ladder))

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
  (slot: shooting?)        ; boolean
  (slot: escaping?)        ; 3 states -> {#f possible escaping}
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
                     (walk-cycle-state 0)
                     (shooting? #f)
                     (escaping? #f))))))

(define (fix-obj-y-pos! obj) (update! obj game-object y floor))

(define-class robot (human-like)
  (constructor: (lambda (self x0 y0 initial-velocity)
                  (init! cast: '(human-like * * * *)
                         self x0 y0 initial-velocity (gensym 'robot)))))

(define-class player (human-like)
  (constructor: (lambda (self x0 y0)
                  (init! cast: '(human-like * * * *)
                         self x0 y0 (new point 0 0) 'player))))

(define-class gui (game-object)
  (slot: visible?)
  ;;(slot: zoom-ratio)
  (constructor: (lambda (self id x y w h visible?)
                  ;; FIXME: usage of zeros can be potentially buggy?
                  (init! cast: '(game-object * * * * *)
                         self id x y w h)
                  (set-fields! self gui ((visible? visible?))))))

(define-class label (gui)
  (slot: text)
  (slot: color)
  (slot: properties)
  (constructor: (lambda (self text x y color property-list)
                  (init! cast: '(gui * * * * * *)
                         self (gensym 'label) x y 0 0 #t)
                  (set-fields! self label
                    ((text text)
                     (properties property-list)
                     (color color))))))

;; (define-class image (gui)
;;   (slot: texture)
;;   (constructor: (lambda (self texture x y w h)
;;                   (init! cast: '(gui * * * *)
;;                          self (gensym 'image) x y w h #t)
;;                   (set-fields! self label
;;                     ((texture texture))))))

;; since the control flow is described by the dispatch made on the
;; menu instance, the continuation can be manipulated by changing this
;; instance...
(define-class menu ()
  (slot: name)
  (slot: objects)
  (slot: continuation-menu)
  (constructor:
   (lambda (self name objects)
     (set-fields! self menu ((name name)
                             (objects objects)
                             (continuation-menu self))))))

(define-class logo-menu (menu statefull)
  (slot: last-input-time)
  (slot: anim-cycle)
  (constructor: (lambda (self)
                  (init! cast: '(menu * *) self 'logo-menu '())
                  (set-fields! self logo-menu ((last-input-time 0)
                                               (anim-cycle 0))))))

(define-class option-menu (menu)
  (slot: current-choice)
  (slot: last-input-time)
  (constructor:
   (lambda (self name objects)
                  (init! cast: '(menu * *) self name objects)
                  (set-fields! self option-menu
                               ((current-choice 0)
                                (last-input-time 0))))))

(define-class option-item ()
  (slot: text)
  (slot: action) ; (lambda (val) ...)
  (slot: active?) ; bool
  (constructor: (lambda (self text action)
                  (set-fields! self option-item ((text text)
                                                 (action action)
                                                 (active? #f))))))

(define-class level (menu state-machine)
  (slot: grid)
  (slot: obj-cache)
  (slot: score)
  (slot: player-start-pos)
  (slot: gold-left)
  (slot: lives)
  (slot: time-limit)
  (slot: paused?)
  (slot: state)
  (slot: current-time)
  (slot: clear-ladder)
  (slot: difficulty)
  (constructor: (lambda (self name grid objects start-pos
                              gold-left clear-ladder)
                  (init! cast: '(menu * *) self name objects)
                  (set-fields! self level
                    ((grid grid)
                     (obj-cache (make-table test: eq?))
                     (score 0)
                     (player-start-pos start-pos)
                     (gold-left gold-left)
                     (lives 3)
                     (time-limit 60.)
                     (paused? #f)
                     (state 'in-game)
                     (current-time 0.)
                     (clear-ladder clear-ladder)
                     (difficulty 'hard))))))

(define-state-machine level
  'level-start
  identity ; dont care... yet
  (level-start in-game game-over level-cleared)
  ((* * (lambda (_) 'nothing-yet)))
  create-new-class?: #f)

;; internal funcions
(define (level-cache-add! obj level)
  (table-set! (level-obj-cache level) (game-object-id obj) obj))
(define (level-cache-remove! obj level)
  (table-set! (level-obj-cache level) (game-object-id obj)))

;;; General menu inteface
(define (change-current-level menu next-menu)
  (menu-continuation-menu-set! menu next-menu))

;;; Option menu interface

(define (get-levels)
  (map (lambda (l) (string-append "data/" l))
       (filter (compose (flip string=? ".scm") path-extension)
               (quick-sort string-ci<? string-ci=? string-ci>?
                           (directory-files "data")))))
(define (create-level-option menu level-to-load)
  (new option-item
       (path-strip-directory (path-strip-extension level-to-load))
       (lambda ()
         (with-exception-catcher
          (lambda (e) (println "Could not load level "
                               (path-strip-directory
                                (path-strip-extension level-to-load))))
          (lambda ()
           (change-current-level menu (load-level level-to-load)))))))

(define (create-level-choice-menu)
  (let* ((menu (new option-menu 'level-choice-menu '()))
         (levels (map (curry2 create-level-option menu) (get-levels))))
    (if (not (pair? levels))
        (error "No level found in the data directory...")
        (begin
          (option-menu-objects-set! menu levels)
          (option-menu-choose! menu 0)
          menu))))

(define (option-menu-choose! menu item-index)
  (let* ((objs (option-menu-objects menu))
         (item (list-ref objs item-index)))
    (cond ((get-current-option-choice menu)
           => (flip option-item-active?-set! #f)))
    (option-menu-current-choice-set! menu item-index)
    (option-item-active?-set! item #t)))

(define (option-menu-choose-relative! menu op)
  (option-menu-choose! menu (modulo (op (option-menu-current-choice menu))
                                    (length (option-menu-objects menu)))))
(define (option-menu-choose-prev! menu)
  (option-menu-choose-relative! menu (flip - 1)))
(define (option-menu-choose-next! menu)
  (option-menu-choose-relative! menu (flip + 1)))

(define (get-current-option-choice menu)
  (cond ((option-menu-current-choice menu)
         => (curry2 list-ref (option-menu-objects menu)))))

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

(define generate-hole
  (let ((last-hole-creation-time -1.)) ; private local var...
    (lambda (p direction creator lvl)

      ;; only create a hole if none where created for
      ;; hole-generation-dt secs...
      (let ((now (level-current-time lvl)))
        (if (>= (- now last-hole-creation-time)
                hole-generation-dt)
            (let* ((hole-x-offset (cdr (assq direction
                                             `((left . -2) (right . 2)))))
                   ;; objects that collides with the potential hole,
                   ;; classified per grid cells
                   (objects-colliding-per-grid-cell
                    (map (curry2 grid-get (level-grid lvl))
                                 (get-grid-cells p
                                                 x-offset: hole-x-offset
                                                 y-offset: -1
                                                 width: 2
                                                 height: 1)))
                   (can-create-hole?
                    (and
                     ;; every grid cell must possess a wall (so that
                     ;; we dont create holes in thin air...
                     (forall (curry2 exists (flip instance-of? 'wall))
                             objects-colliding-per-grid-cell)
                     ;; and we can't create holes on top of other
                     ;; objects (holes, ladders, etc...)
                     (forall (flip instance-of? 'wall)
                             (fold-l (curry2* set-union eq?)
                                     '()
                                     objects-colliding-per-grid-cell)))))
              (if can-create-hole?
                  (let* ((x (+ (player-x p) hole-x-offset))
                         (y (floor (- (player-y p) 2)))
                         (h (new hole x y 2 2 creator lvl)))
                    (set! last-hole-creation-time now)
                    (human-like-shooting?-set! creator #t)
                    (and (within-grid-bounds? h)
                         (level-add! h lvl))))))))))

(define (hole-disappearing? h) (eq? (hole-appear-stage h) 'disappearing))
(define (hole-disappearing! h) (hole-appear-stage-set! h 'disappearing))
(define (hole-empty? h) (eq? (hole-appear-stage h) 'empty))
(define (hole-empty! h) (hole-appear-stage-set! h 'empty))
(define (hole-reappearing? h) (eq? (hole-appear-stage h) 'reappearing))
(define (hole-reappearing! h) (hole-appear-stage-set! h 'reappearing))

(define (can-fall-into-hole? h)
  (and (not (hole-contained-object h))
       (hole-empty? h)))

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

;;; Handbar collisions
(define-method (resolve-collision (hb handbar) (h human-like) level k)
  (human-like-can-use-rope?-set! h (handbar-y hb))

  ;; adjust the hl position, it is important to notice here that this
  ;; necessitates a grid update!
  (human-like-y-set! h (- (handbar-y hb) 2))
  (grid-update (level-grid level) h)

  ;; FIXME: since collision resoluiont happens *after* player/hole
  ;; relation managment, this flag must be reset here since no hole
  ;; can co-exist with a rope... ;)
  (human-like-stuck-in-hole?-set! h #f)

  ;; the human can fall from where he is...
  (if (not (human-like-can-go-forward? h))
      (human-like-can-go-down?-set! h (human-like-x h))))

(define-method (resolve-collision (h human-like) (hb handbar) level k)
  (resolve-collision hb h level k))

;;; Ladder collisions
(define-method (resolve-collision (l ladder) (h human-like) level k)
  (human-like-can-climb-up?-set! h (ladder-x l)))
(define-method (resolve-collision (h human-like) (l ladder) level k)
  (resolve-collision l h level k))

;;; Clear ladder collisions
(define-method (resolve-collision (l clear-ladder) (p player) level k)
  (cond ((not (player-escaping? p)) (player-escaping?-set! p 'possible))
        ((>= (player-y p) (+ (clear-ladder-y l)
                             (clear-ladder-height l)
                             (- (player-height p))))
         (level-level-cleared! level)))
  (call-next-method))
(define-method (resolve-collision (p player) (l clear-ladder) level k)
  (resolve-collision l p level k))

;;; Robot collisions
(define-method (resolve-collision (p player) (r robot) level k)
  (die p level))
(define-method (resolve-collision (r robot) (p player) level k)
  (die p level))

;;; Gold collisions
(define-method (resolve-collision (h human-like) (g gold) level k)
  (die g level))
(define-method (resolve-collision (g gold) (h human-like) level k)
  (resolve-collision h g level k))

;;; Wall collisions
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
             (original-direction (human-like-get-direction h))
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

;;; All other collisions
(define-method (resolve-collision x y lvl k)
  'todo)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Object state management
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-generic change-state!)

;;; Human-like state machine management

(define (human-like-get-direction obj)
  (let ((velocity (human-like-velocity obj)))
    (cond ((< (point-x velocity) 0) 'left)
          ((> (point-x velocity) 0) 'right)
          (else (human-like-facing-direction obj)))))

(define (human-like-can-go-forward? h)
  (and (not (human-like-stuck-in-hole? h))
       (or (human-like-can-walk? h)
           (and (human-like-can-use-rope? h)
                (not (human-like-droped-rope? h))))))

(define-macro (define-cyclic-animation name
                #!key base-class cycle-delta cycle-member
                      state-member states other-actions-fun)
  (let ((obj (gensym 'obj))
        (cycling-delta (gensym 'cycling-delta))
        (cycle-length (gensym 'cycle-length))
        (next-state (gensym 'next-state))
        (cycle-member-getter (symbol-append base-class '- cycle-member))
        (state-member-setter (symbol-append base-class '- state-member '-set!))
        )
    `(define (,name ,obj)
       (let* ((,cycle-length (* ,cycle-delta ,(length states))))
         (update! ,obj ,base-class ,cycle-member
                  (lambda (s) (modulo (+ s 1) ,cycle-length)))
         (let ((,next-state
                (case (quotient (,cycle-member-getter ,obj) ,cycle-delta)
                  ,@(map-with-index (lambda (i x) `((,i) ',x)) states)
                  (else (error "Invalid " base-class " " name " cycle")))))
           (,state-member-setter ,obj ,next-state)
           (,other-actions-fun ,obj))))))

(define-cyclic-animation walk-cycle!
  base-class: human-like
  cycle-delta: 5
  cycle-member: walk-cycle-state
  state-member: state
  states: (standing-up standing-left standing-up standing-right)
  other-actions-fun:
    (lambda (obj) (human-like-facing-direction-set!
                   obj (human-like-get-direction obj))))

(define rope-states '(rope-1-right rope-2-right rope-1-left rope-2-left))
(define-cyclic-animation rope-cycle!
  base-class: human-like
  cycle-delta: 5
  cycle-member: walk-cycle-state
  state-member: state
  states: (rope-1-right rope-2-right rope-1-left rope-2-left)
  other-actions-fun:
    (lambda (obj) (human-like-facing-direction-set!
                   obj (human-like-get-direction obj))))

(define-cyclic-animation ascend-cycle!
  base-class: human-like
  cycle-delta: 5
  cycle-member: walk-cycle-state
  state-member: facing-direction
  states: (left right)
  other-actions-fun:
    (lambda (obj) (human-like-state-set! obj 'ladder)))

(define (fall-cycle! p)
  (human-like-state-set! p 'jumping))

(define-cyclic-animation dying-cycle!
  base-class: human-like
  cycle-delta: 10
  cycle-member: walk-cycle-state
  state-member: facing-direction
  states: (left right)
  other-actions-fun:
    (lambda (obj) (human-like-state-set! obj 'jumping)))

(define (shoot-cycle! p)
  (human-like-state-set! p 'shoot))

(define (reset-walk-cycle! hum-like)
  ;; leave the direction unchanged...
  (human-like-walk-cycle-state-set! hum-like 0)
  (human-like-state-set! hum-like 'waiting))

(define-method (change-state! (p human-like) level)
  (let* ((v (moving-velocity p)))
    (cond
     ((eq? (human-like-escaping? p) 'escaping)
      (ascend-cycle! p))
     ((human-like-shooting? p) (shoot-cycle! p))
     ((human-like-stuck-in-hole? p) (dying-cycle! p))
     ((and (not (zero? (point-x v)))
           (human-like-can-walk? p))
      (walk-cycle! p))
     ((human-like-can-use-rope? p)
       (if (or (not (zero? (point-x v)))
               ;; need to initialize the cycle rope cycle
               (not (memq (human-like-state p) rope-states)))
           (rope-cycle! p)
           'keep-same-state^_^))
     ((not (human-like-can-go-forward? p)) (fall-cycle! p))
     ((not (zero? (point-y v))) (ascend-cycle! p))
     (else (reset-walk-cycle! p)))))

;;; hole state management

(define-method (change-state! (h hole) level)
  (let* ((cycling-delta 10)
         (disappear-cycle-length (* 5 cycling-delta))
         (reappear-cycle-length (* 5 cycling-delta)))
    (cond
      ((hole-disappearing? h)
       (if (>= (hole-appear-cycle-state h) disappear-cycle-length)
           (begin (set-fields! h hole
                    ((can-pass-through? (level-current-time level))
                     (state 0.)
                     (appear-cycle-state 0)))
                  (hole-empty! h)
                  (human-like-shooting?-set! (hole-creator h) #f))
           (begin
             (update! h hole appear-cycle-state (curry2 + 1))
             (hole-state-set! h (- 1. (/ (hole-appear-cycle-state h)
                                         disappear-cycle-length))))))
      ((hole-empty? h)
       ;; FIXME: Should the use of time be replace here by usage of
       ;; frame instead (like all the other state macines)?
       (if (>= (- (level-current-time level) (hole-can-pass-through? h))
               hole-pass-through-dt)
           (begin (hole-reappearing! h)
                  (hole-can-pass-through?-set! h #f))))
      ((hole-reappearing? h)
       (if (>= (hole-appear-cycle-state h) reappear-cycle-length)
           (die h level)
           (begin
             (update! h hole appear-cycle-state (curry2 + 1))
             (hole-state-set! h (/ (hole-appear-cycle-state h)
                                   reappear-cycle-length))))))))

;;; Fallback state management

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
      #t)
     (else
      (human-like-velocity-set! hum point-zero)
      #f))))

(define-method (move! (obj human-like) level k)
  (let* ((velocity (moving-velocity obj)))

    ;; if going down and on a rope, than drop the rope
    (if (and (human-like-can-use-rope? obj)
             (< (point-y velocity) 0))
        (human-like-droped-rope?-set! obj #t))
    
    (if (and (not (point-zero? velocity))
             (allowed-to-move!? obj))
        ;; the allowed-to-move!? might have changed the velocity!
        (let ((modified-velocity (moving-velocity obj)))
          (change-state! obj level)
          (point-add! obj modified-velocity)
          (validate-grid-bounds! obj) ; make sure player stays in level bounds
          (grid-update (level-grid level) obj)
          (let* ((objects-below (get-objects-below obj level)))
            
            ;; Object state reset
            (set-fields! obj human-like
              ((can-walk?     #f)
               (can-climb-up? #f)
               (can-go-down?  #f)
               (can-use-rope? #f)
               (stuck-in-hole? #f)))
            ;; FIXME: Not clean! Should maybe abstract this more to
            ;; have clean states between player and robots?
            (if (not (eq? (player-escaping? obj) 'escaping))
                (player-escaping?-set! obj #f))

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
                    (cond ((exists (flip instance-of? 'hole) objects-below)
                           => (lambda (h)
                                ;; ensure that the player is exactly
                                ;; above the hole
                                (and (= (human-like-x obj) (hole-x h))
                                     h)))
                          (else #f)))
                   (inside-a-hole?
                    (cond ((exists (flip instance-of? 'hole) colliding-objects)
                           => (lambda (h)
                                (and (= (human-like-x obj) (hole-x h))
                                     h)))
                          (else #f))))
              ;; if above a hole, human-like falls!
              (update! obj human-like can-walk?
                       (lambda (can-walk?)
                         (and can-walk?
                              (not (and above-a-hole?
                                        (can-fall-into-hole? above-a-hole?))))))
              ;; hole detection/resolution must be performed *before*
              ;; other detections...
              ;; FIXME: not perfect as because contains robot specific code
              (if (and inside-a-hole?
                       (or (and (instance-of? obj 'robot)
                                (robot-y-set! obj (point-y inside-a-hole?)))
                           ;; means the player is stuck inside a flored hole
                           (human-like-can-go-forward? obj)))
                  (begin (human-like-stuck-in-hole?-set! obj #t)
                         (hole-contained-object-set! inside-a-hole? obj)))
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
        (level-add! (level-clear-ladder level) level))
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
      (level-game-over! level)
      (let ((start-pos (level-player-start-pos level)))
        (level-add! (new player (point-x start-pos) (point-y start-pos))
                    level)))
  (call-next-method))

(define-method (die (h hole) level)
  ;; kill all who dare stand in the hole! (Mwahahaha!)
  (cond ((hole-contained-object h) => (flip die level))) 
  (call-next-method))

(define-method (die (obj game-object) level)
  (pp `(,(game-object-id obj) died at (,(point-x obj) ,(point-y obj))))
  (level-delete! obj level))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; frame update (game loop)
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; AI

(define (immobile-ai robot level)
  (robot-velocity-set! robot point-zero))

(define (seeker-ai robot level)
  (define min-value 1)
  (define (can-go-up/down? robot x-axis? y)
    (if (> y 0)
        (robot-can-climb-up? robot)
        (or (robot-can-go-down? robot)
            (and (robot-can-use-rope? robot)
                 ;; wait to be above the player
                 (not x-axis?)))))
  (let ((player (level-get 'player level)))
    (if player
        (let* ((dir (point-sub player robot))
               (x-axis? (>= (abs (point-x dir)) min-value))
               (y-axis? (let ((y (point-y dir)))
                          (and (not (zero? y))
                               (can-go-up/down? robot x-axis? y))))
               (velo (cond
                      (y-axis? (new point 0 robot-movement-speed))
                      (x-axis? (new point robot-movement-speed 0))
                      (else point-zero)))
               (factor (if (< (if y-axis? (point-y dir) (point-x dir)) 0)
                           -1
                           1)))
          (pp `(,(robot-id robot) ,x-axis? ,y-axis? ,(point->list velo)))
          (robot-velocity-set! robot (point-scalar-mult velo factor))))))

(define (get-ai-fun difficulty)
  (case difficulty
    ((easy) immobile-ai)
    ((hard) seeker-ai)
    (else immobile-ai)))

(define (run-ai robot level)
  ((get-ai-fun (level-difficulty level)) robot level))

;;; Animation

(define-generic animate)

(define-method (animate (p player) level)
  (call/cc (lambda (k)
             (let ((v (cond
                       ((eq? (player-escaping? p) 'escaping)
                        (new point 0 player-movement-speed))
                       ((not (human-like-can-go-forward? p))
                        (new point 0 (- player-movement-speed)))
                       (else (player-velocity p)))))
               (player-velocity-set! p v))
             (move! p level k))))

(define-method (animate (r robot) level)
  (run-ai r level)
  (call/cc (lambda (k)
             (if (not (human-like-can-go-forward? r))
                 (robot-velocity-set!
                  r (new point 0 (- robot-movement-speed))))
             (move! r level k))))

(define-method (animate (h hole) level)
  (change-state! h level))

(define-method (animate (l label) level)
  (for-each (lambda (p) (p l level))
            (label-properties l)))

(define-method (animate (x game-object) level)
  'do-nothing)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Menu animations
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-generic process-key)
(define-generic advance-frame!)

;;; Logo Menu animation

(define-method (process-key key-sym (logo logo-menu))
  (case key-sym
    [(one) (change-current-level logo (load-level (car (get-levels))))]
    [(two)
     ;; TODO: this must be arranged later to have comm with these 2
     ;; corouts. Also this new corout should terminate when the game
     ;; is finished.
     (spawn-brother-thunk 'player-2
                          (lambda ()
                            (game-loop (load-level (car (get-levels))))))
     (change-current-level logo (load-level (car (get-levels))))]
    [(c) 'add-credits]))

(define-method (change-state! (logo logo-menu))
  (let* ((cycling-delta 20)
         (cycle-length (* 4 cycling-delta)))
    (update! logo logo-menu anim-cycle
             (lambda (s) (modulo (+ s 1) cycle-length)))
    (let ((next-state
           (case (quotient (logo-menu-anim-cycle logo) cycling-delta)
             ((0) 'empty)
             ((1) 'blue)
             ((2) 'yellow-green)
             ((3) 'red-yellow-green)
             (else (error "Invalid logo-menu cycle state")))))
      (statefull-state-set! logo next-state))))

(define-method (advance-frame! (logo logo-menu) keys-down keys-up)
  (change-state! logo)
  (for-each (flip process-key logo) keys-down)
  (menu-continuation-menu logo))

;;; Option Menu animation

(define-method (process-key key-sym (opt option-menu))
  (let ((now (time->seconds (current-time))))
    (if (> (- now (option-menu-last-input-time opt))
           0.1)
        (begin
          (case key-sym
            [(up) (option-menu-choose-prev! opt)]
            [(down) (option-menu-choose-next! opt)]
            [(enter) (let ((action (option-item-action
                                    (get-current-option-choice opt))))
                       (action))]
            )
          (option-menu-last-input-time-set! opt now)))))

(define-method (advance-frame! (opt option-menu) keys-down keys-up)
  (for-each (flip process-key opt) keys-up)
  (menu-continuation-menu opt))

;;; Level animation
(define-method (process-key key-sym (level level))
  ;; the keysym are defined in the user-interface module
  (let ((player (level-get 'player level)))
    (if (and player (not (player-shooting? player)))
        (begin
         (case key-sym
           [(left)
            (player-velocity-set! player
                                  (new point (- player-movement-speed) 0))]
           [(right)
            (player-velocity-set! player
                                  (new point player-movement-speed 0))]
           [(up)
            (if (player-escaping? player)
                (player-escaping?-set! player 'escaping))
            (player-velocity-set! player
                                  (new point 0 player-movement-speed))]
           [(down)
            (player-velocity-set! player
                                  (new point 0 (- player-movement-speed)))]
           [(shoot-left)
            (player-facing-direction-set! player 'left)
            (generate-hole player 'left  player level)]
           [(shoot-right)
            (player-facing-direction-set! player 'right)
            (generate-hole player 'right player level)])))))

(define (manage-pause keys-down level)
  (if (memq 'pause keys-down)
      (begin
        ;; Unclean... causes unwanted circular coupling with the
        ;; user-interface... :(
        (key-down-table-reset-key 'pause)
        (update! level level paused? (lambda (x) (not x))))))

(define-method (advance-frame! (level level) keys-down keys-up)
  ;; not sure what is the good approach at moving objects. The player
  ;; speed is reset every frame.
  (cond ((level-get 'player level) =>
         (lambda (player)
           (player-velocity-set! player point-zero))))
  (manage-pause keys-down level)
  (cond
   ((level-paused? level) 'do-nothing)
   ((level-game-over? level) 'restart-level) ;; TODO
   ((level-level-cleared? level)
    ;; TODO: score-calculation
    (change-current-level level (create-level-choice-menu)))
   (else
    (update! level level current-time (lambda (t) (+ t (fl/ 1. (FPS)))))
    (if (> (level-current-time level) (level-time-limit level))
        (level-game-over! level)
        (begin
          (for-each (flip process-key level) keys-down)
          (for-each (flip animate level) (level-objects level))))))
  (menu-continuation-menu level))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Object rendering
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-generic get-layer)
(define-generic render)

;;; Layer definitions

(define-method (get-layer (h human-like))
  human-like-layer)
;; holes must be drawn above the stage objects (walls)
(define-method (get-layer (h hole))
  foreground-layer)
(define-method (get-layer (s stage))
  stage-layer)
(define-method (get-layer (obj game-object))
  foreground-layer)

;;; Option menu rendering

(define-method (render (menu option-menu))
  (let* ((x (/ (* grid-width grid-cell-w) 2))
         (y (* grid-height grid-cell-h))
         (delta-y (##inexact->exact (floor (/ y 20.)))))
    (for-each-with-index
     (lambda (i item)
       (render-string x y "Choose your level" 'white centered?: #t)
       (render-string x
                      (- y (* i delta-y) 20)
                      (option-item-text item)
                      (if (option-item-active? item) 'green 'white)
                      centered?: #t))
     (option-menu-objects menu))
))

;;; Logo menu rendering

(define-method (render (menu logo-menu))
  (let* ((x (/ screen-max-x 2))
         (y (* screen-max-y 3/4))
         (delta-y (##inexact->exact (floor (/ y 20.))))
         (state (logo-menu-state menu)))
    (if (not (eq? state 'empty))
        (render-fontified-sprite logo x y 'logo state centered?: #t))))

;;; Game level rendering

(define (render-object obj texture color char)
  (let* ((world-coords (grid-coord->world-coord obj))
         (x (rect-x world-coords))
         (y (rect-y world-coords))
         (w (rect-width world-coords))
         (h (rect-height world-coords)))
    (draw-textured-object texture color char x y w h)))

(define (render-grid)
  (for j 0 (< j grid-height)
       (for i 0 (< i grid-width)
            (draw-grid-point (* i grid-cell-w) (* j grid-cell-h)))))

(define (format-number width x)
  (if (zero? x)
      (make-string width #\0)
      (let ((pad (max (fx- width
                           (##flonum->fixnum
                            (floor (fl/ (exact->inexact (log x)) (fllog 10.))))
                           1)
                      0)))
        (string-append (make-string pad #\0) (number->string x)))))

(define (format-score x)
  (format-number 6 x))

(define (format-time x)
  (define exact-pad-wanted 2)
  (define inexact-pad-wanted 2)
  (let* ((exact-part (##flonum->fixnum x))
         (exact-part-str (format-number 2 exact-part))
         (flo-part (clamp 0. +inf.0 (- x exact-part) 0.01))
         (flo-part-str (format-number 2 (##flonum->fixnum
                                         (fl* (expt 10. inexact-pad-wanted)
                                              flo-part)))))
    (string-append  exact-part-str "." flo-part-str)))

(define (render-title-bar level)
  (let ((x 0)
        (y (* grid-height grid-cell-h))
        (w 384)
        (h 8))
   (draw-textured-object title_bar 'black 'bar x y w h)
   (render-string 32 y (format-score (level-score level)) 'white)
   (render-string 88 y (format-time (- (level-time-limit level)
                                       (level-current-time level)))
                  'white)
   (render-string 162 y (format-score 999999) 'red)
   (render-string 232 y "01" 'white)
   (render-string 281 y (number->string (level-lives level)) 'white)
   (render-string 321 y (format-score 0) 'white)))

(define-method (render (lvl level))
  (render-title-bar lvl)
  
  ;; it is expected that the object list is ordered with increasing
  ;; layer order...
  (for-each render (level-objects lvl))

  (cond
   ((level-paused? lvl) (render-pause-screen))
   ((level-game-over? lvl) '(render-game-over)) ;; TODO
   ((level-level-cleared? lvl) '(render-next-level-anim)) ;; TODO??
   ))

(define-method (render (w wall))
  (render-object w wall 'pink 'wall))

(define-method (render (hl hole))
  (let* ((world-coords (grid-coord->world-coord hl))
         (x (rect-x world-coords))
         (y (rect-y world-coords))
         (w (rect-width world-coords))
         (h (rect-height world-coords)))
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

(define-method (render (r robot))
  (render-object r robot (robot-facing-direction r) (robot-state r)))

(define-method (render (l ladder))
  (render-object l ladder 'regular 'ladder))

(define-method (render (l label))
  (if (gui-visible? l)
      (let* ((world-coords (grid-coord->world-coord l))
             (x (rect-x world-coords))
             (y (rect-y world-coords))
             (w (rect-width world-coords))
             (h (rect-height world-coords)))
        (render-string x y (label-text l) (label-color l)))))
