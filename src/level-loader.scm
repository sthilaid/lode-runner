(include "class_.scm")

;; not exactly the same as the game grid-with/height
(define loaded-level-grid-width  48)
(define loaded-level-grid-height 31)

(define dummy-load-id          -1)
(define clear-load-id           0)
(define brick-load-id           1)
(define cement-load-id          2)
(define ladder-load-id          3)
(define handbar-load-id         4)
(define trap-door-load-id       5)
(define clear-ladder-load-id    6)
(define gold-load-id            7)
(define robot-load-id           8)
(define player-load-id          9)

(define (load-hole mat i j)
  (let* ((x j) (y i)
         (hole (new hole x y)))
    (for-each (lambda (cell)
                (let ((x (grid-cell-i cell))
                      (y (grid-cell-j cell)))
                 (matrix2d-set! mat y x dummy-load-id)))
              (get-grid-cells hole))
    hole))

(define (load-player mat i j)
  (let* ((x j) (y i)
         (player (new player x y)))
    (for-each (lambda (cell)
                (let ((x (grid-cell-i cell))
                      (y (grid-cell-j cell)))
                 (matrix2d-set! mat y x dummy-load-id)))
              (get-grid-cells player))
    player))

(define (load-gold mat i j)
  (let* ((x j) (y i)
         (gold (new gold x y)))
    (for-each (lambda (cell)
                (let ((x (grid-cell-i cell))
                      (y (grid-cell-j cell)))
                 (matrix2d-set! mat y x dummy-load-id)))
              (get-grid-cells gold))
    gold))

(define (load-robot mat i j)
  (let* ((x j) (y i)
         (robot (new robot x y point-zero)))
    (for-each (lambda (cell)
                (let ((x (grid-cell-i cell))
                      (y (grid-cell-j cell)))
                 (matrix2d-set! mat y x dummy-load-id)))
              (get-grid-cells robot))
    robot))

(define (get-row-width mat x y type-id)
  (let loop ((x (+ x 1)) (w 1))
    (if (and (< x loaded-level-grid-width)
             (= (matrix2d-get mat y x) type-id))
        (loop (+ x 1) (+ w 1))
        w)))

(define (get-col-height mat x y type-id)
  (let loop ((y (+ y 1)) (h 1))
    (if (and (< y loaded-level-grid-height)
             (= (matrix2d-get mat y x) type-id))
        (loop (+ y 1) (+ h 1))
        h)))

(define (load-handbar mat i j)
  ;; confusing because i = y and j = x because of matrix notations
  (let* ((x j)(y i)
         (w (get-row-width mat x y handbar-load-id))
         (handbar (new handbar x y w)))
    (for-each (lambda (cell)
                (let ((x (grid-cell-i cell))
                      (y (grid-cell-j cell)))
                 (matrix2d-set! mat y x dummy-load-id)))
              (get-grid-cells handbar))
    handbar))

(define (load-ladder mat i j)
  (let* ((x j)(y i)
         (h (get-col-height mat x y ladder-load-id))
         (ladder (new ladder x y h)))
    (for-each (lambda (cell)
                (let ((x (grid-cell-i cell))
                      (y (grid-cell-j cell)))
                 (matrix2d-set! mat y x dummy-load-id)))
              (get-grid-cells ladder))
    ladder))

(define (load-clear-ladder mat i j)
  (let* ((x j)(y i)
         (h (get-col-height mat x y clear-ladder-load-id))
         (ladder (new clear-ladder x y h)))
    (for-each (lambda (cell)
                (let ((x (grid-cell-i cell))
                      (y (grid-cell-j cell)))
                 (matrix2d-set! mat y x dummy-load-id)))
              (get-grid-cells ladder))
    ladder))

(define (load-wall mat i j)
  (define (complete-wall-row? x y wished-width)
    (= (get-row-width mat x y brick-load-id) wished-width))
  (define (complete-wall-col? x y wished-height)
    (= (get-col-height mat x y brick-load-id) wished-height))
  
  (let* ((x j) (y i)
         (w (get-row-width  mat x y brick-load-id))
         (h (get-col-height mat x y brick-load-id))
         (real-size
          (if (< h w)
              (let loop ((y (+ y 1)) (h 1))
                (if (and (< y loaded-level-grid-height)
                         (complete-wall-col? x y w))
                    (loop (+ y 1) (+ h 1))
                    (cons w h)))
              (let loop ((x (+ x 1)) (w 1))
                (if (and (< x loaded-level-grid-width)
                         (complete-wall-row? x y h))
                    (loop (+ x 1) (+ w 1))
                    (cons w h)))))
         (wall (new wall x y (car real-size) (cdr real-size))))
    (for-each (lambda (cell)
                (let ((x (grid-cell-i cell))
                      (y (grid-cell-j cell)))
                  (matrix2d-set! mat y x dummy-load-id)))
              (get-grid-cells wall))
    wall))

(define (load-level filename)
  (let* ((lines (call-with-input-file filename
                 (lambda (p) (read-all p read-line))))
         (num-lines
          (map (lambda (l)
                 (call-with-input-string
                  l
                  (lambda (p) (map (lambda (char)
                                     (string->number (list->string (list char))))
                                   (read-all p read-char)))))
               (reverse lines)))
         (mat (make-matrix2d loaded-level-grid-height loaded-level-grid-width)))

    ;; fill up the matrix
    (for-each-with-index (lambda (i row)
                           (for-each-with-index (lambda (j x)
                                                  (matrix2d-set! mat i j x))
                                                row))
                         num-lines)
    (let loop ((i 0) (j 0) (acc '()))
      (if (< i loaded-level-grid-height)
          (if (< j loaded-level-grid-width)
              (let* ((id (matrix2d-get mat i j))
                     (object
                      (and id
                           (cond
                            ;;((= id clear-load-id)   (load-hole mat i j))
                            ((= id player-load-id)  (load-player mat i j))
                            ((= id gold-load-id)    (load-gold mat i j))
                            ((= id robot-load-id)   (load-robot mat i j))
                            ((= id handbar-load-id) (load-handbar mat i j))
                            ((= id ladder-load-id)  (load-ladder mat i j))
                            ((= id clear-ladder-load-id)
                             (load-clear-ladder mat i j))
                            ((= id brick-load-id)   (load-wall mat i j))
                            (else #f)))))
                (if object
                    (loop i (+ j 1) (cons object acc))
                    (loop i (+ j 1) acc)))
              (loop (+ i 1) 0 acc))
          (let* ((level-name (path-strip-directory
                              (path-strip-extension filename)))
                 (clear-ladder (cond ((exists (flip instance-of? 'clear-ladder)
                                              acc)
                                      => identity)
                                     (else (error "Not clear ladder "
                                                  "found in level "
                                                  level-name))))
                 (grid (make-grid))
                 (bottom-wall (new wall 0 0 grid-width 1))
                 (all-objects (cons bottom-wall acc))
                 (number-of-gold (fold-l (lambda (acc x)
                                           (if (gold? x) (+ acc 1) acc))
                                         0
                                         all-objects))
                 (game-start-objects
                  (filter (lambda (x) (not (instance-of? x 'clear-ladder)))
                          all-objects)))
            ;; move up by one all the objects to leave
            ;; room for the bottom wall
            (for-each (lambda (obj)
                        (update! obj game-object y (lambda (y) (+ y 1))))
                      acc)
            ;; the player start pos must be retrieved *after* the
            ;; objects were lifted up of 1 grid cell...
            (let* ((player-start
                    (cond ((exists player? all-objects)
                           => (lambda (p)
                                (new point (player-x p) (player-y p))))
                          (else (error "No player found in the level "
                                       level-name))))
                   (level (new level 
                               level-name
                               grid           ; grid
                               '()            ; objects
                               player-start   ; start pos
                               number-of-gold ; gold-left
                               clear-ladder)))
              (for-each (flip level-add! level) game-start-objects)
              level))))))
