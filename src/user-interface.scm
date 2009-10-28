(include "scm-lib_.scm")
(include "class_.scm")

(define screen-max-x 388)
(define screen-max-y 280)
(define sdl-screen #f)
(define fullscreen-mode? #f)
(define event-thread #f)
(define display-fps? #f)
;;(define current-level (make-parameter #f))

(define (set-bg-color!)
  (glColor3f .3 .3 .3))

(define (render-string x y str color #!key centered?)
  (if (not (eq? color 'black))
      (let ((centered-x
             (if centered?
                 (##flonum->fixnum (exact->inexact
                                    (- x (* (string-length str) 1/2 8))))
                 x))
            (centered-y (if centered? (- y 4) y)))
       (let loop ((i 0) (chars (string->list str)))
         (if (pair? chars)
             (begin
               ;; expecting the bb_fonts to be loaded!
               (draw-char bb_fonts color (car chars) centered-x centered-y i)
               (loop (+ i 1) (cdr chars))))))))

(define (render-fontified-sprite sprite-font x y state color  #!key centered?)
  (if (not (eq? color 'black))
      (let ((centered-x
             (if centered?
                 (##flonum->fixnum (exact->inexact
                                    (- x (* (font-width sprite-font) 1/2))))
                 x))
            (centered-y
             (if centered?
                 (##flonum->fixnum (exact->inexact
                                    (- y (* (font-height sprite-font) 1/2))))
                 y)))
        (draw-char sprite-font color state centered-x centered-y 0))))

(define (render-hole x y w h)
  ;;(set-bg-color!)
  (glColor3f 0. 0. 0.)
  (glBegin GL_TRIANGLE_STRIP)
  (glVertex2i x y)
  (glVertex2i (+ x w) y)
  (glVertex2i x (+ y h))
  (glVertex2i (+ x w) (+ y h))
  (glEnd))

(define (draw-grid-point x y)
  (glColor3f 1. 1. 1.)
  (glBegin GL_POINTS)
  (begin (glVertex2i x y)
         (glVertex2i (+ x 2) y)
         (glVertex2i (+ x 4) y)
         (glVertex2i (- x 2) y)
         (glVertex2i x (+ y 2))
         (glVertex2i x (+ y 4))
         (glVertex2i x (- y 2)))
  (glEnd))

(define (translate x y thunk)
  (glMatrixMode GL_MODELVIEW)
  (glPushMatrix)
  ;;(glLoadIdentity)
  (glTranslatef (exact->inexact x) (exact->inexact y) 0.)
  (thunk)
  (glPopMatrix))

(define (rescale scaling-factor thunk)
  (glMatrixMode GL_MODELVIEW)
  (glPushMatrix)
  ;;(glLoadIdentity)
  (glScalef scaling-factor scaling-factor 1.)
  (thunk)
  (glPopMatrix))

(define (render-pause-screen)
  (let ((w screen-max-x)
        (h (- screen-max-y 24))) ;; <- FIXME: 16 px too much on top of screen?
    (glColor4f 0.1 0.1 0.1 0.5)
    (glBegin GL_QUADS)
    (glVertex2i 0 0)
    (glVertex2i 0 h)
    (glVertex2i w h)
    (glVertex2i w 0)
    (glEnd)
    )
  (translate (/ screen-max-x 2) (/ screen-max-y 2)
             (lambda ()
               (rescale 3.
                        (lambda () 
                          (render-string 0 0  "PAUSE" 'red centered?: #t))))))

(define (render-scene sdl-screen level)
  (SDL::with-locked-surface
   sdl-screen
   (lambda ()
     (set-bg-color!)
     (glClear GL_COLOR_BUFFER_BIT)

     ;; Draw background stuff
     (render level)

     ;;draw frame-rate just over the green line
     (if display-fps?
         (render-string
          0 11 
          (with-output-to-string "" (lambda () (show "FPS: " (FPS))))
          'white))

     ;;(glFlush)
     (SDL::GL::SwapBuffers)
     )))

(define (reshape w h)
  (let* ((zoom-x (/ w screen-max-x))
         (zoom-y (/ h screen-max-y))
         (factor (exact->inexact (ceiling (max zoom-x zoom-y)))))
    (glViewport 0 0 w h)
    (glMatrixMode GL_PROJECTION)
    (glLoadIdentity)
    (gluOrtho2D 0.                            ;;left clip
                (exact->inexact (/ w zoom-x)) ;;right clip
                0.                            ;;bottom clip
                (exact->inexact (/ h zoom-y)));;top
    (glMatrixMode GL_MODELVIEW)
    (glLoadIdentity)))

;;; Keyboard input management

(define key-down-table (make-table test: eq?))
(define (key-down-table-add! key action)
  (table-set! key-down-table key action))
(define (key-down-table-reset-key key)
  (table-set! key-down-table key))

;; only consider the key downs that are not false...
(define (key-down-table-actions)
  (map cdr (filter car (table->list key-down-table))))
(define (key-down-table-keys)
  (filter identity (map car (table->list key-down-table))))

(define key-up-table (make-table test: eq?))
(define (key-up-table-add! key action)
  (table-set! key-up-table key action))
(define (key-up-table-reset-key key)
  (table-set! key-up-table key))
(define (key-up-table-actions)
  (map cdr (table->list key-up-table)))
(define (key-up-table-keys)
  (map car (table->list key-up-table)))
(define (key-up-table-reset!)
  (set! key-up-table (make-table test: eq?)))

(define (key-released key)
  (key-down-table-reset-key key)
  (key-up-table-add! key #t))

(define (->unhandled  evt-struct)
  'todo)

(define (->key-down evt-struct)
  (let ((key-enum  (SDL::key-enum      evt-struct))
        (modifiers (SDL::key-modifiers evt-struct))
        (unicode   (SDL::key-unicode   evt-struct)))
    (case key-enum
      ;; Events handled by the game engine
      [(key-left-arrow)   (key-down-table-add! 'left        #t)]
      [(key-right-arrow)  (key-down-table-add! 'right       #t)]
      [(key-up-arrow)     (key-down-table-add! 'up          #t)]
      [(key-down-arrow)   (key-down-table-add! 'down        #t)]
      [(key-left-control) (key-down-table-add! 'shoot-left  #t)]
      [(key-left-alt)     (key-down-table-add! 'shoot-right #t)]
      [(key-return)       (key-down-table-add! 'enter #t)]
      [(key-1)            (key-down-table-add! 'one #t)]
      [(key-2)            (key-down-table-add! 'two #t)]
      [(key-c)            (key-down-table-add! 'c #t)]
      [(key-p)            (key-down-table-add! 'pause #t)]
      [(key-r)            (key-down-table-add! 'reset #t)]
      ;; Handled directly here...
      [(key-f)            (set! display-fps? (not display-fps?))]
      [(key-q)            (request-exit)]
      )
    ))

(define (->key-up   evt-struct)
  (let ((key-enum  (SDL::key-enum      evt-struct))
        (modifiers (SDL::key-modifiers evt-struct)))
    (case key-enum
      [(key-left-arrow)   (key-released 'left)]
      [(key-right-arrow)  (key-released 'right)]
      [(key-up-arrow)     (key-released 'up)]
      [(key-down-arrow)   (key-released 'down)]
      [(key-p)            (key-released 'pause)]
      [(key-left-control) (key-released 'shoot-left)]
      [(key-left-alt)     (key-released 'shoot-right)]
      [(key-return)       (key-released 'enter)]
      [(key-1)            (key-released 'one)]
      [(key-2)            (key-released 'two)]
      [(key-c)            (key-released 'c)]
      [(key-p)            (key-released 'pause)]
      [(key-r)            (key-released 'reset)])))


;;; Main related stuff

(define (->quit evt-struct)
  (request-exit))

(define (get-video-flags)
  (bitwise-ior SDL::opengl
               SDL::resizable
               ;;(if fullscreen-mode? SDL::fullscreen 0)
               ))


(define (->video-resize evt-struct)
  (let ((w (SDL::resize-w evt-struct))
        (h (SDL::resize-h evt-struct)))
    ;;(SDL::gl-set-attributes SDL::gl-swap-control 0)
    ;;(SDL::gl-set-attributes SDL::gl-doublebuffer 0)
    (SDL::set-video-mode w h 32 (get-video-flags))
    (init-GL w h)
    ))

(define managage-sdl-event
  ;; SDL event structure bits -> Scheme object
  (let ( [xforms (make-vector (+ 1 SDL::num-events) ->unhandled)] )
    (vector-set! xforms SDL::key-down             ->key-down)
    (vector-set! xforms SDL::key-up               ->key-up)
    (vector-set! xforms SDL::quit                 ->quit)
    (vector-set! xforms SDL::video-resize         ->video-resize)
    
    (lambda (sdl-event-struct)
      (let ( (event-type (SDL::raw-event-type sdl-event-struct)) )
        (if (<= 0 event-type SDL::num-events)
            ((vector-ref xforms event-type) sdl-event-struct)
            (->unhandled sdl-event-struct))))))

(define SDL-event-struct (SDL::malloc-event-struct))
(define (poll-SDL-events)
  (if (SDL::poll-event SDL-event-struct)
      (managage-sdl-event SDL-event-struct)))

(define (init-GL w h)
  (glPointSize 1.)
  (glDisable GL_POINT_SMOOTH)

  (glPixelStorei GL_UNPACK_ALIGNMENT 1)
  (glShadeModel GL_FLAT)

  (glEnable GL_BLEND)
  (glBlendFunc GL_SRC_ALPHA GL_ONE)

  (reshape w h)
  )

(define (start-threads!)
  (thread-start! (make-thread (lambda () (##repl)) 'repl))
  )

(define (game-loop start-level)
  (let loop ((render-init-time (time->seconds (current-time)))
             (level start-level))
    (if exit-requested? (quit))
    (poll-SDL-events)
    (let ((current-level (advance-frame! level
                                         (key-down-table-keys)
                                         (key-up-table-keys))))
      (render-scene sdl-screen level)
      (key-up-table-reset!)
      ;; main loop with framerate calculation
      (let* ((now (time->seconds (current-time)))
           (this-fps (floor (/ 1 (- now render-init-time)))))
      (FPS this-fps))
      (##gc)
      ;; (set! GC-INFO (cons (f64vector-ref (##process-statistics) 14)
      ;;                     GC-INFO))
      (loop (time->seconds (current-time)) current-level))))

(define (redraw-loop)
  (SDL::set-window-caption "Lode Runner" "Lode Runner")
  (SDL::set-window-icon (SDL::load-bmp-file "sprites/lode-runner-icon.bmp") #f)

  ;;(SDL::gl-set-attributes SDL::gl-swap-control 0)
  ;;(SDL::gl-set-attributes SDL::gl-doublebuffer 0)
  
  (let ((screen (SDL::set-video-mode screen-max-x screen-max-y 32
                                     (get-video-flags))))
      (if screen
          (call/cc
           (lambda (k)
             ;; initialisation
             (set! sdl-screen screen)
             (set! return
                   (lambda (ret-val)
                     ;;(thread-terminate! event-thread)
                     ;;(thread-terminate! simulation-thread)
                     (k ret-val)))

             ;; initialize textures! (after opengl was enabled
             (for-each (lambda (f) (f)) debug-textures)

             (init-GL screen-max-x screen-max-y)
             (start-threads!)
             (boot (list
                    (new-corout 'main-menu
                                (lambda () (game-loop (new logo-menu))))))))
          (display "Could not set SDL screen")))
  )

(define (request-exit)
  (set! exit-requested? #t))
(define exit-requested? #f)
(define return #f)
(define (quit)
  (profile-stop!)
  (write-profile-report "profiling-report")
  (pp `(average frame-rate: ,(FPS)))
  ;; (pp `(gc min dt: ,(minimum GC-INFO)))
  ;; (pp `(gc average dt: ,(average GC-INFO)))
  ;; (pp `(gc max dt: ,(maximum GC-INFO)))
  ;; (pp (quick-sort < = > GC-INFO))
  (return 0))

;; Main function which only sets up and starts the game threads
(define (main)
  (profile-start!)
  (SDL::within-sdl-lifetime SDL::init-everything
                            redraw-loop))

(cond-expand
 (compiled-version (main))
 (else))