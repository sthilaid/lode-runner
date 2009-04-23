(include "scm-lib-macro.scm")
(include "opengl-header.scm")

(define screen-max-x 388)
(define screen-max-y 280)
(define sdl-screen #f)
(define event-thread #f)
(define display-fps? #f)
(define FPS (create-bounded-simple-moving-avg 10))

(define (render-string x y str color)
  (if (not (eq? color 'black))
      (let loop ((i 0) (chars (string->list str)))
        (if (pair? chars)
            (begin
              ;; expecting the bb_fonts to be loaded!
              (draw-char bb_fonts color (car chars) x y i)
              (loop (+ i 1) (cdr chars)))))))

(define (render-fontified-sprite sprite-font x y state color)
  (if (not (eq? color 'black))
      (draw-char sprite-font color state x y 0)))

(define (render-scene sdl-screen objects)
  (SDL::with-locked-surface
   sdl-screen
   (lambda ()
     (glClearColor 0. 0. 0. 0.)
     (glClear GL_COLOR_BUFFER_BIT)

     ;; Draw background stuff
     (for-each render objects)

     ;;draw frame-rate just over the green line
     (if display-fps?
         (render-string
          0 11 
          (with-output-to-string "" (lambda () (show "FPS: " (FPS) "alkj;adsf;lkj;lkjf;lksfalkj;adsf;lkj;lkjf;lksfalkj;adsf;lkj;lkjf;lksfalkj;adsf;lkj;lkjf;lksfalkj;adsf;lkj;lkjf;lksfalkj;adsf;lkj;lkjf;lksfalkj;adsf;lkj;lkjf;lksfalkj;adsf;lkj;lkjf;lksfalkj;adsf;lkj;lkjf;lksfalkj;adsf;lkj;lkjf;lksfalkj;adsf;lkj;lkjf;lksf")))
          'white))

     (glFlush)
     (SDL::GL::SwapBuffers))))

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

(define (->unhandled  evt-struct)
  'todo)

(define (->key-down evt-struct)
  (let ((key-enum  (SDL::key-enum      evt-struct))
        (modifiers (SDL::key-modifiers evt-struct))
        (unicode   (SDL::key-unicode   evt-struct)))
    (case key-enum
      [(key-left-arrow)   (key-down-table-add!
                           'left
                           (lambda () 'todo))]
      [(key-right-arrow)  (key-down-table-add!
                           'right
                           (lambda () 'todo))]

      [(key-f)            (set! display-fps? (not display-fps?))]
      [(key-q)            (request-exit)])
    ))

(define (->key-up   evt-struct)
  (let ((key-enum  (SDL::key-enum      evt-struct))
        (modifiers (SDL::key-modifiers evt-struct)))
    (case key-enum
      [(key-left-arrow)   (key-down-table-reset-key 'left)]
      [(key-right-arrow)  (key-down-table-reset-key 'right)])
    ))

(define (->quit evt-struct)
  (request-exit))

(define (->video-resize evt-struct)
  (let ((w (SDL::resize-w evt-struct))
        (h (SDL::resize-h evt-struct)))
    (SDL::set-video-mode w h 32 (bitwise-ior SDL::opengl SDL::resizable))
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
            (->unhandled sdl-event-struct))))
))

(define key-down-table (make-table))
(define (key-down-table-add! key action)
  (table-set! key-down-table key action))
(define (key-down-table-reset-key key)
  (table-set! key-down-table key))
(define (key-down-table-actions)
  (map cdr (table->list key-down-table)))

(define (event-thread-thunk)
  (let ((evt-struct (SDL::malloc-event-struct)))
    (let poll-loop ((event-or-false (SDL::poll-event evt-struct)))
      (if event-or-false
          (begin
            (managage-sdl-event evt-struct)
            (poll-loop (SDL::poll-event evt-struct)))
          (begin
            (for-each (lambda (x) (x)) (key-down-table-actions))
            (thread-sleep! 0.01)
            (poll-loop (SDL::poll-event evt-struct)))))))

(c-declare "int argc = 0;")
(define (init-GL w h)
  (glPointSize 1.)
  (glDisable GL_POINT_SMOOTH)

  (glPixelStorei GL_UNPACK_ALIGNMENT 1)
  (glShadeModel GL_FLAT)

  (glEnable GL_BLEND)
  (glBlendFunc GL_SRC_ALPHA GL_ONE)

  (for-each (lambda (f) (f)) debug-textures)

  (reshape w h)
  )

(define (start-threads!)
  ;;(set! simulation-thread (make-thread (game-loop (current-thread))))
  (set! event-thread      (make-thread event-thread-thunk))
  ;;(thread-start! simulation-thread)
  (thread-start! event-thread)
  )

(define (redraw-loop)
  (SDL::set-window-caption "Space Invaders" "Space Invaders")
  (SDL::set-window-icon (SDL::load-bmp-file "sprites/lode-runner-icon.bmp") #f)
  (let ( (screen (SDL::set-video-mode
                  screen-max-x screen-max-y 32
                  (bitwise-ior  SDL::opengl SDL::resizable))) )
      (if screen
          (call/cc
           (lambda (k)
             ;; initialisation
             (set! sdl-screen screen)
             (set! return
                   (lambda (ret-val)
                     (thread-terminate! event-thread)
                     ;;(thread-terminate! simulation-thread)
                     (k ret-val)))
             (init-GL screen-max-x screen-max-y)
             (start-threads!)
             ;; main loop with framerate calculation
             (let loop ((render-init-time (time->seconds (current-time))))
               (if exit-requested? (quit))
               (let ((objects (advance-frame!)))
                 (render-scene screen objects))
               (let* ((now (time->seconds (current-time)))
                      (this-fps (floor (/ 1 (- now render-init-time)))))
                 (FPS this-fps))
               (loop (time->seconds (current-time))))))
          (display "Could not set SDL screen")))
  )

(define usage-message "USAGE: ./space-invaders\n")

(define (request-exit)
  (set! exit-requested? #t))
(define exit-requested? #f)
(define return #f)
(define (quit) (pp `(average frame-rate: ,(FPS))) (return 0))

;; Main function which only sets up and starts the game threads
(define (main)
  (SDL::within-sdl-lifetime SDL::init-everything
                            redraw-loop))
