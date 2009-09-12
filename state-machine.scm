
(include "class.scm")

(define-class state-machine-desc ()
  (slot: initial-state)
  (slot: state-actions)
  (constructor: (lambda (self init-state state-actions)
                  (if (not (assq init-state state-actions))
                      (error "Invalid state machine description: "
                             "Init state (" init-state
                             ")not present in state actions"))
                  (set-fields! self state-machine-desc
                    ((initial-state init-state)
                     (state-actions state-actions))))))

(define (get-state-action smd state)
  (cond ((assq state (state-machine-desc-state-actions smd))
         => cadr)
        (else #f)))

(define-class state-machine ()
  (class-slot: desc)
  (slot: current-state)
  (slot: current-action)

  (constructor: (lambda (self)
                  ;; Use class-slots dynamically by calling the
                  ;; accessor with an instance... ^_^
                  (if (unbound-class-slot? (state-machine-desc self))
                      (error "State machine descriptor for class "
                             (get-class-id self " was not set!")))
                  (let* ((desc (state-machine-desc self))
                         (init-state (state-machine-desc-initial-state desc))
                         (init-action (get-state-action desc init-state)))
                    (set-fields! self state-machine
                               ((current-state init-state)
                                (current-action init-action)))))))



(define-macro (define-state-machine
                name
                init-state
                states
                transitions
                #!key (create-new-class? #t))
  (define (transition->method tr)
    ;; Shoul all state machines share the same genfun ? or not?
    `(define-method (transition (from (match-member:
                                       state-machine current-state ,(car tr)))
                                (to   (match-value: ,(cadr tr))))
       (state-machine-current-state-set! from to)
       (state-machine-current-action-set! from )
       (,(caddr tr) from)))
  `(begin
     ;; Subclass the state-machine meta-class and set its state
     ;; machine descriptor
     ,(if create-new-class? `(define-class ,name (state-machine)) ''nothing)
     (,(symbol-append name '-desc-set!) (new state-machine-desc
                                             ,init-state
                                             ,states))
     ,@(map transition->method transitions)))

(define-class binary-analysis (state-machine)
  (slot: binary-number-str)
  (slot: decimal-number)
  (constructor: (lambda (self input)
                  (init! cast: '(state-machine) self)
                  (set-fields! self binary-analysis
                    ((binary-number-str input)
                     (decimal-number 0))))))

(define (binary-analysis-next-state! obj)
  (define (binstr->sym b) (cond ((string=? b "")  'finished!)
                                ((string=? b "0") 'zero)
                                ((string=? b "1") 'one)
                                (else 'error)))
  (let ((next-state (binstr->sym
                     (substring
                      (binary-analysis-binary-number-str obj) 0 1))))
    (update! obj binary-analysis binary-number-str
             (lambda (b) (substring b 1 (string-length b))))
    next-state))
(define (binary-analysis-transition-action self)
  ((state-machine-current-action self)))

(define-state-machine binary-analysis
  'start
  `((start ,(lambda (self)
              (let ((next-state (binary-analysis-next-state! self)))
                (transition self next-state))))
    (zero ,(lambda (self)
             (let ((next-state (binary-analysis-next-state! self)))
               (update! self binary-analysis decimal-number
                        (lambda (x) (* x 2)))
               (transition self next-state))))
    (one ,(lambda (self)
            (let ((next-state (binary-analysis-next-state! self)))
              (update! self binary-analysis decimal-number
                       (lambda (x) (+ (* x 2) 1)))
              (transition self next-state))))
    (error ,(lambda (_) (pp "invalid binary number!")))
    (finished! ,(lambda (self) (binary-analysis-decimal-number self))))
  (('start 'zero binary-analysis-transition-action)
   ('start 'one  binary-analysis-transition-action)
   ('start 'error binary-analysis-transition-action)
   ('one 'two binary-analysis-transition-action)
   ('one 'finished! binary-analysis-transition-action)
   ('one 'error binary-analysis-transition-action)
   ('two 'one binary-analysis-transition-action)
   ('two 'finished! binary-analysis-transition-action)
   ('two 'error binary-analysis-transition-action))
  create-new-class?: #f)


;; Pseudo code

;; (define-state-machine level-sm
;;   init: game-running
;;   exit: (game-running level-sm)
;;   state: <init>
;;   action: <init-action>
;;   states: ((game-running (lambda () (for-each animate! objects)))
;;            (paused (lambda () 'nothing))
;;            (game-over (lambda () 'game-over-animation))
;;            (level-cleared (lambda () 'lvl-clear-anim)))
;;   transitions:
;;    ((game-running paused (lambda () ...))
;;     (paused game-running (lambda () ...))
    
;;     (game-running game-over (lambda () ...))

;;     (game-running level-cleared (lambda () ...))))