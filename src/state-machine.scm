
;;(include "class.scm")

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
  (constructor: (lambda (self)
                  ;; Use class-slots dynamically by calling the
                  ;; accessor with an instance... ^_^
                  (if (unbound-class-slot? (state-machine-desc self))
                      (error "State machine descriptor for class "
                             (get-class-id self " was not set!")))
                  (let* ((desc (state-machine-desc self))
                         (init-state (state-machine-desc-initial-state desc)))
                    (state-machine-current-state-set! self init-state)))))

(define (state-machine-start sm)
  (let* ((desc (state-machine-desc sm))
         (init-state (state-machine-desc-initial-state desc)))
    (state-machine-current-state-set! sm init-state)
    ((get-state-action desc init-state) sm)))

(define-macro (define-state-machine
                name
                init-state
                states
                transitions
                #!key (create-new-class? #t))
  (define (transition->method tr)
    ;; Shoul all state machines share the same genfun ? or not?
    (let* ((from-state (car tr))
           (to-state (cadr tr))
           (action (caddr tr))
           (from-ty (cond ((eq? from-state '*) `(from ,name))
                          ((symbol? from-state)
                           `(from (match-member: ,name current-state
                                                 ,from-state)))
                          (else (error "Invalid from state "
                                       "transition syntax:" from-state))))
           (to-ty (cond ((eq? to-state '*) 'to)
                        ((symbol? to-state)
                         `(to (match-value: ,to-state)))
                     (else (error "Invalid from state transition syntax.")))))
      `(define-method (transition ,from-ty ,to-ty)
         (state-machine-current-state-set! from to)
         ;; execute the transition action
         (,(caddr tr) from)
         ;; execute the state action
         ((get-state-action (state-machine-desc from) to) from))))
  `(begin
     ;; Subclass the state-machine meta-class and set its state
     ;; machine descriptor
     ,(if create-new-class? `(define-class ,name (state-machine)) ''nothing)
     (,(symbol-append name '-desc-set!) (new state-machine-desc
                                             ,init-state
                                             ,states))
     ,@(map transition->method transitions)))


