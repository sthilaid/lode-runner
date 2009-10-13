
;;(include "class.scm")

(define-class state-machine-desc ()
  (slot: initial-state)
  (slot: initial-action)
  (slot: states)
  (constructor: (lambda (self init-state init-action states)
                  (if (not (memq init-state states))
                      (error "Invalid state machine description: "
                             "Init state (" init-state
                             ")not present in states"))
                  (set-fields! self state-machine-desc
                    ((initial-state init-state)
                     (initial-action init-action)
                     (states states))))))

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
         (init-state (state-machine-desc-initial-state desc))
         (init-action (state-machine-desc-initial-action desc)))
    (state-machine-current-state-set! sm init-state)
    (init-action sm)))

(define-macro (define-state-machine
                name
                init-state
                init-action
                states
                transitions
                #!key (create-new-class? #t))
  ;; (from-state to-state action) -> 'transition' genfun instance
  (define (transition->method tr)
    (define (bad-syntax) (error "Bad transition syntax: " tr))
    ;; Shoul all state machines share the same genfun ? or not?
    (let* ((from-state (car tr))
           (to-state (cadr tr))
           (action (caddr tr))
           (from-ty (cond ((eq? from-state '*) `(from ,name))
                          ((symbol? from-state)
                           `(from (match-member: ,name current-state
                                                 ,from-state)))
                          (else (bad-syntax))))
           (to-ty (cond ((eq? to-state '*) 'to)
                        ((symbol? to-state) `(to (match-value: ,to-state)))
                        (else (bad-syntax)))))
      (if (or (not (symbol? from-state))
              (not (symbol? to-state)))
          (error "define-state-machine error: "
                 "transition states not a symbol: " (list from-state to-state)))
      `(define-method (transition ,from-ty ,to-ty)
         (state-machine-current-state-set! from to)
         ;; execute the transition action
         (,(caddr tr) from))))
  ;; supports multiple declarations of transitions at the same time
  (define (transitions->methods tr)
    (let* ((from-states (if (list? (car tr)) (car tr) (list (car tr))))
           (to-states (if (list? (cadr tr)) (cadr tr) (list (cadr tr))))
           (action (caddr tr)))
      (fold-l (lambda (acc from-state)
                (append (map (lambda (to-state)
                               (transition->method
                                `(,from-state ,to-state ,action)))
                             to-states)
                        acc))
              '()
              from-states)))
  ;; generate the state predicate verifiying if in a certain state
  (define (gen-state-predicate state-name)
    `(define (,(symbol-append name '- state-name '?) obj)
       (eq? (,(symbol-append name '-current-state) obj)
            (quote ,state-name))))
  ;; transition to state-name alias
  (define (gen-state-modifier state-name)
    `(define (,(symbol-append name '- state-name '!) obj)
       (transition obj (quote ,state-name))))
  
  ;; verification of the format of the states
  (if (not (forall symbol? states))
      (error "define-state-machine error: all states must be a symbol."
             "current states: " states))
  (let ((code
         `(begin
            ;; Subclass the state-machine meta-class and set its state
            ;; machine descriptor
            ,(if create-new-class? `(define-class ,name (state-machine))
                 ''nothing)
            (,(symbol-append name '-desc-set!)
             (new state-machine-desc ,init-state ,init-action (quote ,states)))
            ,@(fold-l (lambda (acc tr) (append (transitions->methods tr)
                                                acc))
                      '()
                       transitions)
            ,@(map gen-state-predicate states)
            ,@(map gen-state-modifier states))))
    code))


