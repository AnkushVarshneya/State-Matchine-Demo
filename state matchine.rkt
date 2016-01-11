;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname Question4) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
;Assignment#3 Question #4
;Ankush Varshneya
;100853074

(require test-engine/racket-tests)
(require 2htdp/image)
(require 2htdp/universe)

; The program stores the FSM's description, expression typed in so far, 
; the current state (subset of states), states, alphabets, rules (transitions)
; The user types a letter in the alphabet and the state matchine switch's
; states accoring to the rules(transitions) as inicated by the blue highlightingl
; Note: numpad does not work for entering alphabet please use numbers on
; the keyboad above the letters DO NOT USE THE NUMPAD!
; The example FSM excepts the stirng with substring 1001 ie RE:*1001*

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; definition area

(define STATE-RADIUS 50)

; defines for state type
(define T 'TRANSITION)
(define S 'START)
(define A 'ACCEPT)

;window dimentions
(define WIDTH 800)
(define HEIGHT 300)

;extra list accessor definition
(define (caddddr x) (cadddr (cdr x)))
(define (cadddddr x) (caddddr (cdr x)))
(define (caddddddr x) (cadddddr (cdr x)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; data-methods

; Constructs FSM from
; - description: describes what the matchine accepts
; - expression: the expression that has gone through the fsm from user input
; - current-explaination: explanation behind current transition
; - current-state: the current state selected in the fsm (subset of states list)
; - states: list of all possible states in fsm
; - alphabets: list of all possible alphabets in fsm
; - rules: list of all possible rules in fsm
(define (make-FSM description expression current-explaination current-state
                  states alphabets rules)
  (list description expression current-explaination current-state
        states alphabets rules))

; Takes FSM and returns its discripion
(define (FSM-description FSM) (car FSM))
; Takes FSM and returns its current-state
(define (FSM-expression FSM) (cadr FSM))
; Takes FSM and returns its current explaination
(define (FSM-current-explaination FSM) (caddr FSM))
; Takes FSM and returns its current-state
(define (FSM-current-state FSM) (cadddr FSM))
; Takes FSM and returns its states
(define (FSM-states FSM) (caddddr FSM))
; Takes FSM and returns its alphabets
(define (FSM-alphabets FSM) (cadddddr FSM))
; Takes FSM and returns its rules
(define (FSM-rules FSM) (caddddddr FSM))

; Print list of alphabets as a set
(define (print-alphabets sub-list)
  (cond [(null? sub-list)
         ""]
        [(null? (cdr sub-list))
         (car sub-list)]
        [else
         (string-append (car sub-list) ", " (print-alphabets (cdr sub-list)))]))

; Tells if key-event matches anything in the alphabets by crding alphabets list
(define (key-event-in-alphabet? key-event sub-list)
  (cond [(null? sub-list)
         false]
        [(key=? key-event (car sub-list))
         true]
        [else
         (key-event-in-alphabet? key-event (cdr sub-list))]))

; constructs state from label type posn
; - label: label of state
; - type: type of state (TRANSITION, START, ACCEPT)
; - posn: position of state (for GUI purpose)
; Note: the posn-y of accept/start states are different as these states
; have a start/accept label which requires an adjusted posn-y
; so its visually similar in height as the rest of the states
(define (make-state label type posn)
  (list
   label
   type       
   (if (or (equal? type S) (equal? type A))
       (make-posn (posn-x posn) (+ (posn-y posn) (* STATE-RADIUS 0.4)))
       posn)))

; Takes state and returns its label
(define (state-label state) (car state))
; Takes state and returns its type
(define (state-type state) (cadr state))
; Takes state and returns its position (for GUI bases)
(define (state-posn state) (caddr state))

; Construct rule from name transition-list
; - name: name of rule
; - transition-list: this is the definition that calculates the next-state
(define (make-rule name transition-list)
  (list name transition-list))

; Takes rule and returns its name
(define (rule-name rule) (car rule))
; Takes rule and returns its transition-list
(define (rule-transition-list rule) (cadr rule))

; return a rule in a list of rules based on rule-pattern by crding list of rules
; if not found return empty list else recusivly find it!
; calling functions must check for null errors ie. empty list.
(define (get-rule-by-name pattern sub-list)
  (cond [(null? sub-list)
         '()]
        [(equal? (rule-name (car sub-list)) pattern)
         (car sub-list)]
        [else
         (get-rule-by-name pattern (cdr sub-list))]))

; Construct transition current-state next-state description
; - current-state: the current state selected
; - next-state: next-state based on transition
; - description: describe the transaction
(define (make-transition current next description)
  (list current next description))

; Takes transition and returns its current-state
(define (transition-current-state transition) (car transition))
; Takes transition and returns its next-state
(define (transition-next-state transition) (cadr transition))
; Takes transition and returns its description
(define (transition-description transition) (caddr transition))

; returns the transition in a list of transitions based on current
; state in transition by crding down list of transitions
; if not found return empty list else recusivly find it!
; calling functions must check for null errors ie. empty list.
(define (get-transition-by-current-state pattern sub-list)
  (cond [(null? sub-list)
         '()]
        [(equal? (transition-current-state (car sub-list)) pattern)
         (car sub-list)]
        [else
         (get-transition-by-current-state pattern (cdr sub-list))]))

; Get the next state
; If current state gives a transition return the next state of the transition
; else return current state
(define (get-next-state current-state rule)
  (let ([transition
         (get-transition-by-current-state
          current-state
          (if (null? rule)
              '()
              (rule-transition-list rule)))])
    (if (null? transition)
        current-state
        (transition-next-state transition))))

; Get the transition-description
; If current state gives a transition return the next state of the transition
; else return current state
(define (get-transition-description current-state rule)
  (let ([transition
         (get-transition-by-current-state
          current-state
          (if (null? rule)
              '()
              (rule-transition-list rule)))])
    (if (null? transition)
        "No transition applied"
        (transition-description transition))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; example FSM

(define ex-FSM
  (make-FSM
   ;description
   "This FSM only accepts the string with substring 1001"
   ;expresion
   ""
   ;current explaination
   ""
   ;current state
   "a"
   ; states
   (list
    (make-state "a" S (make-posn (* (/ WIDTH 5) 0.5) (/ HEIGHT 2)))
    (make-state "b" T (make-posn (* (/ WIDTH 5) 1.5) (/ HEIGHT 2)))
    (make-state "c" T (make-posn (* (/ WIDTH 5) 2.5) (/ HEIGHT 2)))
    (make-state "d" T (make-posn (* (/ WIDTH 5) 3.5) (/ HEIGHT 2)))
    (make-state "e" A (make-posn (* (/ WIDTH 5) 4.5) (/ HEIGHT 2))))
   ; alphabets
   (list "0" "1")
   ; rules (states x alphabets)
   ; state| a | b | c | d | e 
   ; -----|-------------------
   ;     0| a | c | d | a | e
   ; -----|-------------------
   ;     1| b | a | a | e | e
   (list
    (make-rule
     "1"
     (list
      (make-transition "a" "b" "First 1 has come, goto next-state")
      (make-transition "b" "a" "Expected 0 got 1, goto start-state")
      (make-transition "c" "a" "Expected 0 got 1, goto start-state")
      (make-transition "d" "e"
                       "Second 1 has come, Input String /expression accepted")
      (make-transition "e" "e" "Input String /expression accepted")))
    (make-rule
     "0"
     (list
      (make-transition "a" "a" "Expected 1 got 0, goto start-state")
      (make-transition "b" "c" "First 0 has come, goto next-state")
      (make-transition "c" "d" "Second 0 has come, goto next-state")
      (make-transition "d" "a" "Expected 1 got 0, goto start-state")
      (make-transition "e" "e" "Input String /expression accepted"))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; GUI-methods

; the CANVAS
(define (CANVAS current-world)
  (place-image/align
   (text
    (string-append
     (FSM-description current-world)
     "... Alphabets = {"
     (print-alphabets (FSM-alphabets current-world))
     "}")
    20
    "black")
   0
   0
   "left"
   "top"
   (place-image/align
    (text
     (string-append
      "Numpad doesn't work for entering alphabet,"
      " use numbers above the letters on keyboard"
      )
     20
     "black")
    0
    20
    "left"
    "top"
    (place-image/align
     (text
      (string-append "Explaination: " (FSM-current-explaination current-world))
      20 
      "black")
     0
     (- HEIGHT 25)
     "left"
     "bottom"
     (place-image/align
      (text
       (string-append "Expression Entered: " (FSM-expression current-world))
       20 
       "black")
      0
      HEIGHT
      "left"
      "bottom"
      (place-image                                
       (rectangle WIDTH HEIGHT "solid" "white")
       (/ WIDTH 2)
       (/ HEIGHT 2)
       (empty-scene WIDTH HEIGHT)))))))

;place all the states
(define (place-states sub-list scene current-state)
  (if (null? sub-list)
      scene
      (let ([state-circle 
             (if (equal? (state-label (car sub-list)) current-state)
                 (circle STATE-RADIUS "solid" "blue")
                 (circle STATE-RADIUS "outline" "black"))])
        (place-image
         (place-image
          (text (state-label (car sub-list)) (* STATE-RADIUS 0.8) "black")
          STATE-RADIUS
          STATE-RADIUS
          (cond
            [(equal? (state-type (car sub-list)) S)
             (place-image
              (text "start" (* STATE-RADIUS 0.5) "black")
              STATE-RADIUS
              (* STATE-RADIUS 2.4)
              (place-image
               state-circle
               STATE-RADIUS
               STATE-RADIUS
               (rectangle (* STATE-RADIUS 2)
                          (* STATE-RADIUS 2.8)
                          "solid"
                          "white")))]
            [(equal? (state-type (car sub-list)) T)
             state-circle]
            [(equal? (state-type (car sub-list)) A)
             (place-image
              (text "accept" (* STATE-RADIUS 0.5) "black")
              STATE-RADIUS
              (* STATE-RADIUS 2.4)
              (place-image
               (place-image
                (circle (* STATE-RADIUS 0.75) "outline" "black")              
                STATE-RADIUS
                STATE-RADIUS
                state-circle)
               STATE-RADIUS
               STATE-RADIUS
               (rectangle (* STATE-RADIUS 2)
                          (* STATE-RADIUS 2.8)
                          "solid"
                          "white")))]))
         (posn-x (state-posn (car sub-list)))
         (posn-y (state-posn (car sub-list)))
         (place-states (cdr sub-list) scene current-state)))))

;;;;
; Functions to pass to big-bang

; The initial state of the world when the program starts
(define INIT-WORLD ex-FSM)

; Take the current-world state given by the universe and draw the scene
(define (redraw current-world)
  (place-states
   (FSM-states current-world)
   (CANVAS current-world)
   (FSM-current-state current-world)))

; Take the current world and make a new one using an updated FSM
(define (change-current-world-key current-world key-event) 
  (if (key-event-in-alphabet? key-event (FSM-alphabets current-world))
      (make-FSM       
       (FSM-description current-world)   
       (string-append (FSM-expression current-world) key-event)    
       (get-transition-description
        (FSM-current-state current-world)
        (get-rule-by-name key-event (FSM-rules current-world)))   
       (get-next-state
        (FSM-current-state current-world)
        (get-rule-by-name key-event (FSM-rules current-world)))       
       (FSM-states current-world)
       (FSM-alphabets current-world)
       (FSM-rules current-world))
      current-world))

; Ask whether game is over
; Note game is never over as the FSM in always expecting values
(define (over? current-world) false)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(big-bang
 ; the "state" in its initial form 
 ; - it will be passed around to the other functions below
 INIT-WORLD
 ; draw the scene again with current "state"
 (on-draw redraw)
 ; update the scene according to key presses
 (on-key change-current-world-key)
 ; check whether everything should stop (win, lose?)
 (stop-when over?))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Checks

; making fsa
(check-expect
 ex-FSM
 (list
  "This FSM only accepts the string with substring 1001"
  ""
  ""
  "a"
  (list
   (list "a" 'START (make-posn 80 (+ (/ HEIGHT 2) (* STATE-RADIUS 0.4))))
   (list "b" 'TRANSITION (make-posn 240 (/ HEIGHT 2)))
   (list "c" 'TRANSITION (make-posn 400 (/ HEIGHT 2)))
   (list "d" 'TRANSITION (make-posn 560 (/ HEIGHT 2)))
   (list "e" 'ACCEPT (make-posn 720 (+ (/ HEIGHT 2) (* STATE-RADIUS 0.4)))))
  (list "0" "1")
  (list
   (list
    "1"
    (list
     (list "a" "b" "First 1 has come, goto next-state")
     (list "b" "a" "Expected 0 got 1, goto start-state")
     (list "c" "a" "Expected 0 got 1, goto start-state")
     (list "d" "e" "Second 1 has come, Input String /expression accepted")
     (list "e" "e" "Input String /expression accepted")))
   (list
    "0"
    (list
     (list "a" "a" "Expected 1 got 0, goto start-state")
     (list "b" "c" "First 0 has come, goto next-state")
     (list "c" "d" "Second 0 has come, goto next-state")
     (list "d" "a" "Expected 1 got 0, goto start-state")
     (list "e" "e" "Input String /expression accepted"))))))

(test)