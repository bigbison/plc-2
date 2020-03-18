;*******************************************
; Ben Trabold
; Remnbrandt van der Ploeg
;
; EECS 345 Interpreter Part 1
;
;********************************************


#lang racket
(require "simpleParser.rkt")
(require "statefunction.rkt")

(define operator car)
(define operand1 cadr)
(define operand2 caddr)

; input file name to interpret file
(define main
  (lambda (file)
    (run (parser file) '(()()) )
    ))

(define run
  (lambda (instructions state)
    (cond
      ((boolean? state) (truefalse state))
      ((number? state) state)
      ((null? instructions) (error 'parse "no valid results found"))
      (else (run (cdr instructions) (process-statement (car instructions) state))))))



(define truefalse
  (lambda (input)
    (cond
      ((eq? input #t) 'true)
      ((eq? input #f) 'false)
      )
    )
  )

; typical m_value
(define M_value
  (lambda (expression state)
    (cond
      ((eq? expression #f) 'false)
      ((eq? expression #t) 'true)
      ((null? expression) (error 'parser "parser should have caught this"))
      ((number? expression) expression)
      ((symbol? expression) (retrieve-var-state expression state))
      ((eq? '+ (car expression)) (+ (M_value(cadr expression) state) (M_value (caddr expression) state )))
      ((eq? '* (car expression)) (* (M_value(cadr expression) state) (M_value (caddr expression) state )))
      ((and (eq?(length expression) 3)(eq? '- (car expression))) (- (M_value(cadr expression) state) (M_value (caddr expression) state )))
      ((and (eq?(length expression) 2)(eq? '- (car expression))) (- 0 (M_value (cadr expression) state )))
      ((eq? '/ (car expression)) (quotient (M_value(cadr expression) state) (M_value (caddr expression) state )))
      ((eq? '% (car expression)) (modulo (M_value(cadr expression) state) (M_value (caddr expression) state )))
      (else (M_boolean expression state))))) ; catches assignment with boolean cases

(define M_boolean
  (lambda (expression state)
    (cond
      ((eq? 'true expression) #t)
      ((eq? 'false expression) #f)
      ((null? expression) (error 'parser "invalid op"))
      ((symbol? expression) (retrieve-var-state expression state))
      ((eq? '== (car expression)) (eq? (M_value (cadr expression) state) (M_value (caddr expression) state)))
      ((eq? '!= (car expression)) (not (eq? (M_value (cadr expression) state) (M_value (caddr expression) state))))
      ((eq? '< (car expression)) (< (M_value (cadr expression) state) (M_value (caddr expression) state)))
      ((eq? '> (car expression)) (> (M_value (cadr expression) state) (M_value (caddr expression) state)))
      ((eq? '<= (car expression)) (<= (M_value (cadr expression) state) (M_value (caddr expression) state)))
      ((eq? '>= (car expression)) (>= (M_value (cadr expression) state) (M_value (caddr expression) state)))
      ((eq? '&& (car expression)) (and (M_boolean (cadr expression) state) (M_boolean (caddr expression) state)))
      ((eq? '|| (car expression)) (or (M_boolean (cadr expression) state) (M_boolean (caddr expression) state)))
      ((eq? '! (car expression)) (not (M_boolean (cadr expression) state)))
      )))

(define M_declare
  (lambda (var state)
    (declare-var-state var state)))

(define M_declare-assign
  (lambda (var expression state)
    (assign-var-state var (M_value expression (declare-var-state var state)) (declare-var-state var state))))
                      

(define M_return
  (lambda (expression state)
      (M_value expression state)))

; This only works if the statement has already been declared
(define M_assign
  (lambda (expression state)
    (assign-var-state (cadr expression) (M_value (caddr expression) state) state)))

(define M_if
  (lambda (condition then state)
    (cond
      ((M_boolean condition state) (process-statement then state))
      (else state))))

(define M_if-else
  (lambda (condition then else state)
    (cond
      ((M_boolean condition state) (process-statement then state))
      (else (process-statement else state) ))))


(define M_while
  (lambda (condition body state)
    (cond
      ((M_boolean condition state) (M_while condition body (process-statement body state)))
      (else state))))

; processes statements and changes the state accordingly
(define process-statement
  (lambda (statement state)
    (cond
      ((declare? statement) (M_declare (cadr statement) state))
      ((declare-assignment? statement) (M_declare-assign (cadr statement)
                                       (caddr statement) state))
      ((assignment? statement) (M_assign statement state)) 
      ((while? statement) (M_while (cadr statement) (caddr statement) state))
      ((if-else? statement) (M_if-else (cadr statement) (caddr statement)
                                       (cadddr statement) state))
      ((if? statement) (M_if (cadr statement) (caddr statement) state))
      ((return? statement) (M_return (cadr statement) state))
      (else (error 'statement "invalid statement")))))

; checks if our statement is a return statement
(define return?
  (lambda (stmt)
    (cond
      ((eq? 'return (car stmt)) #t)
      (else #f))))

; checks if our statement is an assign statement
(define assignment?
  (lambda (statement)
    (cond
      ((eq? '= (car statement)) #t)
      (else #f))))

; checks if we have an if else statement
; must be called prior to the plain if statement
(define if-else?
  (lambda (statement)
    (cond
      ((eq? (length statement) 2) #f) ; no else
      ((and (eq? 'if (car statement)) (eq? (length statement) 4)) #t)
      (else #f))))

; checks if we have an if statement by itself
(define if?
  (lambda (statement)
    (cond
      ((and (eq? (length statement) 3) (eq? 'if (car statement))) #t)
      (else #f))))

; checks if we have a while statement
(define while?
  (lambda (statement)
    (cond
      ((eq? 'while (car statement)) #t)
      (else #f))))

; chekcs if we have a simple declare statement
(define declare?
  (lambda (statement)
    (cond
      ((and (eq? (length statement) 2) (eq? 'var (car statement))) #t)
      (else #f))))

; checks if we have a declaration and assignment in the same statement
; TODO check if this handles this statement correctly
(define declare-assignment?
  (lambda (statement)
    (cond
      ((and (eq? 'var (car statement)) (not (null? (caddr statement)))) #t)
      (else #f))))

;list of variables
(define var-list car)

;list of values
(define val-list cadr)

; rebuilds the correct state format from seperate lists of states and values
(define rebuild
  (lambda (lis1 lis2)
    (cons  lis1 (list lis2))
    ))

; first part of the state, example for '((x y z) (1 2 3))
; it returns '((x) (1))
(define first-state
  (lambda (state)
    (list (caar state) (caadr state))))

; rest of the state besides the first, ex. for '((x y z) (1 2 3))
; it returns '((y z) (2 3))
(define later-state
  (lambda (state)
    (list (cdar state) (cdadr state))))

; attatches the first part of the state to the last, helps with recursing
; through the state list
(define attatch-state
  (lambda (first later)
    (list (cons (car first) (car later)) (cons (cadr first) (cadr later)))))


; retrives the current value of a varaible
(define retrieve-var-state
  (lambda (var state)
    (cond
      ((not (been-declared? var state)) (error var "variable used before declaration"))
      ((null? state) null)
      ((null? var) null)
      ((and (eq? var (caar state)) (eq? (caadr state) 'undf))
       (error var "variable not assigned")) ; throw error if not assigned
      ((eq? var (caar state)) (caadr state))
      ((and (eq? var (caar state)) (eq? (caar state) 'true)) #t)
      ((and (eq? var (caar state)) (eq? (caar state) 'false)) #f)
      (else (retrieve-var-state var (rebuild (cdr (car state)) (cdr (cadr state)))))
      )))

; declares a varaible and adds it to the state list
(define declare-var-state
  (lambda (var state)
    (cond
      ((been-declared? var state) (error var "variable already declared"))
      ((null? var) null)
      ((null? state) null)
      ((attatch-state (list var 'undf) state)))))

; assigns a value to a variable
(define assign-var-state
  (lambda (var val state)
    (cond
      ((not (been-declared? var state))(error var "variable used before declaration"))
      ((null? val) null)
      ((null? var) null)
      ((eq? (car (var-list state)) var) (list (var-list state)
                                              (cons val (cdr (val-list state)))))
      (else (attatch-state (first-state state)
                           (assign-var-state var val (later-state state)))))))

;check if variable has been declared
(define been-declared?
  (lambda (var state)
    (cond
      ((null? state) #f)
      ((null? (car state)) #f)
      ((eq? (car (first-state state)) var) #t)
      (else (been-declared? var (later-state state))))))



    
