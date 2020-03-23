;*******************************************
; Ben Trabold
; Remnbrandt van der Ploeg
;
; EECS 345 Interpreter Part 1
;
;********************************************


#lang racket
(require "simpleParser.rkt")
;(require "statefunction.rkt")

(define operator car)
(define operand1 cadr)
(define operand2 caddr)

(define val caddr)
(define value car)
(define state cadr)
; list of blocks after a begin or try
(define blocks cdr)
; Current (first) code block in blocks
(define current-block car)
; the condition before a block
(define condition cadr)
; body of a code block after a conditional
(define body caddr)
; block after an else statement
(define else-stmt cadddr)
; Code block following a try
(define try-block car)
; block following a catch
(define catch cdadr)
; block after a catch
(define catch-block cadr)
; exception of catch statement
(define exception caar)
; block after a finally
(define (finally-block x) (cadar (cddr x)))

(define initial-break (lambda (v) (error "break not in a loop")))
(define initial-throw (lambda (v1 v2) (error "throw not inside try")))
(define initial-continue (lambda (v) (error "continue not in a loop")))

; input file name to interpret file

(define main
  (lambda (file)
    (run (parser file) '((()())))
    ))

(define run
  (lambda (instructions state)
    (call/cc
     (lambda (return)
       (if (null? instructions)
           (error "no return statement")
           (run (cdr instructions)  
                (process-statement (car instructions) state (lambda (v) (return (car v))) initial-break initial-continue initial-throw)))))))


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
  (lambda (expression state throw)
    (cond
      ((eq? expression #f) 'false)
      ((eq? expression #t) 'true)
      ((null? expression) (error 'parser "parser should have caught this"))
      ((number? expression) expression)
      ((symbol? expression) (state-layer-get expression state))
      ((eq? '+ (car expression)) (+ (M_value(cadr expression) state throw) (M_value (caddr expression) state throw)))
      ((eq? '* (car expression)) (* (M_value(cadr expression) state throw) (M_value (caddr expression) state throw)))
      ((and (eq?(length expression) 3)(eq? '- (car expression))) (- (M_value(cadr expression) state throw) (M_value (caddr expression) state throw)))
      ((and (eq?(length expression) 2)(eq? '- (car expression))) (- 0 (M_value (cadr expression) state throw)))
      ((eq? '/ (car expression)) (quotient (M_value(cadr expression) state throw) (M_value (caddr expression) state throw)))
      ((eq? '% (car expression)) (modulo (M_value(cadr expression) state throw) (M_value (caddr expression) state throw)))
      (else (M_boolean expression state)))))


(define M_boolean
  (lambda (expression state throw)
    (cond
      ((eq? 'true expression) #t)
      ((eq? 'false expression) #f)
      ((null? expression) (error 'parser "invalid op"))
      ((symbol? expression) (retrieve-var-state expression state))
      ((eq? '== (car expression)) (eq? (M_value (cadr expression) state throw) (M_value (caddr expression) state throw)))
      ((eq? '!= (car expression)) (not (eq? (M_value (cadr expression) state throw) (M_value (caddr expression) state throw))))
      ((eq? '< (car expression)) (< (M_value (cadr expression) state throw) (M_value (caddr expression) state throw)))
      ((eq? '> (car expression)) (> (M_value (cadr expression) state throw) (M_value (caddr expression) state throw)))
      ((eq? '<= (car expression)) (<= (M_value (cadr expression) state throw) (M_value (caddr expression) state throw)))
      ((eq? '>= (car expression)) (>= (M_value (cadr expression) state throw) (M_value (caddr expression) state throw)))
      ((eq? '&& (car expression)) (and (M_boolean (cadr expression) state throw) (M_boolean (caddr expression) state throw)))
      ((eq? '|| (car expression)) (or (M_boolean (cadr expression) state throw) (M_boolean (caddr expression) state throw)))
      ((eq? '! (car expression)) (not (M_boolean (cadr expression) state throw)))
      )))

(define M_declare
  (lambda (var state)
    (state-layer-declare var state)))


(define M_declare-assign
  (lambda (var expression state throw)
    (state-layer-assign var (M_value expression (state-layer-declare var state) throw)
                      (state-layer-declare var state))))


(define M_return
  (lambda (expression state return throw)
        (return (M_value expression state throw)))) ; <--- this doesn't work but we can't figure out why


(define M_assign
  (lambda (var expression state throw)
    (state-layer-assign var (M_value expression state throw) state)))


(define M_if
  (lambda (condition then state return break continue throw)
    (cond
      ((M_boolean condition state throw) (process-statement then state return break continue throw))
      (else state))))


(define M_if-else
  (lambda (condition then else state return break continue throw)
    (cond
      ((M_boolean condition state) (process-statement then state return break continue throw))
      (else (process-statement else state return break continue throw)))))

; returns the state after a while statement
(define M_while
  (lambda (condition body state return throw)
    (call/cc
     (lambda (break)
       (call/cc
        (lambda (continue)
          (letrec ((loop (lambda (loop-body state)
                           (if (value (M_boolean condition state throw))
                               (loop loop-body (process-statement body state return break (lambda (v) (continue (loop loop-body (state-layer-pop v)))) throw))
                               state))))
            (loop body state))))))))


; returns the state after executing a try statement
(define M_try
  (lambda (statements state return break continue throw)
    (call/cc
     (lambda (throw2)
       (M_begin (try-block statements) state return break continue (lambda (v1 v2) (throw2 (M_catch v1 (catch statements) v2 return break continue throw))))))))

; returns the state after executing a catch statement
(define M_catch
  (lambda (excep statements state return break continue throw)
    (state-layer-pop (M_begin (catch-block statements) (state-layer-assign (exception statements) excep (state-layer-declare (exception statements) (state-layer-push (empty-state) state))) return break continue throw))))



; returns the state after executing a try statement with a finally
(define M_try-finally
  (lambda (statements state return break continue throw)
    (call/cc
     (lambda (throw2)
       (M_begin (finally-block statements)
                (M_begin (try-block statements) state return break continue
                         (lambda (v1 v2) (throw2 (M_begin (finally-block statements)
                                                          (M_catch v1 (catch statements) v2 return break continue throw) return break continue throw)))) return break continue throw)))))


; returns the state after execution of the code block
(define M_begin
  (lambda (statements state return break continue throw)
    (letrec ((loop (lambda (statements state)
                     ;(display s)
                     ;(display "\n")
                     (cond
                       ((null? statements) (state-layer-pop state))
                       (else (loop (blocks statements) (process-statement (current-block statements) state return (lambda (v) (break (state-layer-pop v))) continue (lambda (v1 v2) (throw v1 (state-layer-pop v2))))))))))
      (loop statements (state-layer-push (empty-state) state)))))


; processes statements and changes the state accordingly
(define process-statement
  (lambda (statement state return break continue throw)
    (cond
      ((declare? statement) (M_declare (cadr statement) state))
      ((declare-assignment? statement) (M_declare-assign (cadr statement)
                                       (caddr statement) state throw))
      ((assignment? statement) (M_assign (cadr statement) (caddr statement) state throw))
      ((while? statement) (M_while (cadr statement) (caddr statement) state return throw))
      ((if-else? statement) (M_if-else (cadr statement) (caddr statement)
                                       (cadddr statement) state return break continue throw))
      ((if? statement) (M_if (cadr statement) (caddr statement) state return break continue throw))
      ((return? statement) (M_return (cadr statement) state return throw))
      ((begin? statement) (M_begin(blocks statement) state return break continue throw))
      ((try? statement) (M_try (blocks statement) state return break continue throw))
      ((try-finally? statement) (M_try-finally (blocks statement) state return break continue throw))
      ((break? statement) (break state))
      ((continue? statement) (continue state))
      ((throw? statement) (throw (value (M_value (cadr statement) state throw)) state))
      (else (error statement "invalid statement"))
      )))
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
(define declare-assignment?
  (lambda (statement)
    (cond
      ((and (eq? 'var (car statement)) (not (null? (caddr statement)))) #t)
      (else #f))))

; Returns true if given a try statement with a finally
(define try-finally?
  (lambda (statement)
    (cond
      ((and (eq? 'try (operator statement)) (eq? (length statement) 4)) #t)
      (else #f))))


; Returns true if given a try statement with no finally
(define try?
  (lambda (statement)
    (cond
      ((and (eq? 'try (operator statement)) ((eq? (length (cadddr statement)) 0))) #t)
        (else #f))))

; checks if we have a begin statement
(define begin?
  (lambda (statement)
    (eq? 'begin (operator statement))))

; checks if we have a break statement
(define break?
  (lambda (statement)
    (eq? 'break (operator statement))))

; checks if we have a continue statement
(define continue?
  (lambda (statement)
    (eq? 'continue (operator statement))))

; checks if we have a throw statement
(define throw?
  (lambda (statement)
    (eq? 'throw (operator statement))))

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
      ((null? (car state)) (error var "variable not declared"))
      ((eq? (car (first-state state)) var) (cadr (first-state state)))
      (else (retrieve-var-state var (later-state state))))))

; declares a varaible and adds it to the state list
(define declare-var-state
  (lambda (var state)
    (cond
      ((been-declared? var state) (error var "variable already declared"))
      ((null? var) null)
      ((null? state) null)
      ((attatch-state (list var '()) state)))))

; assigns a value to a variable
(define assign-var-state
  (lambda (var val state)
    (cond
      ((null? (car state)) (error var "variable not declared"))
      ((eq? (car (first-state state)) var) (list (car state) (cons val (cdadr state))))
      (else (attatch-state (first-state state) (assign-var-state var val (later-state state)))))))

; removes a variable from the state
(define remove-var-state
  (lambda (var state)
    (cond
      ((null? (car state)) (error var "variable not declared"))
      ((eq? (car (first-state state)) var) (later-state state))
      (else (attatch-state (first-state state) (remove-var-state var (later-state state)))))))

;check if variable has been declared
(define been-declared?
  (lambda (var state)
    (cond
      ((null? state) #f)
      ((null? (car state)) #f)
      ((eq? (car (first-state state)) var) #t)
      (else (been-declared? var (later-state state))))))

; check if a variable has been assigned a value
(define been-assigned?
  (lambda (var state)
    (if (been-declared? var state)
        (not (null? (retrieve-var-state var state)))
        (error var "variable not declared"))))

; returns an empty state
(define empty-state
  (lambda ()
    '(()())))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;Layer functions that allow the old state implementation to work
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define state-layer-push
  (lambda (state layer)
    (cons state layer)))

(define state-layer-pop
  (lambda (layer)
    (cdr layer)))

(define state-layer-peek
  (lambda (layer)
    (car layer)))

; Removes the variable and its value from the entire state
(define state-layer-remove
  (lambda (var layer)
    (cond
      ((null? layer) (error var "variable not declared"))
      ((been-declared? var (state-layer-peek layer)) (state-layer-push  (remove-var-state var (state-layer-peek layer)) (state-layer-pop layer)))
      (else (state-layer-push (state-layer-peek layer) (remove-var-state var (state-layer-pop layer)))))))

; Returns the value of var in the state.
(define state-layer-get
  (lambda (var layer)
    (cond
      ((null? layer) (error var "variable not declared"))
      ((been-declared? var (state-layer-peek layer)) (retrieve-var-state var (state-layer-peek layer)))
      (else (state-layer-get var (state-layer-pop layer))))))

; Returns #t if var has a value assigned to it in the current state layer
(define state-layer-assigned?
  (lambda (var layer)
    (cond
      ((null? layer) #f)
      ((been-assigned? var (state-layer-peek layer)) #t)
      (else (state-layer-assigned? var (state-layer-pop layer))))))

; returns #t if var has been declared in the current state layer
(define state-layer-declared?
  (lambda (var layer)
    (cond
      ((null? layer) #f)
      ((been-declared? var (state-layer-peek layer)) #t)
      (else (state-layer-declared? var (state-layer-pop layer))))))

; declares a new variable in the state layer without assigning it a value
(define state-layer-declare
  (lambda (var layer)
    (if (been-declared? var (state-layer-peek layer))
        (error var "variable already declared")
        (state-layer-push (attatch-state (list var '()) (state-layer-peek layer)) (state-layer-pop layer)))))

; assigns a value to a variable already declared in the state layer
(define state-layer-assign
  (lambda (var val layer)
    (cond
      ((null? layer) (error var "variable not declared"))
      ((been-declared? var (state-layer-peek layer)) (state-layer-push (assign-var-state var val (state-layer-peek layer)) (state-layer-pop layer)))
      (else (state-layer-push (state-layer-peek layer) (state-layer-assign var val (state-layer-pop layer)))))))










    
