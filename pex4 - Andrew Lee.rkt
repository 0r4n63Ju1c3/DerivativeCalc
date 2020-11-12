#lang racket  ; Select "Determine language from source" in lower left
;------------------------------------------------------------------------------
; CS 330 Fall 2020 Programming Exercise 4 - Symbolic Differentiation
;
; author:  <Andrew Lee>
;
; date:  <November 12, 2020>
;
; documentation: None used besides the class labs 
;------------------------------------------------------------------------------

;------------------------------------------------------------------------------
; (variable? E) - Is E a variable (single symbol)?
;
; PRECONDITIONS: E is a list or atom
; POSTCONDITIONS: returns #t or #f (no side-effects)
; EXAMPLE USE:  (variable? 'x) returns #t
(define (variable? E)
  (cond ((list? E) #f)
        (else #t)))
  

;------------------------------------------------------------------------------
; (same-variable? V1 V2) - Are V1 and V2 same variable?
;
; PRECONDITIONS: V1 and V2 are lists and/or atoms
; POSTCONDITIONS:  returns #t if V1 and V2 are variables
;           and V1 = V2 (in the eqv? sense)
; EXAMPLE USE: (same-variable? 'x 'x) returns #t
(define (same-variable? V1 V2)
  (if (and (variable? V1)(variable? V2)) (eq? V1 V2) #f))


;------------------------------------------------------------------------------
; (sum? E) - Is E a summation function call?
;
; PRECONDITIONS: E is a list or atom
; POSTCONDITIONS: returns #t if E is a list and first element is +
; EXAMPLE USE: (sum? '(+ x 5)) returns #t
(define (sum? E)
  (if (list? E) (equal? (car E) '+) #f))

;------------------------------------------------------------------------------
; (cos? E) - Is E a cos function call?
;
; PRECONDITIONS: E is a list or atom
; POSTCONDITIONS: returns #t if E is a list and first element is cos
; EXAMPLE USE: (cos? '(cos x)) returns #t
(define (cos? E)
  (if (list? E) (equal? (car E) 'cos) #f))

;------------------------------------------------------------------------------
; (log? E) - Is E a log function call?
;
; PRECONDITIONS: E is a list or atom
; POSTCONDITIONS: returns #t if E is a list and first element is log
; EXAMPLE USE: (log? '(log x)) returns #t
(define (log? E)
  (if (list? E) (equal? (car E) 'log) #f))



;------------------------------------------------------------------------------
; (sin? E) - Is E a sin function call?
;
; PRECONDITIONS: E is a list or atom
; POSTCONDITIONS: returns #t if E is a list and first element is sin
; EXAMPLE USE: (sin? '(sin x)) returns #t
(define (sin? E)
  (if (list? E) (equal? (car E) 'sin) #f))


;------------------------------------------------------------------------------
; (exp? E) - Is E a exp function call?
;
; PRECONDITIONS: E is a list or atom
; POSTCONDITIONS: returns #t if E is a list and first element is exp
; EXAMPLE USE: (exp? '(exp x)) returns #t
(define (exp? E)
  (if (list? E) (equal? (car E) 'exp) #f))




;------------------------------------------------------------------------------
; (quotient? E) - Is E a division function call?
;
; PRECONDITIONS: E is a list or atom
; POSTCONDITIONS: returns #t if E is a list and first element is /
; EXAMPLE USE: (quotient? '(/ x 5)) returns #t
(define (quotient? E)
  (if (list? E) (equal? (car E) '/) #f))


;------------------------------------------------------------------------------
; (negative? E) - Is E a negative function call?
;
; PRECONDITIONS: E is a list or atom
; POSTCONDITIONS: returns #t if E is a list and first element is -
; EXAMPLE USE: (negative? '(- x 5)) returns #t
(define (negative? E)
  (if (list? E) (equal? (car E) '-) #f))



;------------------------------------------------------------------------------
; (power? E) - Is E a power function call?
;
; PRECONDITIONS: E is a list or atom
; POSTCONDITIONS: returns #t if E is a list and first element is **
; EXAMPLE USE: (sum? '(+ x 5)) returns #t
(define (power? E)
  (if (list? E) (equal? (car E) '**) #f))



;------------------------------------------------------------------------------
; (addend SUM) - Returns the first operand of the sum
;
; PRECONDITIONS: SUM is a sum list
; POSTCONDITIONS: returns the first operand: number, variable, 
;          or another operation
; EXAMPLE USE: (addend '(+ x 5)) returns 'x
(define (addend SUM)
  (if (sum? SUM) (cadr SUM) #f))

;------------------------------------------------------------------------------
; (negFirst SUM) - Returns the first operand of the sum
;
; PRECONDITIONS: SUM is a sum list
; POSTCONDITIONS: returns the first operand: number, variable, 
;          or another operation
; EXAMPLE USE: (addend '(- x 5)) returns 'x
(define (negFirst SUM)
  (if (negative? SUM) (cadr SUM) #f))

;------------------------------------------------------------------------------
; (negSecond PROD) - returns the second operand of the product
;
; PRECONDITIONS: PROD is a product list
; POSTCONDITIONS: returns the second operand: number, variable,
;          or another operation
; EXAMPLE USE: (multiplicand '(* 2 y)) returns y
(define (negSecond PROD)
  (if (negative? PROD) (caddr PROD) #f))


;------------------------------------------------------------------------------
; getTop - Returns the top operand of the division
;
; PRECONDITIONS: SUM is a sum list
; POSTCONDITIONS: returns the first operand: number, variable, 
;          or another operation
; EXAMPLE USE: (addend '(/ x 5)) returns 'x
(define (getTop SUM)
  (if (quotient? SUM) (cadr SUM) #f))


;------------------------------------------------------------------------------
; getSin - Returns the variable for the sin 
;
; PRECONDITIONS: SUM is a sum list
; POSTCONDITIONS: returns the first operand: number, variable, 
;          or another operation
; EXAMPLE USE: (getSin '(sin x) returns 'x
(define (getSin SUM)
  (if (sin? SUM) (cadr SUM) #f))


;------------------------------------------------------------------------------
; getExp - Returns the variable for the sin 
;
; PRECONDITIONS: SUM is a sum list
; POSTCONDITIONS: returns the first operand: number, variable, 
;          or another operation
; EXAMPLE USE: (getExp '(exp x) returns 'x
(define (getExp SUM)
  (if (exp? SUM) (cadr SUM) #f))

;------------------------------------------------------------------------------
; getLog - Returns the variable for the sin 
;
; PRECONDITIONS: SUM is a sum list
; POSTCONDITIONS: returns the first operand: number, variable, 
;          or another operation
; EXAMPLE USE: (getLog '(log x) returns 'x
(define (getLog SUM)
  (if (log? SUM) (cadr SUM) #f))

;------------------------------------------------------------------------------
; getCos - Returns the variable for the sin 
;
; PRECONDITIONS: SUM is a sum list
; POSTCONDITIONS: returns the first operand: number, variable, 
;          or another operation
; EXAMPLE USE: (getCos '(cos x) returns 'x
(define (getCos SUM)
  (if (cos? SUM) (cadr SUM) #f))



;------------------------------------------------------------------------------
; getBot - Returns the top operand of the division
;
; PRECONDITIONS: SUM is a sum list
; POSTCONDITIONS: returns the first operand: number, variable, 
;          or another operation
; EXAMPLE USE: (getBot '(/ x 5)) returns 5
(define (getBot SUM)
  (if (quotient? SUM) (caddr SUM) #f))


;------------------------------------------------------------------------------
; (augend SUM) - returns the second operand of the sum
;
; PRECONDITIONS: SUM is a sum list
; POSTCONDITIONS: returns the second operand: number, variable,
;          or another operation
; EXAMPLE USE: (augend '(+ x 5)) returns 5
(define (augend SUM)
  (if (sum? SUM) (caddr SUM) #f))



;------------------------------------------------------------------------------
; (=number? EXP NUM) - checks if EXP is a number and equal to NUM
;
; PRECONDITIONS: EXP is an expression list, NUM is a number
; POSTCONDITIONS: #t returned if EXP is a number and equal to NUM,
;         otherwise #f
; EXAMPLE USE: (=number? 5 5) returns #t
(define (=number? EXP NUM)
  (if (number? EXP) (equal? EXP NUM) #f))



;------------------------------------------------------------------------------
; (make-sum A1 A2) - returns a sum list for A1 + A2
;
; PRECONDITIONS: A1 and A2 are valid expressions
; POSTCONDITIONS: list expression (+ A1 A2) or reduced form
;        if either A1 or A2 is zero or both are numbers
; EXAMPLE USE: (make-sum 'x 5) returns (+ x 5)
(define (make-sum A1 A2)
  (cond ((and (number? A1)(equal? A1 0)) A2)
        ((and (number? A2)(equal? A2 0)) A1)
        ((and (number? A1)(number? A2)) (+ A2 A1))
        (else (list '+ A1 A2))))


;------------------------------------------------------------------------------
; (make-negative A1 A2) - returns a sum list for A1 + A2
;
; PRECONDITIONS: A1 and A2 are valid expressions
; POSTCONDITIONS: list expression (+ A1 A2) or reduced form
;        if either A1 or A2 is zero or both are numbers
; EXAMPLE USE: (make-negative 'x 5) returns (- x 5)
(define (make-negative A1 A2)
  (cond ((and (number? A2)(equal? A2 0)) A1)
        ((and (number? A1)(number? A2)) (- A2 A1))
        (else (list '- A1 A2))))

;------------------------------------------------------------------------------
; (make-sin A1) - returns a sin derivative
;
; PRECONDITIONS: A1 and A2 are valid expressions
; POSTCONDITIONS: list expression (+ A1 A2) or reduced form
;        if either A1 or A2 is zero or both are numbers
; EXAMPLE USE: (sin 'x ) returns (cos x)
(define (make-sin A1)
  (if(number? A1) (cos A1) (list 'cos A1)))

;------------------------------------------------------------------------------
; (make-exp A1) - returns a sin derivative
;
; PRECONDITIONS: A1 and A2 are valid expressions
; POSTCONDITIONS: list expression (+ A1 A2) or reduced form
;        if either A1 or A2 is zero or both are numbers
; EXAMPLE USE: (exp 'x ) returns (exp x)
(define (make-exp A1)
  (if(number? A1) (exp A1) (list 'exp A1)))




;------------------------------------------------------------------------------
; (make-cos A1) - returns a sin derivative
;
; PRECONDITIONS: A1 are valid expressions
; POSTCONDITIONS: list expression (+ A1 A2) or reduced form
;        if either A1 or A2 is zero or both are numbers
; EXAMPLE USE: (cos 'x ) returns (sin x)
(define (make-cos A1)
  (if(number? A1) (cos A1) (list 'sin A1)))


;------------------------------------------------------------------------------
; (make-log A1) - returns a sin derivative
;
; PRECONDITIONS: A1 are valid expressions
; POSTCONDITIONS: list expression (A1) or reduced form
;        if either A1 or A2 is zero or both are numbers
; EXAMPLE USE: (sin 'x ) returns (log x)
(define (make-log A1)
  (if(number? A1) (log A1) (list '/ 1 A1)))

;------------------------------------------------------------------------------
; (make-divide A1 A2 A3) - returns a divide list for A1 + A2
;
; PRECONDITIONS: A1 and A2 and A3 are valid expressions
; POSTCONDITIONS: list expression (+ A1 A2 A3) or reduced form
;        if either A1 or A2 or A3 is zero or both are numbers
; EXAMPLE USE: (make-divide 'x 5 5) returns (x-5/5^2)
(define (make-divide A1 A2 A3)
  (cond ((and (number? A1)(equal? A1 0)) 0)
        ((and (number? A2)(equal? A2 0)) 0)
        ((and (number? A2)(equal? A2 1)) (A1))
        ((and (number? A1)(number? A2))  (/ A1 A2))
        (else (list '/ (list '- A1 A2) (list '** A3 2)))))
         



;------------------------------------------------------------------------------
; (product? E) - Is E a product function call?
;
; PRECONDITIONS: E is a list or atom
; POSTCONDITIONS: returns #t if E is a list and first element is *
; EXAMPLE USE: (product? '(* 2 y)) returns #t
(define (product? E)
  (if (list? E) (equal? (car E) '*) #f))




;------------------------------------------------------------------------------
; (multiplier PROD) - Returns the first operand of the product
;
; PRECONDITIONS: PROD is a product list
; POSTCONDITIONS: returns the first operand: number, variable, 
;          or another operation
; EXAMPLE USE: (multiplier '(* 2 y)) returns 2
(define (multiplier PROD)
  (if (product? PROD) (cadr PROD) #f))


;------------------------------------------------------------------------------
; getVar - Returns the first operand of the power
; PRECONDITIONS: PROD is a product list
; POSTCONDITIONS: returns the first operand: number, variable, 
;          or another operation
; EXAMPLE USE: getVar '(** 2 y)) returns 2
(define (getVar PROD)
  (if (power? PROD) (cadr PROD) #f))


; getPower - returns the second operand of the product
;
; PRECONDITIONS: PROD is a product list
; POSTCONDITIONS: returns the second operand: number, variable,
;          or another operation
; EXAMPLE USE: (getPower '(** 2 y)) returns y
(define (getPower PROD)
  (if (power? PROD) (caddr PROD) #f))





;------------------------------------------------------------------------------
; (multiplicand PROD) - returns the second operand of the product
;
; PRECONDITIONS: PROD is a product list
; POSTCONDITIONS: returns the second operand: number, variable,
;          or another operation
; EXAMPLE USE: (multiplicand '(* 2 y)) returns y
(define (multiplicand PROD)
  (if (product? PROD) (caddr PROD) #f))



;------------------------------------------------------------------------------
; (make-product M1 M2) - returns a product list for M1 + M2
;
; PRECONDITIONS: M1 and M2 are valid expressions
; POSTCONDITIONS: list expression (+ M1 M2) or reduced
;              form for M1 or M2 equal to 0, 1, or both numbers
; EXAMPLE USE: (make-product 2 'y) returns (* 2 y)
(define (make-product M1 M2)
  (cond ((or (and (number? M1) (equal? M1 0)) (and (number? M2) (equal? M2 0))) 0)
        ((and (number? M1)(equal? M1 1)) M2)
        ((and (number? M2)(equal? M2 1)) M1)
        ((and (number? M1)(number? M2)) (* M1 M2))
        (else (list '* M1 M2))))


        

;------------------------------------------------------------------------------
; (** BASE EXPONENT) - raises BASE to the EXPONENT
;
; PRECONDITIONS: BASE and EXPONENT are integers
; POSTCONDITIONS: returns BASE raised to EXPONENT
; EXAMPLE USAGE:  (** 2 5) returns 32
(define (** BASE EXPONENT)
  (if(equal? EXPONENT 0) 1 (* BASE (** BASE (- EXPONENT 1)))))



;------------------------------------------------------------------------------
; (make-power M1 M2) - returns a power list for M1^M2
;
; PRECONDITIONS: BASE and EXPONENT are integers
; POSTCONDITIONS: list expression (* M2 (** M1 (- M2 1)) or reduced
;              form for M1 or M2 equal to 0, 1, or both numbers
; EXAMPLE USAGE:  ('x 3) returns (* 3(** x 2))
(define (make-power M1 M2)
  (cond ((and (number? M1) (number? M2)) (expt M1 M2))
        ((and (number? M1) (equal? M1 0)) 0)
        ((and (number? M2) (equal? M2 1)) M1)
        (else (list '** M1 M2)))) 



;** FOR THE POWER RULE, YOU WILL NEED SOME ADDITIONAL HELPER FUNCTIONS **


;****** COMPLETE THE COMMENT BLOCKS AND FUNCTIONS FOR THE FOLLOWING ***********



;------------------------------------------------------------------------------
; (deriv-variable EXP VAR) - returns symbolic derivative of a variable
;
; PRECONDITIONS: EXP and VAR are lists or atoms  
; POSTCONDITIONS: list expression (* M2 (** M1 (- M2 1)) or reduced
;              form for M1 or M2 equal to 0, 1, or both numbers
; EXAMPLE USAGE:  ('5 3) returns (1)

(define (deriv-variable EXP VAR) (if(eq? EXP VAR) 1 0))



;------------------------------------------------------------------------------
; (deriv-sum EXP VAR) - returns the symbolic derivative of a sum
;
; PRECONDITIONS: EXP and VAR are lists or atoms  
; POSTCONDITIONS: list expression (* M2 (** M1 (- M2 1)) or reduced
;              form for M1 or M2 equal to 0, 1, or both numbers
; EXAMPLE USAGE:  ('5 3) returns (0)

(define (deriv-sum EXP VAR)
  (make-sum
   (deriv (addend EXP) VAR)
   (deriv (augend EXP) VAR)))

;------------------------------------------------------------------------------
; (deriv-neg EXP VAR) - returns the symbolic derivative of a subtraction
;
; PRECONDITIONS: EXP and VAR are lists or atoms  
; POSTCONDITIONS: list expression (* M2 (** M1 (- M2 1)) or reduced
;              form for M1 or M2 equal to 0, 1, or both numbers
; EXAMPLE USAGE:  ('5 3) returns (0)

(define (deriv-neg EXP VAR)
  (make-negative
   (deriv (negFirst EXP) VAR)
   (deriv (negSecond EXP) VAR)))



;------------------------------------------------------------------------------
; (deriv-product EXP VAR) - returns the symbolic derivative of a product
;
; PRECONDITIONS: EXP and VAR are lists or atoms  
; POSTCONDITIONS: list expression (* M2 (** M1 (- M2 1)) or reduced
;              form for M1 or M2 equal to 0, 1, or both numbers
; EXAMPLE USAGE:  ('3x 3) returns (3)
(define (deriv-product EXP VAR)
  (make-sum
   (make-product (multiplier EXP)
                 (deriv (multiplicand EXP) VAR))
   (make-product (deriv (multiplier EXP) VAR)
                 (multiplicand EXP))))



;------------------------------------------------------------------------------
; (deriv-power EXP VAR) - returns the symbolic derivative of a power
;
; PRECONDITIONS: EXP and VAR are lists or atoms  
; POSTCONDITIONS: list expression (* M2 (** M1 (- M2 1)) or reduced
;              form for M1 or M2 equal to 0, 1, or both numbers
; EXAMPLE USAGE:  ('** x 3) returns (* 3 (** x 2))
(define (deriv-power EXP VAR)
  (make-product
   (make-product (getPower EXP) (make-power (getVar EXP)(- (getPower EXP) 1)))
   (deriv (getVar EXP) VAR)))




;------------------------------------------------------------------------------
; (deriv-quotient EXP VAR) - returns the symbolic derivate of a quotient
;
; PRECONDITIONS: EXP and VAR are lists or atoms  
; POSTCONDITIONS: list expression (* M2 (** M1 (- M2 1)) or reduced
;              form for M1 or M2 equal to 0, 1, or both numbers
; EXAMPLE USAGE:  ('/ 3 x) returns (/(- 3 1) (** x 2)
(define (deriv-quotient EXP VAR)
  (make-divide
   (make-product (deriv (getTop EXP) VAR) (getBot EXP) )
   (make-product (getTop EXP) (deriv (getBot EXP) VAR))
   (getBot EXP)))
   
        

;------------------------------------------------------------------------------
; (deriv-sin EXP VAR) - returns the symbolic deivative  of sin
;
; PRECONDITIONS: EXP and VAR are lists or atoms  
; POSTCONDITIONS: list expression (* M2 (** M1 (- M2 1)) or reduced
;              form for M1 or M2 equal to 0, 1, or both numbers
; EXAMPLE USAGE:  ('sin x) returns (cos x)
(define (deriv-sin EXP VAR)
  (make-product
   (make-sin (getSin EXP))
   (deriv (getSin EXP) VAR)))



;------------------------------------------------------------------------------
; (deriv-cos EXP VAR) - returns the symbolic derivative of cos
;
; PRECONDITIONS: EXP and VAR are lists or atoms  
; POSTCONDITIONS: list expression (* M2 (** M1 (- M2 1)) or reduced
;              form for M1 or M2 equal to 0, 1, or both numbers
; EXAMPLE USAGE:  ('cos x) returns (- 0(sin x))
(define (deriv-cos EXP VAR)
  (list '- 0 
  (make-product
   (make-cos (getCos EXP))
   (deriv (getCos EXP) VAR))))


;------------------------------------------------------------------------------
;  (deriv-log EXP VAR) - returns the symbolic derivative of log
;
; PRECONDITIONS: EXP and VAR are lists or atoms  
; POSTCONDITIONS: list expression (* M2 (** M1 (- M2 1)) or reduced
;              form for M1 or M2 equal to 0, 1, or both numbers
; EXAMPLE USAGE:  ('log x) returns (* (log x) deriv(x))
(define (deriv-log EXP VAR)
  (make-product
   (make-log (getLog EXP))
   (deriv (getLog EXP) VAR)))


;------------------------------------------------------------------------------
; (deriv-exp EXP VAR) - returns the symbolic derivative of exp
;
; PRECONDITIONS: EXP and VAR are lists or atoms  
; POSTCONDITIONS: list expression (* M2 (** M1 (- M2 1)) or reduced
;              form for M1 or M2 equal to 0, 1, or both numbers
; EXAMPLE USAGE:  ('exp x) returns (* (exp x) deriv(x))
(define (deriv-exp EXP VAR)
  (make-product
   (make-exp (getExp EXP))
   (deriv (getExp EXP) VAR)))


;------------------------------------------------------------------------------
; (deriv EXP VAR) - returns expression list that is the derivative
;          of EXP with respect to VAR
;
; PRECONDITIONS:  EXP is a list representing a linear expression,
;                 VAR is a variable
; POSTCONDITIONS: returns the derivative of EXP with respect to VAR
;          returns an error message if improperly formatted expression
; EXAMPLE USE: (deriv '(+ (* 3 x) (* a x)) 'x) returns (+ 3 a)

(define (deriv exp var)
  (cond ((number? exp) 0)
        ((variable? exp)
         (deriv-variable exp var))
        ((sum? exp)
         (deriv-sum exp var))
        ((negative? exp)
         (deriv-neg exp var))
        ((product? exp)
         (deriv-product exp var))
        ((power? exp)
         (deriv-power exp var))
        ((quotient? exp)
         (deriv-quotient exp var))
        ((sin? exp)
         (deriv-sin exp var))
        ((cos? exp)
         (deriv-cos exp var))
        ((log? exp)
         (deriv-log exp var))
        ((exp? exp)
         (deriv-exp exp var))
        (else
         (error "unknown expression type -- DERIV" exp))))



;------------------------------------------------------------------------------
; (replace VAR VALUE EXP) - replaces variable VAR with value VALUE 
;                           in expression EXP
;
; PRECONDITIONS: VAR is a variable, VALUE is numeric value, 
;                EXP is an expression list
; POSTCONDITIONS: returns the EXP expression with VAR replaced by VALUE
; EXAMPLE USE: (replace 'x 4 '(+ x (* 3 x))) returns (+ 4 (* 3 4))
(define (replace VAR VALUE EXP)
  (cond
    ((empty? EXP) '())
    ((equal? VAR (car EXP))(cons VALUE (replace VAR VALUE (cdr EXP))))
    ((list? (car EXP))
      (cons (replace VAR VALUE (car EXP))
            (replace VAR VALUE (cdr EXP))))
    (else (cons (car EXP) (replace VAR VALUE (cdr EXP))))))




;------------------------------------------------------------------------------
; (infix-sum? EXP) - checks if EXP is an infix sum
;
; PRECONDITIONS: VAR is a variable, VALUE is numeric value, 
;                EXP is an expression list
; POSTCONDITIONS: returns true or false if inflix 
; EXAMPLE USE: (infix-sum? 'x + 3 returns #t

(define (infix-sum? EXP)
  (if (and (= (length EXP) 3) (eq? (cadr EXP) '+)) #t #f))

;------------------------------------------------------------------------------
; (infix-product? EXP) - checks if EXP is an infix product
;
; PRECONDITIONS: VAR is a variable, VALUE is numeric value, 
;                EXP is an expression list
; POSTCONDITIONS: returns true or false if inflix 
; EXAMPLE USE: (infix-product? 'x * 3 returns #t

(define (infix-product? EXP)
  (if (and (= (length EXP) 3) (eq? (cadr EXP) '*)) #t #f))

;------------------------------------------------------------------------------
; (infix-power? EXP) - checks if EXP is an infix power
;
; PRECONDITIONS: VAR is a variable, VALUE is numeric value, 
;                EXP is an expression list
; POSTCONDITIONS: returns true or false if inflix 
; EXAMPLE USE: (infix-power? 'x ** 3 returns #t
(define (infix-power? EXP)
  (if (and (= (length EXP) 3) (eq? (cadr EXP) '**)) #t #f))


;------------------------------------------------------------------------------
; (infix-quotient? EXP) - checks if EXP is an infix quotient
;
; PRECONDITIONS: VAR is a variable, VALUE is numeric value, 
;                EXP is an expression list
; POSTCONDITIONS: returns true or false if inflix 
; EXAMPLE USE: (infix-power? 'x / 3 returns #t
(define (infix-quotient? EXP)
  (if (and (= (length EXP) 3) (eq? (cadr EXP) '/)) #t #f))



;------------------------------------------------------------------------------
; (infix-negative? EXP) - checks if EXP is an infix negative
;
; PRECONDITIONS: VAR is a variable, VALUE is numeric value, 
;                EXP is an expression list
; POSTCONDITIONS: returns true or false if inflix 
; EXAMPLE USE: (infix-power? 'x - 3 returns #t
(define (infix-negative? EXP)
  (if (and (= (length EXP) 3) (eq? (cadr EXP) '-)) #t #f))
  

;------------------------------------------------------------------------------
; (infix-2-prefix EXP) - checks if EXP is an infix power
;
; PRECONDITIONS:  EXP is an expression list
; POSTCONDITIONS: returns the converted expression from infix to prefix 
; EXAMPLE USE: (infix-2-prefix EXP 'x ** 3) returns '(** x 3)
(define (infix-2-prefix EXP)
 (cond
    ((or (null? EXP) (variable? EXP) (number? EXP)) EXP)
    ((and (list? EXP) (= (length EXP) 1)) (infix-2-prefix (car EXP))) ; handles (x) from cos, sin, ...
    ((or (infix-sum? EXP) (infix-product? EXP) )
         (list (cadr EXP) (infix-2-prefix (car EXP)) (infix-2-prefix (caddr EXP))))
    ((or (infix-quotient? EXP)(infix-negative? EXP))
         (list (cadr EXP) (infix-2-prefix (car EXP)) (infix-2-prefix (caddr EXP))))
    ((infix-power? EXP)
         (list (cadr EXP) (infix-2-prefix (car EXP)) (infix-2-prefix (caddr EXP))))
    ((or (sin? EXP) (cos? EXP))
         (list (car EXP) (infix-2-prefix (cadr EXP))))
    ((or (log? EXP) (exp? EXP))
         (list (car EXP) (infix-2-prefix (cadr EXP))))
    (else
         (error "Could not convert inflix expression to prefix."))))

;------------------------------------------------------------------------------
; (prefix-2-ingfix EXP) - checks if EXP is an prefix power
;
; PRECONDITIONS:  EXP is an expression list
; POSTCONDITIONS: returns the converted expression from prefix to infix 
; EXAMPLE USE: (prefix-2-infix EXP '** x 3) returns '(x ** 3)
(define (prefix-2-infix EXP)
  (cond 
    ((or (null? EXP) (variable? EXP) (number? EXP)) EXP)
    ((or (sum? EXP) (product? EXP))
         (list (prefix-2-infix (cadr EXP)) (car EXP) (prefix-2-infix (caddr EXP))))
    ((or (sin? EXP) (cos? EXP))
         (list (car EXP) (prefix-2-infix (cadr EXP))))
    ((or (quotient? EXP) (negative? EXP))
          (list (prefix-2-infix (cadr EXP)) (car EXP) (prefix-2-infix (caddr EXP))))
    ((power? EXP)
          (list (prefix-2-infix (cadr EXP)) (car EXP) (prefix-2-infix (caddr EXP))))
    ((or (log? EXP) (exp? EXP))
         (list (car EXP) (prefix-2-infix (cadr EXP))))
    (else
         (error "Could not convert prefix expression to infix."))))



;------------------------------------------------------------------------------
; (derivative EXP VAR) - computes symbolic derivative of infix expression EXP wrt VAR
;                        providing the results in infix format
;
; PRECONDITIONS:  EXP is a list representing a linear expression,
;                 VAR is a variable
; POSTCONDITIONS: returns the derivative of EXP with respect to VAR
;          returns an error message if improperly formatted expression
; EXAMPLE USE: (deriv '( 3 * x) + (a * x)) 'x) returns (3 + a)
(define (derivative EXP VAR)
  (prefix-2-infix (deriv (infix-2-prefix EXP) VAR)))

 
;------------------------------------------------------------------------------
; (derivative eval-derivative EXP VAR VAL) - computes symbolic derivative of infix expression EXP wrt VAR
;                        providing the results in decimal format
;
; PRECONDITIONS:  EXP is a list representing a linear expression,
;                 VAR is a variable
;                 VAL is a value
; POSTCONDITIONS: returns the value of the derivative of EXP with respect to VAR
;          returns an error message if improperly formatted expression
; EXAMPLE USE: (eval-derivative '( (exp(((5 * x) + 3 ) ** 2))/(log(sin(cos x)))) 'x 1) returns (-7.305e029)
(define (eval-derivative EXP VAR VAL)
  (eval (replace VAR VAL (deriv (infix-2-prefix EXP) VAR))))


;------------------------------------------------------------------------------
; (derivative eval-deriv EXP VAR VAL) - computes symbolic derivative of infix expression EXP wrt VAR
;                        providing the results in decimal format
;
; PRECONDITIONS:  EXP is a list representing a linear expression,
;                 VAR is a variable
;                 VAL is a value
; POSTCONDITIONS: returns the value of the derivative of EXP with respect to VAR
;          returns an error message if improperly formatted expression
; EXAMPLE USE: (eval-deriv '(/ (exp (** (+ (* 5 x) 3) 2)) (log (sin (cos x) ) ) ) 'x 1 ) returns (-7.305e029)
(define (eval-deriv EXP VAR VAL)
  (eval (replace VAR VAL (deriv  EXP VAR))))

