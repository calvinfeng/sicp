;;; 1.1 The Elements of Programming

;;; Square Roots by Netwon's Method
(define (sqrt-step guess target)
  (if (good-enough? guess target) guess (sqrt-step (improve guess target) target)))

;;; A guess is improved by averaging it
(define (improve guess target)
  (average guess (/ target guess)))

;;; where as average is defined as follows
(define (average x y)
  (/ (+ x y) 2))

;;; good-enough is simply an approximation check
(define (good-enough? guess target)
  (< (abs (- (square guess) target)) 0.0001))

;;; finally, use 1.0 as the initial guess
(define (sqrt num)
  (sqrt-step 1.0 num))

(sqrt 10)

;;; Exercise 1.8 Cube Roots by Newton Methods
(define (cube-root-step guess target)
  (if (cube-good-enough? guess target) guess (cube-root-step (improve-cube-guess guess target) target)))

(define (improve-cube-guess guess target)
  (/ (+ (/ target (square guess)) (* 2 guess)) 3))


(define (cube-good-enough? guess target)
  (< (abs (- (* (square guess) guess) target)) 0.0001))

(define (cube-root num)
  (cube-root-step 1.0 num))

(cube-root 8)


;;; Internal definitions and block structure - lexical scoping
(define (sqrt x)
  (define (good-enough? guess)
    (< (abs (- (square guess) x)) 0.001))
  (define (improve guess)
    (average guess (/ x guess)))
  (define (sqrt-step guess)
    (if (good-enough? guess) guess (sqrt-step (improve guess))))
  (sqrt-step 1.0))

(sqrt 10)
