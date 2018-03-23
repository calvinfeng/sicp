;Procedures as Arguments
(define (pi-sum a b)
  (if (> a b)
    0
    (+ (/ 1.0 (* a (+ a 2))) (pi-sum (+ a 4) b))))

(pi-sum 1 10000)


;Define higher-order function, notice that term is a function and next is an incremental function
(define (sum term a next b)
  (if (> a b)
    0
    (+ (term a) (sum term (next a) next b))))

(define (cube a)
  (* a a a))

(define (incre-by-1 n)
  (+ n 1))

(define (sum-cube-series a b)
  (sum cube a incre-by-1 b))

(sum-cube-series 1 10)

;We can define integral using the summation higher order function we created
;Integral of f is equivalent to (f(a + dx/2) + f(a + dx/2 + dx) + f(a + dx/2 + 2dx) + ...) * dx, assuming dx => 0
(define (integral f a b dx)
  (define (add-dx x) (+ x dx))
  (* (sum f (+ a (/ dx 2.0)) add-dx b) dx))

(integral cube 0 1 0.0001)
