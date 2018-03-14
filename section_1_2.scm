;;; 1.2 Procedures and the Processes They Generate

;;; Factorial
(define (factorial n)
  (fact-iter 1 1 n))

(define (fact-iter product counter max-count)
  (if (> counter max-count) product
    (fact-iter (* counter product) (+ counter 1) max-count)))

(factorial 5)

;;; In contrasting iteration and recursion, we must be careful not to confuse the notion of a recursive process with the
;;; notion of a recursive procedure. When we describe the procedure as recurisve, we are referring to the syntactic fact
;;; that the procedure definition refers to the procedure itself. But when we describe a process as following a pattern
;;; that is, say, linearly recursive, we are speaking about how the process evolves, not about the syntax of how a
;;; procedure is written. It may seem disturbing that we refer a recursive procedure such as fact-iter as generating an
;;; iterative process. However the process really is iterative: Its state is captured completely by its three state
;;; variables, and an interpreter need keep track of only three variables in order to execute the process.


;;; Tree Recursion
(define (fib n)
  (cond ((= n 0) 0)
        ((= n 1) 1)
        ( else ( + (fib (- n 1)) (fib (- n 2))))))

(fib 10)


;;; Counting change
;;; The number of ways to change the amount a using n kinds of coins equals to the sume of the following:
;;; 1.) the number of ways to change amount a using all BUT the first kind of coin
;;; 2.) the number of ways to change amount a - d using all n kinds of coins, where d is the denomination of the first
;;; kind of coin.
;;;
;;; To see why this is true, the ways to make change can be dvided into two groups; those that do not use any of the
;;; first kind of coin and those that do.
;;;
;;; If a is exactly 0, we should count that as 1 way to make change
;;; If a is less than 0, we should count that as 0 ways to make change
;;; If n is 0, we should count that as 0 ways to make change
(define (count-change amount)
  (cc amount 5))

(define (cc amount kinds-of-coins)
  (cond ((= amount 0) 1)
        ((or (< amount 0) (= kinds-of-coins 0)) 0)
        (else (+ (cc amount (- kinds-of-coins 1)) (cc (- amount (first-denomination kinds-of-coins)) kinds-of-coins)))))

(define (first-denomination kinds-of-coins)
  (cond ((= kinds-of-coins 1) 1)
        ((= kinds-of-coins 2) 5)
        ((= kinds-of-coins 3) 10)
        ((= kinds-of-coins 4) 25)
        ((= kinds-of-coins 5) 50)))

(count-change 100)

;;; Exponentiation
;;; The substitution model reveals a shape of expansion followed by contraction. The expansion occurs as the process
;;; builds up a chain of deferred operations (in this case, a chain of multiplications.) The contraction occurs as the
;;; operations are actually performed. This type of process, characterized by a chain of deferred operations, is called
;;; a recursive process. Carrying out this process requires the interpreter keep track of the operations to be performed
;;; later on.
(define (exp base n)
  (if (= n 0) 1 (* base (exp base (- n 1)))))

(exp 10 4)

;;; By contrast, this latter process does not grow and shrink. At each step, all we need to keep track of, for any n,
;;; are the current values of teh variables base, counter, and product. We call this an iterative process. In general,
;;; an iterative process is one whose state can be summarized by a fixed number of state variables, together with a
;;; a fixed rule that describes how the state variables should be updated as the process moves from state to state and
;;; an optional end test that specifies conditions under which the process should terminate.
(define (exp base n)
  (exp-iter base n 1))

(define (exp-iter base counter product)
  (if (= counter 0) product (exp-iter base (- counter 1) (* base product))))

(exp 10 4)


;;; Greatest Common Divisors
;;; If r is the remainder when a / b, then the common divisors of a and b are precisely the same as the common divisors
;;; of b and r, i.e. gcd(a, b) = gcd(b, r)
;;;
;;; Example:
;;; gcd(206, 40) = gcd(40, 6) = gcd(6, 4) = gcd(4, 2) = gcd(2, 0) => 2
;;; gcd(50, 40) = gcd(40, 10) = gcd(10, 0) => 10
(define (gcd a b)
  (if (= b 0) a (gcd b (remainder a b))))

(gcd 50 40)

;;; isPrime? Searching for divisors
(define (smallest-divisor n)
  (find-divisor n 2))

(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) n)
        ((divides? test-divisor n) test-divisor)
        (else (find-divisor n (+ test-divisor 1)))))

(define (divides? a b)
  (= (remainder a b) 0))

(define (prime? n)
  (= n (smallest-divisor n)))

;;; isPrime? Fermat Test
;;; Fermat's Little Theorem: If n is a prime number and a is any positive integer less than n, then a raised to the nth
;;; power is congruent to a modulo n. That is, given a number n, pick a random number a < n and compute the remainder of
;;; a^n moldule n. if the result is not equal  a, then n is certainly not prime. If it is a, then chances are good that
;;; n is prime.

;;; Given a base, raises it to the power of the given exponent and then perform remainder math on it.
(define (expmod base exp m)
  (cond ((= exp 0) 1)
        ((even? exp) (remainder (square (expmod base (/ exp 2) m)) m))
        (else (remainder (* base (expmod base (- exp 1) m)) m))))

(define (fermat-test n)
  (define (try-it a)
    (= (expmod a n n) a)
    (try-it (+ 1 (random (- n 1))))))
