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