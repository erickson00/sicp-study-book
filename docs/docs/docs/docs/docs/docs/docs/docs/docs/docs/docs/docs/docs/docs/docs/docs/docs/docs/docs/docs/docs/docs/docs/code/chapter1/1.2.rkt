#lang planet neil/sicp

; 构造过程的抽象-过程与它产生的计算

; 依赖

; 本章节内容

; 阶乘的递归计算过程
; (define (factorial n)
;  (if (= n 1) 1 (* n (factorial (- n 1)))))
; 阶乘的迭代计算过程
(define (factorial n)
  (define (fact-iter product counter)
    (if (> counter n)
        product
        (fact-iter (* counter product)
                   (+ counter 1))))
  (fact-iter 1 1))

; 斐波那契数列的计算过程
; (define (fib n)
;   (define (fib-iter n)
;     (cond ((= n 0) 0)
;           ((= n 1) 1)
;           (else (+ (fib-iter (- n 1)) (fib-iter (- n 2))))))
;   (fib-iter n))
(define (fib n)
  (define (fib-iter pre prepre count)
    (cond  ((= count 0) prepre)
           (else (fib-iter
                  (+ pre prepre)
                  pre
                  (- count 1)))))
  (fib-iter 1 0 n))

; 换零钱
define (count-change amount)
  (cc amount 5))
(define (cc amount kinds-of-coins)
  (cond ((= amount 0) 1)
        ((or (< amount 0) (= kinds-of-coins 0)) 0)
        (else (+ (cc amount
                     (- kinds-of-coins 1))
                 (cc (- amount
                        (first-denomination kinds-of-coins))
                     kinds-of-coins)))))
(define (first-denomination kinds-of-coins)
  (cond ((= kinds-of-coins 1) 1)
        ((= kinds-of-coins 2) 5)
        ((= kinds-of-coins 3) 10)
        ((= kinds-of-coins 4) 25)
        ((= kinds-of-coins 5) 50)))

; 测试
(factorial 4)
(fib 6)
(count-change 100)

