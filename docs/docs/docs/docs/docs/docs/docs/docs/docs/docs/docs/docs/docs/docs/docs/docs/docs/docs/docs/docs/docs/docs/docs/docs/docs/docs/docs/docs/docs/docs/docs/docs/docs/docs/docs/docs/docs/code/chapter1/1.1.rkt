#lang planet neil/sicp

; 构造过程的抽象-程序设计的基本元素

; 依赖
(define (square x) (* x x))
(define (abs x)
  (if (< x 0) (- x) x))
(define (average x y) (/ (+ x y) 2))

; 本章节内容
(define (sum-of-squares x y) (+ (square x) (square y)))
(define (f a)
  (sum-of-squares (+ a 1) (* a 2)))

(define (sqrt x)
  (define (good-enough? guess)
    (< (abs (- (square guess) x)) 0.001))
  (define (improve  guess)
    (average guess (/ x guess)))
  (define (sqrt-iter guess)
    (if (good-enough? guess)
        guess
        (sqrt-iter (improve guess))))
  (sqrt-iter 1))

; 测试
(f 5)
(sqrt 2)

