#lang planet neil/sicp

;依赖
(define (square x) (* x x))
(define (average x y) (/ (+ x y) 2))

;1.3章节
;用高阶函数做抽象
(define (cube x) (* x x x))
(define (indentity x) x)

;1.3.1章节
;过程作为参数
;(define (sum-integers a b)
;  (if (> a b) 0 (+ a (sum-integers (+ a 1) b))))
;(define (sum-cubes a b)
;  (if (> a b) 0 (+ (cube a) (sum-cubes (+ a 1) b))))
;求和公共模式
;(define (sum term a next b)
;  (if (> a b)
;      0
;      (+ (term a) (sum term (next a) next b))))
;(define (accumulate combiner null-value term a next b)
;  (if (> a b) null-value
;      (combiner (term a) (accumulate combiner null-value term (next a) next b))))
;(define (sum term a next b)
;  (define (iter a result)
;    (if (> a b) result (iter (next a) (+ (term a) result))))
;  (iter a 0))
(define (accumulate combiner null-value term a next b)
  (define (iter a result)
    (if (> a b) result (iter (next a) (combiner (term a) result))))
  (iter a null-value))
(define (sum term a next b)
  (accumulate + 0 term a next b))

(define (sum-cubes a b)
  (define (inc n) (+ n 1))
  (sum cube a inc b))

(define (sum-integers a b)
  (sum indentity a inc b))

(define (integral f a b dx)
  (define (add-dx x) (+ x dx))
  (* (sum f (+ a (/ dx 2)) add-dx b)
     dx))

;1.3.2章节
;用lambda构造过程
(define (pi-sum a b)
  (sum (lambda (x) (/ 1.0 (* x (+ x 2))))
       a
       (lambda (x) (+ x 4))
       b))
;用let创建局部变量
(define (f x y)
  (let ((a (+ 1 (* x y)))
        (b (- 1 y)))
       (+ (* x (square a))
          (* y b)
          (* a b))))

;1.3.3章节
;过程作为一般性的方法
;通过区间折半寻找方程的根
(define (serach f neg-point pos-point)
  (define (close-enough? a b)
    (< (abs (- a b)) 0.001))
  (let ((mid-point (average neg-point pos-point)))
    (if (close-enough? neg-point pos-point)
        mid-point
        (let ((test-value (f mid-point)))
          (cond ((positive? test-value)
                 (serach f neg-point mid-point))
                ((negative? test-value)
                 (serach f mid-point pos-point))
                (else mid-point))))))

(define (half-interval-method f a b)
  (let ((a-value (f a))
        (b-value (f b)))
    (cond ((and (negative? a-value) (positive? b-value))
           (serach f a b))
          ((and (negative? b-value) (positive? a-value))
           (serach f b a))
          (else
           (error "Values are not opposite sign" a b)))))
;不动点
(define tolerance 0.0001)
(define (fixed-point f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) tolerance))
  (define (try guess)
    (let ((next (f guess)))
      (if (close-enough? guess next)
          next
          (try next))))
  (try first-guess))
;(define (sqrt x)
;  (fixed-point (lambda (y) (average y (/ x y))) 1))


;1.3.4章节
;过程作为返回值
;阻尼
(define (average-damp f)
  (lambda (x) (average x (f x))))
;(define (sqrt x)
;  (fixed-point (average-damp (lambda (y) (/ x y)))) 1)
(define (deriv g)
  (define dx 0.00001)
  (lambda (x) (/ (- (g (+ x dx)) (g x)) dx)))
(define (newton-transform g)
  (lambda (x) (- x (/ (g x) ((deriv g) x)))))
(define (newtom-method g guess)
  (fixed-point (newton-transform g) guess))
(define (sqrt x)
  (newtom-method (lambda (y) (- (square y) x)) 1))

;测试
;(sum-cubes 1 10)
;(sum-integers 1 5)
;(integral cube 0 1 0.0001)
;(* 8 (pi-sum 1 10000))
;(half-interval-method sin 2.0 4.0)
;(half-interval-method (lambda (x) (- (* x x x) (* 2 x) 3)) 1 2)
; (fixed-point cos 1.0)
(sqrt 9)

