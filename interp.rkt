#lang racket

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;     env0 ext-env lookup是对环境的基本操作

;;空环境
(define env0 '())

;;扩展环境，将v绑定到x，然后返回带有该绑定的新环境
(define ext-env
  (lambda (x v env)
    (cons (cons x v) env)))

;;在环境中查找x，并返回绑定的值
(define lookup
  (lambda (x env)
    (let [(p (assq x env))]
      (cond 
       [(not p) x]
       [else (cdr p)]))))

;;定义闭包
(struct Closure (f env))


;; 解释器的递归定义（接受两个参数，表达式 exp 和环境 env）
;; 共 5 种情况（变量，函数，调用，数字，算术表达式）
(define interp1
  (lambda (exp env)
    (match exp                                          ; 模式匹配 exp 的以下情况（分支）
      [(? symbol? x) (lookup x env)]                    ; 变量
      [(? number? x) x]                                 ; 数字
      [`(lambda (,x) ,e)                                ; 函数lambda
       (Closure exp env)]
      [`(define ,name ,exp)                             ; top-value defination
       (let ([v (interp1 exp env)])
         (set! env0 (ext-env name v env0)))]
      [`(if ,test ,a ,b)                                ;if
       (let ([t (interp1 test env)])
         (if t
             (interp1 a env)
             (interp1 b env)))]
      [`(quote ,x) x]                                   ;quote
      [`(,e1 ,e2)                                       ; 调用
       (let ([v1 (interp1 e1 env)]
             [v2 (interp1 e2 env)])
         (match v1
         	[(Closure `(lambda (,x) ,e) env1)
         	(interp1 e (ext-env x v2 env1))]
        	[else (display "no procedure defined")]))]))]
      [`(,op ,e1 ,e2)                                   ; 算术表达式
       (let ([v1 (interp1 e1 env)]
             [v2 (interp1 e2 env)])
         (match op
           ['> (> v1 v2)]
           ['< (< v1 v2)]
	   ['= (= v1 v2)]
           ['+ (+ v1 v2)]
           ['- (- v1 v2)]
           ['* (* v1 v2)]
           ['/ (/ v1 v2)]
           [else (display "not match for")
                 (display  op)]))])))


;用户界面
(define real
  (lambda ()
    (call/cc 
     (lambda (k)
       (display "REAL=>")
       (let ([exp (read)])
         (if (not (equal? '(exit) exp))
             (display (interp1 exp env0))
             (k "exit")))
       (newline)
     (real)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
