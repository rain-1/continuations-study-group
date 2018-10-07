#lang racket

(require racket/control)

;; (1990) Abstracting Control - Olivier Danvy, Andrzej Filinski

(define (substitute var term value)
  ;; capture avoiding substitution
  (cond ((and (symbol? term) (equal? term var))
         value)
        ((symbol? term)
         term)
        ((number? term)
         term)
        ((null? term)
         '())
        ((pair? term)
         (match term
           (`(lambda (,x ,y) ,b) #:when (or (equal? x var)
                                            (equal? y var))
                                 term)
           (`(lambda ,x ,b) #:when (equal? x var)
                            term)
           (`(shift ,x ,b) #:when (equal? x var)
                           term)
           (else
            (cons (substitute var (car term) value)
                  (substitute var (cdr term) value)))))))

(define (cps t)
  ;; this set of equations can be seen as a meta-circular compiler
  ;; from a language with the new control operators into a purely
  ;; functional subset
  (match t
    (x #:when (number? x) x)
    (x #:when (symbol? x) x)
    (`(@ ,e1 ,e2)
     (let ((t (gensym "t")))
       (shift k
              `(,(cps e1)
                ,(cps e2)
                (lambda (,t) ,(k t))))))
    (`(if ,e1 ,e2 ,e3)
     (shift k
            `(if ,(cps e1)
                 ,(reset (k (cps e2)))
                 ,(reset (k (cps e3))))))
    (`(lambda ,x ,e)
     (let ((k (gensym "k")))
       `(lambda (,x ,k)
          ,(reset `(,k ,(cps e))))))
    (`(shift ,k ,e)
     (let ((a (gensym "a"))
           (h (gensym "h")))
       (shift k^
              (substitute k
                          (reset (cps e))
                          `(lambda (,a ,h)
                             (,h ,(k^ a)))))))
    (`(reset ,e) (reset (cps e)))
    (else (error "unknown object" t))))

;; Note:
;; In the source language
;; - every function takes 1 argument
;; - applications are done with @
;; In the target language
;; - every function takes 2: 1 arg + 1 continuation
;; - continuations take one argument
;; - applications are done without @

(define (t1)
  (cps '(@ (@ cons 1) 2))
  ;(cons 1 (lambda (t43311) (t43311 2 (lambda (t43310) t43310))))
  )

(define (t2)
  (cps '(lambda x x))
  ;(lambda (x k43496) (k43496 x))
  )

(define (t3)
  (cps '(lambda x (lambda y y)))
  ;(lambda (x k43999) (k43999 (lambda (y k44000) (k44000 y))))
  )

(define (t4)
  (cps '(lambda x (lambda y x)))
  ;(lambda (x k44122) (k44122 (lambda (y k44123) (k44123 x))))
  )

(define (t5)
  (cps '(lambda x (lambda y (@ x y))))
  ;(lambda (x k44281)
  ;  (k44281 (lambda (y k44282) (x y (lambda (t44283) (k44282 t44283))))))
  )

(define (t6)
  (cps '(if (@ a a) (@ b b) (@ c c)))
  ;(a
  ; a
  ; (lambda (t44515)
  ;   (if t44515 (b b (lambda (t44516) t44516)) (c c (lambda (t44517) t44517)))))
  )

(define (t7)
  (cps '(shift c 1))
  ;1
  )

(define (t8)
  (cps '(shift c (@ c 1)))
  ;((lambda (a44788 h44789) (h44789 a44788)) 1 (lambda (t44790) t44790))
  )

(define (t9)
  (cps '(@ (@ + 1)
           (reset (@ (@ + 10)
                     (shift c (@ c (@ c 100))))))))

(define (t9b)
  (define +k (lambda (x k)
               (k (lambda (y h)
                    (h (+ x y))))))
  (+k
   1
   (lambda (t45190)
     (t45190
      (+k
       10
       (lambda (t45192)
         ((lambda (a45193 h45194) (h45194 (t45192 a45193 (lambda (t45191) t45191))))
          100
          (lambda (t45196)
            ((lambda (a45193 h45194) (h45194 (t45192 a45193 (lambda (t45191) t45191))))
             t45196
             (lambda (t45195) t45195))))))
      (lambda (t45189) t45189)))))
