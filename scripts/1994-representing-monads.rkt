#lang racket

(require racket/control)

(struct monad (ext unit))

(define (reflect mnd m)
  ;; M[a] -> a
  (shift k ((monad-ext mnd) k m)))

(define (reify mnd t)
  ;; (unit -> a) -> M[a]
  ((monad-ext mnd)
   (monad-unit mnd)
   (reset ((monad-unit mnd) (t)))))

(define monad:error
  (monad (lambda (f x)
           (match x
             (`(just ,x) (f x))
             (`(error ,e) `(error ,e))))
         (lambda (x)
           `(just ,x))))

(define (myraise e)
  (reflect monad:error `(error ,e)))

(define (myhandle thunk h)
  (match (reify monad:error thunk)
    (`(just ,x) x)
    (`(error ,e) (h e))))

(define (myshow t)
  (myhandle (lambda () (print (string-append "ok: " (number->string (t)))))
            (lambda (s) (print (string-append "error: " s)))))

(define (t1)
  (myshow (lambda () (+ 1 2))))

(define (t2)
  (myshow (lambda () (+ 1 (myraise "oops")))))

(define monad:list
  (monad (lambda (f x)
           (apply append (map f x)))
         (lambda (x)
           (list x))))

(define (amb x y)
  (reflect monad:list (append (reify monad:list (lambda () x))
                              (reify monad:list (lambda () y)))))

(define (fail) (reflect monad:list '()))

(define (t3)
  (reify monad:list (lambda ()
                      (let ((x (* (amb 3 4) (amb 5 7))))
                        (if (>= x 20)
                            x
                            (fail))))))

