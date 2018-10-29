#lang racket

(require racket/control)

;; e is the exception prompt

(struct exception:ok (value))
(struct exception:error (err))

(define-syntax raise
  (syntax-rules ()
    ((raise e t)
     (shift-at e k (exception:error t)))))

(define-syntax handle
  (syntax-rules ()
    ((handle e t h)
     (let ((e (make-continuation-prompt-tag)))
       (match (exception:ok (reset-at e t))
         ((exception:ok v) v)
         ((exception:error err) (h err)))))))

(define (print x) (write x) (newline))

(define (test-1)
  (handle e
          (print 'test-1)
          (lambda (ex)
            (print `(exception ,ex happened)))))

(define (test-2)
  (handle e
          (begin
            (print 'test-2-a)
            (raise e #t)
            (print 'test-2-b))
          (lambda (ex)
            (print `(exception ,ex happened)))))

(define (test-3-a)
  (handle e
          (begin
            (print 'test-3-1-a)
            (handle f
                    (begin
                      (print 'test-3-2-a)
                      (raise f #t)
                      (print 'test-3-2-b))
                    (lambda (ex)
                      (print `(exception level 2 ,ex happened))))
            (print 'test-3-1-b))
          (lambda (ex)
            (print `(exception level 1 ,ex happened)))))

;> (test-3-a)
;test-3-1-a
;test-3-2-a
;test-3-1-b

(define (test-3-b)
  (handle e
          (begin
            (print 'test-3-1-a)
            (handle f
                    (begin
                      (print 'test-3-2-a)
                      (raise e #t)
                      (print 'test-3-2-b))
                    (lambda (ex)
                      (print `(exception level 2 ,ex happened))))
            (print 'test-3-1-b))
          (lambda (ex)
            (print `(exception level 1 ,ex happened)))))
;> (test-3-b)
;test-3-1-a
;test-3-2-a
;#<exception:error>
