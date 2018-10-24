#lang racket

(require racket/control)

;; This is a version of exceptions.scm from [1] modified to use shift/reset instead
;; of call/cc.
;;
;; [1] http://matt.might.net/articles/programming-with-continuations--exceptions-backtracking-search-threads-generators-coroutines/

; exception-stack : list[continuation]
(define exception-stack '())

;; the idea is based on this
;;
;> (reset (let ((e (lambda () (shift h #t)))) (write 1) (e) (write 2)))
;1#t

(define-syntax try
  (syntax-rules (catch)
    ((_ exp ... catch proc)
     (reset
      (let ((cc (lambda (v) (shift k (proc (cadr v))))))
        (dynamic-wind
         (lambda ()
           (set! exception-stack (cons cc exception-stack)))
         (lambda ()
           exp ...)
         (lambda ()
           (set! exception-stack (cdr exception-stack)))))))))

(define (throw exception-value)
  (let ((handler (car exception-stack)))
    (handler (list 'exception exception-value))))

; Example:
(define (test-1)
  (try (try (throw 'foo)
            catch
            (lambda (exn)
              (display "got inner exception: ")
              (display exn)
              (newline)
              (throw 'bar)))
       catch
       (lambda (exn)
         (display "got outer exception: ")
         (display exn)
         (newline))))

;;> (test-1)
;;got inner exception: foo
;;got outer exception: bar
