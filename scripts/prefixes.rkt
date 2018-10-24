#lang racket

(require racket/control)

(define (visit lst)
  (if (null? lst)
      (shift k '())
      (cons (car lst)
            (shift k (cons (k '()) (reset (k (visit (cdr lst)))))))))
(define (prefix lst)
  (reset (visit lst)))

;> (prefix '(1 2 3))
;'((1) (1 2) (1 2 3))

