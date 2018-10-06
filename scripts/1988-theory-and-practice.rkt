#lang racket

(require racket/control)

(define (leaf? x) (not (pair? x)))

(define (enumerate e t)
  (let l ((t t))
    (if (leaf? t)
        (e t)
        (for-each l t))))

(define (enumerate2 t)
  (let ((e (lambda (l) (control r (cons l (lambda () (prompt (r '()))))))))
    (prompt (enumerate e t))))

(define t1
  (list (list 1 2)
        (list 3 4)))

(define t2
  (list (list (list 1 2) 3)
        (list 4 (list 5 6))))

;> (enumerate print t1)
;1234
;> (enumerate print t2)
;123456

;> (enumerate2 t2)
;'(1 . #<procedure>)
;> ((cdr (enumerate2 t2)))
;'(2 . #<procedure>)

