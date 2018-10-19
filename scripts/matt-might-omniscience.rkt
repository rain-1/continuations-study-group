#lang racket

(require racket/control)

(define (amb lst) (shift k (for-each k lst)))
(define (assert b) (unless b (amb '())))

(let ((a (amb (list 1 2 3 4 5 6 7)))
      (b (amb (list 1 2 3 4 5 6 7)))
      (c (amb (list 1 2 3 4 5 6 7))))
  
  ; We're looking for dimensions of a legal right
  ; triangle using the Pythagorean theorem:
  (assert (= (* c c) (+ (* a a) (* b b))))
  
  ; And, we want the second side to be the shorter one:
  (assert (< b a))

  ; Print out the answer:
  (display (list a b c))
  (newline))

(define (implies p q)
  (or (not p) q))

(define-syntax sat-solve
  (syntax-rules ()
    ((sat-solve (var ...) cond)
     (let ((var (amb '(#f #t))) ...)
       (assert cond)
       (list var ...)))))

(sat-solve (a b c)
           (and (implies a (not b))
                (not a)
                c))

;; (4 3 5)
;; '(#f #f #t)
;; '(#f #t #t)
