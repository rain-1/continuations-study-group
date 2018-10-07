#lang racket

;; http://okmij.org/ftp/papers/DDBinding.pdf
;; https://docs.racket-lang.org/reference/cont.html

(require racket/control)

(define (make-dynamic-variable name)
  (make-continuation-prompt-tag name))

(define-syntax dref
  (syntax-rules ()
    ((dref var)
     (shift-at var c (lambda (y) ((c y) y))))))

(define-syntax dlet
  (syntax-rules (= in)
    ((dlet var = val in body)
     ((reset-at var ((lambda (z) (lambda (y) z)) body))
      val))))

(define-syntax dset!
  (syntax-rules ()
    ((dset! var val)
     (shift-at var c (lambda (y) ((c y) val))))))

(define (t0)
  (let ((p (make-dynamic-variable 'p)))
    (dlet p = 3 in (dref p))))

(define (t1)
  (let ((p (make-dynamic-variable 'p)))
    (dlet p = 1 in
          (dlet p = 2 in
                (dref p)))))

(define (t2-a)
  (let ((p (make-dynamic-variable 'p))
        (q (make-dynamic-variable 'q)))
    (dlet p = 3 in
          (dlet q = 4 in
                ((dlet p = 1 in (lambda (z) (dref p))) 0)))))

(define (t2-b)
  (let ((p (make-dynamic-variable 'p))
        (q (make-dynamic-variable 'q)))
    (dlet p = 3 in
          (dlet q = 4 in
                ((dlet p = 1 in (lambda (z) (dref q))) 0)))))

(define (t3)
  (let ((p (make-dynamic-variable 'p)))
    (dlet p = 0 in
          (let* ((f (lambda () (dref p)))
                 (x (f))
                 (y (dlet p = 1 in (f)))
                 (z (f)))
            (list x y z)))))

(define (t4)
  (let ((p (make-dynamic-variable 'p)))
    (dlet p = 1 in (reset (dref p)))))

(define (t5)
  (let ((p (make-dynamic-variable 'p)))
    (dlet p = 1 in
          (reset
           (dlet p = 2 in
                 (shift c (dref p)))))))

(define (t6)
  (let ((p (make-dynamic-variable 'p))
        (r (make-dynamic-variable 'r)))
    ((lambda (f)
       (dlet p = 2 in
             (dlet r = 20 in
                   (f 0))))
     (dlet p = 1 in
           (reset (dlet r = 10 in
                        ((lambda (x) (+ (dref p) (dref r)))
                         (shift c c))))))))

