#lang racket

;; A Syntactic Theory of Dynamic Binding - Luc Moreau
;; https://link.springer.com/content/pdf/10.1007%2FBFb0030637.pdf

;; So, when one implements a delimited control operator like call-with-prompt, one needs to make
;; two decisions. Firstly, does the handler run within or outside the prompt? Having the handler
;; run within the prompt allows an abort inside the handler to return to the same prompt handler,
;; which is often useful. However it prevents tail calls from the handler, so it is less general.
;; from the guile manual: https://www.gnu.org/software/guile/manual/html_node/Shift-and-Reset.html#Shift-and-Reset

(require racket/control)

(define x_ed (make-parameter 'x_ed))

; NOTE: (abort ..) is the same as (shift k ..) where you don't use k
; NOTE: in (handle f M) we call f the handler and M the try block

; v1
;(define-syntax handle
;  (syntax-rules ()
;    ((handle f M)
;     (parameterize ((x_ed (lambda (v) (abort (f v))))) M))))
; v2
;(define-syntax handle
;  (syntax-rules ()
;    ((handle f M)
;     (reset (parameterize ((x_ed (lambda (v) (abort (f v))))) M)))))
; v3
;(define-syntax handle
;  (syntax-rules ()
;    ((handle f M)
;     (parameterize ((x_ed (lambda (v) (abort (f v))))) (reset M)))))
; v4
;(define-syntax handle
;  (syntax-rules ()
;    ((handle f M)
;     (let ((old-x_ed (x_ed)))
;       (parameterize ((x_ed (lambda (v)
;                              (abort (parameterize ((x_ed old-x_ed))
;                                       (f v))))))
;         (reset M))))))
; v5 - thanks to Ryan Culpepper https://stackoverflow.com/a/52958419/7954294
;(define-syntax handle
;  (syntax-rules ()
;    ((handle f M)
;     (let ((old-x_ed (x_ed)))
;       (parameterize ((x_ed (lambda (v)
;                              (control0 k (parameterize ((x_ed old-x_ed))
;                                            (f v))))))
;         (prompt0 M))))))
; v6
(define-syntax handle
  (syntax-rules ()
    ((handle f M)
     (prompt0
      (parameterize ((x_ed (lambda (v)
                             (control0 k (f v)))))
        M)))))
(define-syntax raise
  (syntax-rules ()
    ((raise v) ((x_ed) v))))

(define (print x) (write x) (newline))

(define (test-1)
  (print "level-1 open")
  (handle (lambda (v)
            (print "level-1 caught"))
          (begin
            (print "level-2 open")
            (handle (lambda (v)
                      (print "level-2 caught"))
                    (begin
                      (print "level-3 open")
                      (raise #t)
                      (print "level-3 close")))
            (print "level-2 close")))
  (print "level-1 close"))

(define (test-2)
  (print "level-1 open")
  (handle (lambda (v)
            (print "level-1 caught"))
          (begin
            (print "level-2 open")
            (handle (lambda (v)
                      (print "level-2 caught")
                      (raise #t))
                    (begin
                      (print "level-3 open")
                      (raise #t)
                      (print "level-3 close")))
            (print "level-2 close")))
  (print "level-1 close"))

(define (test-3)
  ;; this runs out of memory because it keeps installing handlers
  (handle (lambda (v)
            'err)
          (test-3)))

;v1
;> (test-1)
;"level-1 open"
;"level-2 open"
;"level-3 open"
;"level-2 caught"

;v2 and v3
;> (test-1)
;"level-1 open"
;"level-2 open"
;"level-3 open"
;"level-2 caught"
;"level-2 close"
;"level-1 close"

;v2 and v3
;> (test-2)
;...
;"level-2 caught"
;"level-2 caught"
; infinite loop

;v4
;> (test-2)
;"level-1 open"
;"level-2 open"
;"level-3 open"
;"level-2 caught"
;"level-1 caught"
;"level-2 close" <--- we don't want this to happen
;"level-1 close"

;v5 v6
;> (test-2)
;"level-1 open"
;"level-2 open"
;"level-3 open"
;"level-2 caught"
;"level-1 caught"
;"level-1 close"
