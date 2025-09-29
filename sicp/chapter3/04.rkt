;
#lang sicp

(#%require rackunit)

(#%require racket/exn)



(define (make-account balance password)
  (let ((attempts 0)) (
  
  (define (withdraw amount) 
    (if (>= balance amount)
      (begin (set! balance (- balance amount)) balance)
      (display "inusficcient funds")))

  (define (deposit amount) 
    (set! balance (+ balance amount)))

  (define (dispatch pswrd op)
    (if (cond? pswrd password)
        (cond ((eq? withdraw) (lambda (x) (withdraw x)))
              ((eq? deposit) (lambda (x) (deposit x)))
              (else (error "Unknown request" op)))
        (if (> attempts 7)
              (error "Calling Cops...")
              (begin (set! attempts (+ attempts 1) 
                     (display "Wrong password"))))))
  dispatch))


