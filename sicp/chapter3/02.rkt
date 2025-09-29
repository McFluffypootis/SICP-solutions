;
#lang sicp

(#%require rackunit)



(define (make-monitored f)
  (let ((calls 0))
  (lambda (m)
    (cond ((eq? m 'how-many-calls?) calls)
          ((eq? m 'reset-count) 
           (begin (set! calls 0) 0))
          (else (begin (set! calls (+ calls 1)) (f m)))))))





(define s (make-monitored sqrt))


(check-equal? (s 100) 10)

(check-equal? (s 25) 5)

(check-equal? (s 'how-many-calls?) 2)

(check-equal? (s 'reset-count) 0)

(check-equal? (s 100) 10)

(check-equal? (s 'how-many-calls?) 1)
