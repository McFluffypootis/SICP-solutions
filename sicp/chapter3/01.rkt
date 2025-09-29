;
#lang sicp

(#%require rackunit)


(define (make-accumulator start)
  (let ((acc start))
    (lambda (amount)
      (set! acc (+ acc amount))
      acc)))

(define A (make-accumulator 5))


(check-equal? (A 10) 15)

(check-equal? (A 10) 25)

(check-equal? (A -5) 20)

(check-equal? ((make-accumulator 0) 5) 5)


