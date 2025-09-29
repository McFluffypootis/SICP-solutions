;
#lang sicp

(#%require rackunit)

(define (make-accumulate sum)
  (lambda (term) (sequence (set! sum (+ sum term)) sum)))


(define A (make-accumulator 5))


(check-equal? (A 10) 15)

(check-equal? (A 10) 25)

(check-equal? (A -5) 20)

(check-equal? ((make-accumulator 0) 5) 5)


