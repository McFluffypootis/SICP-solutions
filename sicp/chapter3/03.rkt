;
#lang sicp

(#%require rackunit)

(#%require racket/exn)


(define (make-account balance password)

  (define (withdraw amount) 
    (if (>= balance amount)
      (begin (set! balance (- balance amount)) balance)
      (display "inusficcient funds")))

  (define (deposit amount) 
    (set! balance (+ balance amount)))

  (define (dispatch pswrd op)
    (if (eq? pswrd password)
        (cond ((eq? withdraw) (lambda (x) (withdraw x)))
              ((eq? deposit) (lambda (x) (deposit x)))
              (else (error "Unknown request" op)))
          (display "Wrong password!")))
  dispatch)


(define incorrect-message "Incorrect password")


(define insufficient-message "Insufficient funds")


(define acc (make-account 100 'secret-password))


(define (wrong-password-attempt)

  (with-handlers ([exn:fail?

                    (lambda (e) (exn->string e))])

     ((acc 'some-other-password 'deposit) 50)))


(define (insufficient-funds-attempt)

  (with-handlers ([exn:fail?

                    (lambda (e) (exn->string e))])

     ((acc 'secret-password 'withdraw) 5000)))


(check-equal? ((acc 'secret-password 'withdraw) 40) 60)

(check-equal? ((acc 'secret-password 'deposit) 40) 100)

; substring used because checking system has extra output

(check-equal? (substring (wrong-password-attempt) 0 18) incorrect-message)

(check-equal? (insufficient-funds-attempt) insufficient-message)

