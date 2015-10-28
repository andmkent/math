#lang typed/racket/base

(require (prefix-in : racket/unsafe/ops)
         racket/flonum
         racket/fixnum)

(provide (all-defined-out))

(define-syntax-rule (unsafe-vector-ref . args)
  (vector-ref . args))
(define-syntax-rule (unsafe-vector-set! . args)
  (vector-set! . args))
(define-syntax-rule (unsafe-vector-length . args)
  (vector-length . args))

(define-syntax-rule (unsafe-flvector-ref . args)
  (flvector-ref . args))
(define-syntax-rule (unsafe-flvector-set! . args)
  (flvector-set! . args))

(define-syntax-rule (unsafe-fx* . args)
  (fx* . args))
(define-syntax-rule (unsafe-fx+ . args)
  (fx+ . args))
(define-syntax-rule (unsafe-fx- . args)
  (fx- . args))
(define-syntax-rule (unsafe-fxmodulo . args)
  (fxmodulo . args))
(define-syntax-rule (unsafe-fxquotient . args)
  (fxquotient . args))
(define-syntax-rule (unsafe-fx>= . args)
  (fx>= . args))
(define-syntax-rule (unsafe-fx= . args)
  (fx= . args))
(define-syntax-rule (unsafe-fx< . args)
  (fx< . args))

(define-syntax-rule (unsafe-car . args)
  (car . args))
(define-syntax-rule (unsafe-cdr . args)
  (cdr . args))

#|
(define unsafe-vector-ref :unsafe-vector-ref)
(define unsafe-vector-set! :unsafe-vector-set!)
(define unsafe-vector-length :unsafe-vector-length)

(define unsafe-flvector-ref :unsafe-flvector-ref)
(define unsafe-flvector-set! :unsafe-flvector-set!)

(define unsafe-fx* :unsafe-fx*)
(define unsafe-fx+ :unsafe-fx+)
(define unsafe-fxmodulo :unsafe-fxmodulo)
(define unsafe-fx>= unsafe-fx>=)

(define unsafe-car :unsafe-car)
(define unsafe-cdr :unsafe-cdr)
|#
