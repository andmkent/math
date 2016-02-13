#lang typed/racket/base

(require typed/safe/ops)

(provide tangent-number)

(: tangent-number : Integer -> Natural)
;  The n'th tangent number:
;  <http://mathworld.wolfram.com/TangentNumber.html>
(define (tangent-number n)
  (cond [(n . < . 0)  (raise-argument-error 'tangent-number "Natural" n)]
        [else
         ; Implementation note:
         ;   See "Concrete Mathematics" p 287 for the method
         ;; <refined-local> Refinement added to T for simple vector operations
         (define T : (Refine [T : (Vectorof Natural)] (= (+ n 2) (len T))) (make-vector (+ n 2) 0))
         ; T[x]=x
         (safe-vector-set! T 1 1)
         ;; <refined-local> Refinements added to k & i.
         (for: ([k : (Refine [k : Natural] (<= k n)) (in-range (+ n 1))])
           ; differentiate T[x]
           (for: ([i : (Refine [i : Natural] (<= i k)) (in-range (+ k 1))])
             (safe-vector-set! T i (* (add1 i) (safe-vector-ref T (add1 i)))))
           (safe-vector-set! T k 0)
           ; multiply T[x] with 1+x^2
           (let loop ([i : (Refine [i : Natural] (<= 2 i (+ k 2))) 2])
             (cond
               [(>= i (+ k 2)) (void)]
               [else (safe-vector-set! T i (+ (safe-vector-ref T i) (safe-vector-ref T (- i 2))))
                     (loop (add1 i))])))
         (safe-vector-ref T 0)]))