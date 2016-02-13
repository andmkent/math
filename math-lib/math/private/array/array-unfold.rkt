#lang typed/racket/base

(require racket/fixnum
         typed/safe/ops
         "array-struct.rkt"
         "array-pointwise.rkt"
         "array-fold.rkt"
         "utils.rkt"
         "../unsafe.rkt")

(provide unsafe-array-axis-expand
         array-axis-expand
         list-array->array)

(: check-array-axis (All (A) (Symbol (Array A) Integer -> Index)))
(define (check-array-axis name arr k)
  (define dims (array-dims arr))
  (cond [(fx= dims 0)  (raise-argument-error name "Array with at least one axis" 0 arr k)]
        [(or (k . < . 0) (k . > . dims))
         (raise-argument-error name (format "Index <= ~a" dims) 1 arr k)]
        [else  k]))

(: unsafe-array-axis-expand (All (A B) ((Array A) Index Index (A Index -> B) -> (Array B))))
(define (unsafe-array-axis-expand arr k dk f)
  (define ds (array-shape arr))
  ;; <nope> can't express relation between array-shape and k in the type
  (define new-ds (if (<= k (vector-length ds))
                     (safe-vector-insert ds k dk)
                     (error 'unsafe-array-axis-expand "k > (len ds)")))
  (define proc (unsafe-array-proc arr))
  (unsafe-build-array
   new-ds (λ: ([js : Indexes])
            (cond [(< k (vector-length js))
                   ;; <need dynamic check> Requires a change in the
                   ;; type of unsafe-build array, which requires
                   ;; structs to cooperate.
                   (define jk (safe-vector-ref js k))
                   (f (proc (safe-vector-remove js k)) jk)]
                  [else
                   (error 'unsafe-array-axis-expand "k too small for js")]))))


(: array-axis-expand (All (A B) ((Array A) Integer Integer (A Index -> B) -> (Array B))))
(define (array-axis-expand arr k dk f)
  (let ([k  (check-array-axis 'array-axis-expand arr k)])
    (cond [(not (index? dk))  (raise-argument-error 'array-axis-expand "Index" 2 arr k dk f)]
          [else  (array-default-strict
                  (unsafe-array-axis-expand arr k dk f))])))

;; ===================================================================================================
;; Specific unfolds/expansions

(: list-array->array (All (A) (case-> ((Array (Listof A)) -> (Array A))
                                      ((Array (Listof A)) Integer -> (Array A)))))
(define (list-array->array arr [k 0])
  (define dims (array-dims arr))
  (cond [(and (k . >= . 0) (k . <= . dims))
         (let ([arr  (array-strict (array-map (inst list->vector A) arr))])
           ;(define dks (remove-duplicates (array->list (array-map vector-length arr))))
           (define dk (array-all-min (array-map vector-length arr)))
           (array-default-strict
            (unsafe-array-axis-expand arr k dk (inst unsafe-vector-ref A))))]
        [else
         (raise-argument-error 'list-array->array (format "Index <= ~a" dims) 1 arr k)]))
