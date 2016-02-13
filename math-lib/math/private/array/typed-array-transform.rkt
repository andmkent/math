#lang typed/racket/base

(require typed/safe/ops
         racket/vector
         "../unsafe.rkt"
         "array-struct.rkt"
         "array-broadcast.rkt"
         "utils.rkt")

(provide (all-defined-out))

;; ===================================================================================================
;; Arbitrary transforms

(: array-transform (All (A) ((Array A) In-Indexes (Indexes -> In-Indexes) -> (Array A))))
(define (array-transform arr new-ds idx-fun)
  (define old-ds (array-shape arr))
  (define old-f (unsafe-array-proc arr))
  (build-array
   new-ds (λ: ([js : Indexes])
            (old-f (check-array-indexes 'array-transform old-ds (idx-fun js))))))

(: unsafe-array-transform (All (A) ((Array A) Indexes (Indexes -> Indexes) -> (Array A))))
(define (unsafe-array-transform arr new-ds idx-fun)
  (define old-f (unsafe-array-proc arr))
  (unsafe-build-array new-ds (λ: ([js : Indexes]) (old-f (idx-fun js)))))

;; ===================================================================================================
;; Back permutation and swap

(: array-axis-permute (All (A) ((Array A) (Listof Integer) -> (Array A))))
(define (array-axis-permute arr perm)
  (define ds (array-shape arr))
  (let-values ([(ds perm) (apply-permutation
                           perm ds (λ () (raise-argument-error 'array-axis-permute "permutation"
                                                               1 arr perm)))])
    (define dims (vector-length ds))
    (define old-js (make-thread-local-indexes dims))
    (array-default-strict
     (unsafe-array-transform
      arr ds
      (λ: ([js : Indexes])
        (let ([old-js  (old-js)])
          (let: loop : Indexes ([i : Nonnegative-Fixnum  0])
            ; <nope> set! requires us to know more about ref
            (cond [(i . < . dims)  (unsafe-vector-set! old-js
                  ; <nope> ref requires a change to the type of the input function of unsafe-array-transform,
                  ; which requres nested function types to reason about outer functions, as follows:
                  ; (: unsafe-array-transform
                       ;(All (A) (~> ([arr : (Array A)]
                  ;       [new-ds : Indexes]
                   ;      [idx-fun : (~>
                    ;    ([js : (Refine [js : Indexes] (= (len js) (len ds)))])
                     ;        Indexes)])
                      ;       (Array A))))
                  (unsafe-vector-ref perm i)
                  (unsafe-vector-ref js i))
                                   (loop (+ i 1))]
                  [else  old-js]))))))))

(: array-axis-swap (All (A) ((Array A) Integer Integer -> (Array A))))
(define (array-axis-swap arr i0 i1)
  (define ds (array-shape arr))
  (define dims (vector-length ds))
  (cond [(or (i0 . < . 0) (i0 . >= . dims))
         (raise-argument-error 'array-transpose (format "Index < ~a" dims) 1 arr i0 i1)]
        [(or (i1 . < . 0) (i1 . >= . dims))
         (raise-argument-error 'array-transpose (format "Index < ~a" dims) 2 arr i0 i1)]
        [(= i0 i1)  arr]
        [else
         (define new-ds (vector-copy-all ds))
         (define j0 (safe-vector-ref new-ds i0))
         (define j1 (safe-vector-ref new-ds i1))
         (safe-vector-set! new-ds i0 j1)
         (safe-vector-set! new-ds i1 j0)
         (define proc (unsafe-array-proc arr))
         (array-default-strict
          ; <changed>
          (safe-build-array
           new-ds (λ: ([js : (Refine [v : Indexes] (= (len v) (len new-ds)))])
                    (define j0 (safe-vector-ref js i0))
                    (define j1 (safe-vector-ref js i1))
                    (safe-vector-set! js i0 j1)
                    (safe-vector-set! js i1 j0)
                    (define v (proc js))
                    (safe-vector-set! js i0 j0)
                    (safe-vector-set! js i1 j1)
                    v)))]))

;; ===================================================================================================
;; Adding/removing axes

(: array-axis-insert (All (A) (case-> ((Array A) Integer -> (Array A))
                                      ((Array A) Integer Integer -> (Array A)))))
(define (array-axis-insert arr k [dk 1])
  (define ds (array-shape arr))
  (define dims (vector-length ds))
  (cond [(or (k . < . 0) (k . > . dims))
         (raise-argument-error 'array-axis-insert (format "Index <= ~a" dims) 1 arr k dk)]
        [(not (index? dk))
         (raise-argument-error 'array-axis-insert "Index" 2 arr k dk)]
        [else
         (define new-ds (safe-vector-insert ds k dk))
         (define proc (unsafe-array-proc arr))
         (array-default-strict
          ;;<changed>
          (safe-build-array
           new-ds
           (λ: ([js : (Refine [v : Indexes] (= (len v) (len new-ds)))])
             (proc (safe-vector-remove js k)))))]))

(: array-axis-ref (All (A) ((Array A) Integer Integer -> (Array A))))
(define (array-axis-ref arr k jk)
  (define ds (array-shape arr))
  (define dims (vector-length ds))
  (cond [(or (k . < . 0) (k . >= . dims))
         (raise-argument-error 'array-axis-ref (format "Index < ~a" dims) 1 arr k jk)]
        [(or (jk . < . 0) (jk . >= . (safe-vector-ref ds k)))
         (raise-argument-error 'array-axis-ref (format "Index < ~a" (safe-vector-ref ds k))
                           2 arr k jk)]
        [else
         (define new-ds (safe-vector-remove ds k))
         (define proc (unsafe-array-proc arr))
         (array-default-strict
          (safe-build-array
           new-ds
           (λ: ([js : (Refine [v : Indexes] (= (len v) (len new-ds)))])
             (proc ((inst safe-vector-insert Index) js k jk)))))]))

;; ===================================================================================================
;; Reshape

(: array-reshape (All (A) ((Array A) In-Indexes -> (Array A))))
(define (array-reshape arr ds)
  (let ([ds  (check-array-shape
              ds (λ () (raise-argument-error 'array-reshape "(Vectorof Index)" 1 arr ds)))])
    (define size (array-size arr))
    (unless (= size (array-shape-size ds))
      (raise-argument-error 'array-reshape (format "(Vectorof Index) with product ~a" size) 1 arr ds))
    (define old-ds (array-shape arr))
    (cond [(equal? ds old-ds)  arr]
          [else
           (define old-dims (vector-length old-ds))
           (define g (unsafe-array-proc arr))
           (define old-js (make-thread-local-indexes old-dims))
           (array-default-strict
            ;; <changed>
            (safe-build-array
             ds (λ: ([js : (Refine [v : Indexes] (= (len v) (len ds)))])
                  (let ([old-js  (old-js)])
                    (define j (safe-array-index->value-index ds js))
                    (safe-value-index->array-index! old-ds j old-js)
                    (g old-js)))))])))

(: array-flatten (All (A) ((Array A) -> (Array A))))
(define (array-flatten arr)
  (define size (array-size arr))
  (define ds ((inst make-vector Index) 1 size))
  (define old-ds (array-shape arr))
  (cond [(equal? ds old-ds)  arr]
        [else
         (define old-dims (vector-length old-ds))
         (define g (unsafe-array-proc arr))
         (define old-js (make-thread-local-indexes old-dims))
         (array-default-strict
          ; <changed> 
          (safe-build-array
           ds (λ: ([js : (Refine [v : Indexes] (= (len v) (len ds)))])
                (let ([old-js  (old-js)])
                  (define j (safe-vector-ref js 0))
                  (safe-value-index->array-index! old-ds j old-js)
                  (g old-js)))))]))

;; ===================================================================================================
;; Append

(: array-broadcast-for-append (All (A) ((Listof (Array A))
                                        Integer -> (Values (Listof (Array A))
                                                           (Listof Index)))))
(define (array-broadcast-for-append arrs k)
  (define dss (map (λ: ([arr : (Array A)]) (array-shape arr)) arrs))
  (define dims (apply max (map vector-length dss)))
  (cond [(not (index? dims))  (error 'array-broadcast-for-append "can't happen")]
        [(or (k . < . 0) (k . >= . dims))
         (raise-argument-error 'array-append* (format "Index < ~a" dims) k)]
        [else
         (let* ([dss  (map (λ: ([ds : Indexes]) : (Refine [v : (Vectorof Index)] (= dims (len v)))
                             (define dms (vector-length ds))
                             ;; added some intermediate variables and annotations to help the type checker, but no code change
                             (define start (ann ((inst make-vector Index) (- dims dms) 1)
                                                 (Refine [v : Indexes] (= (+ dims (* -1 dms)) (len v)))))
                             (define end (ann ds (Refine [v : (Vectorof Index)] (= dms (len v)))))
                             (vector-append start end))
                           dss)]
                [dks  (map (λ: ([ds : (Refine [v : (Vectorof Index)] (= dims (len v)))]) (safe-vector-ref ds k)) dss)]
                [dss  (map (λ: ([ds : (Refine [v : (Vectorof Index)] (= dims (len v)))]) (safe-vector-remove ds k)) dss)]
                [ds   (array-shape-broadcast dss)]
                ;; <nope> Need a stronger type for `array-shape-broadcast` to make this have any hope
                [dss  (map (λ: ([dk : Index]) (unsafe-vector-insert ds k dk)) dks)])
           (define new-arrs
             (map (λ: ([arr : (Array A)] [ds : Indexes]) (array-broadcast arr ds)) arrs dss))
           (values new-arrs dks))]))

(: array-append* (All (A) (case-> ((Listof (Array A)) -> (Array A))
                                  ((Listof (Array A)) Integer -> (Array A)))))
(define (array-append* arrs [k 0])
  (when (null? arrs) (raise-argument-error 'array-append* "nonempty (Listof (Array A))" arrs))
  (let-values ([(arrs dks)  (array-broadcast-for-append arrs k)])
    (define new-dk (apply + dks))
    (cond
      [(not (index? new-dk))  (error 'array-append* "resulting axis is too large (not an Index)")]
      [else
       (define dss (map (λ: ([arr : (Array A)]) (array-shape arr)) arrs))
       (define new-ds (vector-copy-all (car dss)))
       (if (< -1 k (vector-length new-ds))
           (let ([k k])
             (safe-vector-set! new-ds k new-dk)
             ;; Make two mappings:
             ;; 1. old-procs : new array index -> old array procedure
             ;; 2. old-jks :   new array index -> old array index
             (define old-procs (make-vector new-dk (unsafe-array-proc (car arrs))))
             (define: old-jks : Indexes (make-vector new-dk 0))
             (let arrs-loop ([arrs arrs] [dks dks] [#{jk : Nonnegative-Fixnum} 0])
               (unless (null? arrs)
                 (define arr (car arrs))
                 (define proc (unsafe-array-proc arr))
                 (define dk (car dks))
                 (let i-loop ([#{i : Nonnegative-Fixnum} 0] [#{jk : Nonnegative-Fixnum} jk])
                   ; <nope> No indication of the size of the vectors in relation to jk
                   (cond [(i . < . dk)  (unsafe-vector-set! old-procs jk proc)
                                        (unsafe-vector-set! old-jks jk i)
                                        (i-loop (+ i 1) (unsafe-fx+ jk 1))]
                         [else  (arrs-loop (cdr arrs) (cdr dks) jk)]))))
             (array-default-strict
              (safe-build-array
               new-ds (λ: ([js : (Refine [v : Indexes] (= (len v) (len new-ds)))])
                        (define jk (safe-vector-ref js k))
                        (safe-vector-set! js k (unsafe-vector-ref old-jks jk))
                        (define v ((unsafe-vector-ref old-procs jk) js))
                        (safe-vector-set! js k jk)
                        v))))
           (error 'unbound-k))])))
