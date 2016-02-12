#lang typed/racket

(require typed/safe/ops
         racket/fixnum
         "array-struct.rkt"
         "../unsafe.rkt"
         "utils.rkt")

(provide array-broadcasting
         array-broadcast
         array-shape-broadcast)

(: array-broadcasting (Parameterof (U #f #t 'permissive)))
(define array-broadcasting (make-parameter #t))

(: shift-stretch-axes (All (A) ((Array A) Indexes -> (Array A))))
(define (shift-stretch-axes arr new-ds)
  (define old-ds (array-shape arr))
  (define old-dims (vector-length old-ds))
  (define new-dims (vector-length new-ds))
  (define shift : (Refine [i : Index] (= i (+ (len new-ds)
                                              (* -1 (len old-ds)))))
    (let ([shift  (- new-dims old-dims)])
      (cond [(index? shift)  shift]
            [else  (error 'array-broadcast
                          "cannot broadcast to a lower-dimensional shape; given ~e and ~e"
                          arr new-ds)])))
  (define old-js (make-thread-local-indexes old-dims))
  (define old-f (unsafe-array-proc arr))
  (safe-build-array
   new-ds
   (λ: ([new-js : (Refine [v : Indexes] (= (len v) (len new-ds)))])
     (let ([old-js  (old-js)])
       (let: loop : A ([k : Nonnegative-Fixnum  0])
         (cond [(k . < . old-dims)
                ; <changed> don't have type information about new-js, specifically its length.
                (define new-jk (safe-vector-ref new-js (+ k shift)))
                (define old-dk (safe-vector-ref old-ds k))
                (define old-jk (unsafe-fxmodulo new-jk old-dk))
                ; <changed> old-js defined as a thunk, so its length is obscured.
                (safe-vector-set! old-js k old-jk)
                (loop (+ k 1))]
               [else  (old-f old-js)]))))))

(: array-broadcast (All (A) ((Array A) Indexes -> (Array A))))
(define (array-broadcast arr ds)
  (cond [(equal? ds (array-shape arr))  arr]
        [else  (define new-arr (shift-stretch-axes arr ds))
               (if (or (array-strict? arr) ((array-size new-arr) . fx<= . (array-size arr)))
                   new-arr
                   (array-default-strict new-arr))]))

(: shape-insert-axes (~> ([ds : Indexes]
                          [n : Integer])
                         (Refine [out : Indexes]
                                 (= (len out) (+ (len ds) n)))))
(define (shape-insert-axes ds n)
  (: v Indexes)
  (define v (vector-append ((inst make-vector Index) n 1) ds))
  (if (= (vector-length v) (+ (vector-length ds) n))
      v
      (error 'append-length-error)))

; <refined-local> shape-persmissive-broadcast is a local function, so adding refinements has no global side-effects.
; Refinements are added both in the definition for shape-permissive-broadcase and for the newly made new-ds.
(: shape-permissive-broadcast (~> ([ds1 : Indexes]
                                   [ds2 : Indexes]
                                   [dims : (Refine [dims : Index]
                                                   (= dims (len ds1))
                                                   (= dims (len ds2)))]
                                   [fail : (-> Nothing)])
                                  Indexes))
(define (shape-permissive-broadcast ds1 ds2 dims fail)
  (define new-ds : (Refine [new-ds : Indexes] (= dims (len new-ds))) (make-vector dims 0))
  (let loop ([#{k : Nonnegative-Fixnum} 0])
    (cond [(k . < . dims)
           (define dk1 (safe-vector-ref ds1 k))
           (define dk2 (safe-vector-ref ds2 k))
           (safe-vector-set!
            new-ds k
            (cond [(or (= dk1 0) (= dk2 0))  (fail)]
                  [else  (fxmax dk1 dk2)]))
           (loop (+ k 1))]
          [else  new-ds])))

; <refined-local> shape-normal-broadcast is a local function, so adding refinements has no global side-effects.
; Refinements are added both in the definition for shape-normal-broadcase and for the newly made new-ds.
(: shape-normal-broadcast (~> ([ds1 : Indexes]
                               [ds2 : Indexes]
                               [dims : (Refine [dims : Index]
                                               (= dims (len ds1))
                                               (= dims (len ds2)))]
                               [fail : (-> Nothing)])
                              Indexes))
(define (shape-normal-broadcast ds1 ds2 dims fail)
  (define new-ds : (Refine [new-ds : Indexes] (= dims (len new-ds))) (make-vector dims 0))
  (let loop ([#{k : Nonnegative-Fixnum} 0])
    (cond [(k . < . dims)
           (define dk1 (safe-vector-ref ds1 k))
           (define dk2 (safe-vector-ref ds2 k))
           (safe-vector-set!
            new-ds k
            (cond [(= dk1 dk2)  dk1]
                  [(and (= dk1 1) (dk2 . > . 0))  dk2]
                  [(and (= dk2 1) (dk1 . > . 0))  dk1]
                  [else  (fail)]))
           (loop (+ k 1))]
          [else  new-ds])))

(: shape-broadcast2 (Indexes Indexes (-> Nothing) (U #f #t 'permissive) -> Indexes))
(define (shape-broadcast2 ds1 ds2 fail broadcasting)
  (cond [(equal? ds1 ds2)  ds1]
        [(not broadcasting)  (fail)]
        [else
         (define dims1 (vector-length ds1))
         (define dims2 (vector-length ds2))
         (define n (- dims2 dims1))
         (cond [(n . > . 0)
                (let ([ds1 : (Refine [ds1 : Indexes] (= (len ds1) (len ds2)))
                           (shape-insert-axes ds1 n)])
                  (if (eq? broadcasting 'permissive)
                      (shape-permissive-broadcast ds1 ds2 dims2 fail)
                      (shape-normal-broadcast ds1 ds2 dims2 fail)))]
               [(n . < . 0)
                (let ([ds2 : (Refine [ds2 : Indexes] (= (len ds1) (len ds2)))
                           (shape-insert-axes ds2 (- n))])
                  (if (eq? broadcasting 'permissive)
                      (shape-permissive-broadcast ds1 ds2 dims1 fail)
                      (shape-normal-broadcast ds1 ds2 dims1 fail)))]
               [else
                (if (eq? broadcasting 'permissive)
                    (shape-permissive-broadcast ds1 ds2 dims1 fail)
                    (shape-normal-broadcast ds1 ds2 dims1 fail))])]))

(: array-shape-broadcast (case-> ((Listof Indexes) -> Indexes)
                                 ((Listof Indexes) (U #f #t 'permissive) -> Indexes)))
(define (array-shape-broadcast dss [broadcasting (array-broadcasting)])
  (define (fail) (error 'array-shape-broadcast
                        "incompatible array shapes (array-broadcasting ~v): ~a"
                        broadcasting
                        (string-join (map (λ (ds) (format "~e" ds)) dss) ", ")))
  (cond [(empty? dss)  #()]
        [else  (for/fold ([new-ds  (first dss)]) ([ds  (in-list (rest dss))])
                 (shape-broadcast2 new-ds ds fail broadcasting))]))
