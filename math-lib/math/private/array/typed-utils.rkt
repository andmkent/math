#lang typed/racket/base

(require typed/safe/ops
         racket/fixnum
         racket/list
         racket/performance-hint
         (for-syntax racket/base)
         "../unsafe.rkt")

(provide (all-defined-out))

(define-type (Listof* A) (Rec T (U A (Listof T))))
(define-type (Vectorof* A) (Rec T (U A (Vectorof T))))

(define-type Indexes (Vectorof Index))
(define-type In-Indexes (U (Vectorof Integer) Indexes))

(begin-encourage-inline

   ; <refined-local> new-js required a refinement that its length is dims.
  (: vector->supertype-vector
     (All (A B)
          (~> ([v : (Vectorof A)])
              (Refine [res : (Vectorof (U A B))] (= (len v) (len res))))))
  (define (vector->supertype-vector js)
    (define dims (vector-length js))
    (cond [(= dims 0)  (build-vector 0 (λ (x) (error 'impossible)))]
          [else  (define new-js : (Refine [new-js : (Vectorof (U A B))]
                                          (= dims (len new-js)))
                   (make-vector dims (safe-vector-ref js 0)))
                 (let loop : (Refine [new-js : (Vectorof (U A B))]
                                          (= dims (len new-js)))
                   ([#{i : Nonnegative-Fixnum} 1])
                   (cond [(i . < . dims)  (safe-vector-set! new-js i (safe-vector-ref js i))
                                          (loop (+ i 1))]
                         [else  new-js]))]))
  
  (: vector-copy-all
     (All (A) (~> ([v : (Vectorof A)])
                  (Refine [res : (Vectorof A)] (= (len v) (len res))))))
  (define (vector-copy-all js) ((inst vector->supertype-vector A A) js))
  
  (: array-shape-size (Indexes -> Natural))
  (define (array-shape-size ds)
    (define dims (vector-length ds))
    (let loop ([#{i : Nonnegative-Fixnum} 0] [#{n : Natural} 1])
      (cond [(i . < . dims)  (define d (safe-vector-ref ds i))
                             (loop (+ i 1) (* n d))]
            [else  n])))
  
  (: check-array-shape-size (Symbol Indexes -> Index))
  (define (check-array-shape-size name ds)
    (define size (array-shape-size ds))
    (cond [(index? size)  size]
          [else  (error name "array size ~e (for shape ~e) is too large (is not an Index)" size ds)]))

  ; <refined-local> new-ds required a refinement that its length is dims.
  (: check-array-shape (In-Indexes (-> Nothing) -> Indexes))
  (define (check-array-shape ds fail)
    (define dims (vector-length ds))
    (define new-ds : (Refine [new-ds : Indexes]
                             (= dims (len new-ds))) (make-vector dims 0))
    (let loop ([#{i : Nonnegative-Fixnum} 0])
      (cond [(i . < . dims)
             (define di (safe-vector-ref ds i))
             (cond [(index? di)  (safe-vector-set! new-ds i di)
                                 (loop (+ i 1))]
                   [else  (fail)])]
            [else  new-ds])))

  ;; <refined> Safe version of array-index->value-index
  (: safe-array-index->value-index (~> ([ds : Indexes]
                                        [js : (Refine [js : Indexes]
                                                      (<= (len ds) (len js)))])
                                       Nonnegative-Fixnum))
  (define (safe-array-index->value-index ds js)
    (define dims (vector-length ds))
    (let loop ([#{i : Nonnegative-Fixnum} 0] [#{j : Nonnegative-Fixnum} 0])
      (cond [(i . < . dims)
             (define di (safe-vector-ref ds i))
             (define ji (safe-vector-ref js i))
             (loop (+ i 1) (unsafe-fx+ ji (unsafe-fx* di j)))]
            [else  j])))

  (: unsafe-array-index->value-index (Indexes Indexes -> Nonnegative-Fixnum))
  (define (unsafe-array-index->value-index ds js)
    (define dims (vector-length ds))
    (unless (dims . <= . (vector-length js))
      (error 'unsafe-array-index->value-index "internal error"))
    (let loop ([#{i : Nonnegative-Fixnum} 0] [#{j : Nonnegative-Fixnum} 0])
      (cond [(i . < . dims)
             (define di (safe-vector-ref ds i))
             (define ji (safe-vector-ref js i))
             (loop (+ i 1) (unsafe-fx+ ji (unsafe-fx* di j)))]
            [else  j])))

  (: safe-value-index->array-index! (~> ([ds : Indexes]
                                         [j : Nonnegative-Fixnum]
                                         [js : (Refine [js : Indexes]
                                                       (<= (len ds) (len js)))])
                                        Void))
  (define (safe-value-index->array-index! ds j js)
    (with-asserts ([j index?])
      (define dims (vector-length ds))
      (let: loop : Index ([i : (Refine [i : Nonnegative-Fixnum] (<= i dims)) dims] [s : Nonnegative-Fixnum  1])
        (cond [(zero? i)  j]
              [else  (let* ([i  (- i 1)]
                            [j  (loop i (unsafe-fx* s (safe-vector-ref ds i)))])
                       (safe-vector-set! js i (fxquotient j s))
                       (unsafe-fxmodulo j s))]))
      (void)))
  
  ; <refined-local> Refinement on i for ds
  (: unsafe-value-index->array-index! (Indexes Nonnegative-Fixnum Indexes -> Void))
  (define (unsafe-value-index->array-index! ds j js)
    (with-asserts ([j index?])
      (define dims (vector-length ds))
      (let: loop : Index ([i : (Refine [i : Nonnegative-Fixnum] (<= i dims)) dims] [s : Nonnegative-Fixnum  1])
        (cond [(zero? i)  j]
              [else  (let* ([i  (- i 1)]
                            [j  (loop i (unsafe-fx* s (safe-vector-ref ds i)))])
                       (unsafe-vector-set! js i (fxquotient j s))
                       (unsafe-fxmodulo j s))]))
      (void)))
  
  )  ; begin-encourage-inline

;; Using this instead of literal #() is currently slightly faster (about 18% on my machine)
(define: empty-vectorof-index : Indexes
  #())

(: raise-array-index-error (Symbol Indexes In-Indexes -> Nothing))
(define (raise-array-index-error name ds js)
  (error name "expected indexes for shape ~e; given ~e"
         (vector->list ds) js))

(: array-index->value-index (Symbol Indexes In-Indexes -> Nonnegative-Fixnum))
(define (array-index->value-index name ds js)
  (define (raise-index-error) (raise-array-index-error name ds js))
  (define dims (vector-length ds))
  (unless (= dims (vector-length js)) (raise-index-error))
  (let loop ([#{i : Nonnegative-Fixnum} 0] [#{j : Nonnegative-Fixnum}  0])
    (cond [(i . < . dims)
           (define di (safe-vector-ref ds i))
           (define ji (safe-vector-ref js i))
           (cond [(and (exact-integer? ji) (0 . <= . ji) (ji . < . di))
                  (loop (+ i 1) (unsafe-fx+ ji (unsafe-fx* di j)))]
                 [else  (raise-index-error)])]
          [else  j])))

;; <refined> Safe version check-array-indexes
(: safe-check-array-indexes (~> ([name : Symbol]
                                 [ds : Indexes]
                                 [js : (Refine [js : In-Indexes]
                                               (= (len ds) (len js)))])
                                Indexes))
(define (safe-check-array-indexes name ds js)
  (define (raise-index-error) (raise-array-index-error name ds js))
  (define dims (vector-length ds))
  (unless (= dims (vector-length js)) (raise-index-error))
  (define new-js : (Refine [new-js : Indexes]
                           (= dims (len new-js))) (make-vector dims 0))
  (let loop ([#{i : Nonnegative-Fixnum} 0])
    (cond [(i . < . dims)
           (define di (safe-vector-ref ds i))
           (define ji (safe-vector-ref js i))
           (cond [(and (exact-integer? ji) (0 . <= . ji) (ji . < . di))
                  (safe-vector-set! new-js i ji)
                  (loop (+ i 1))]
                 [else  (raise-index-error)])]
          [else  new-js])))

; <refined-local> Refinement added for new-js.
(: check-array-indexes (Symbol Indexes In-Indexes -> Indexes))
(define (check-array-indexes name ds js)
  (define (raise-index-error) (raise-array-index-error name ds js))
  (define dims (vector-length ds))
  (unless (= dims (vector-length js)) (raise-index-error))
  (define new-js : (Refine [new-js : Indexes]
                           (= dims (len new-js))) (make-vector dims 0))
  (let loop ([#{i : Nonnegative-Fixnum} 0])
    (cond [(i . < . dims)
           (define di (safe-vector-ref ds i))
           (define ji (unsafe-vector-ref js i))
           (cond [(and (exact-integer? ji) (0 . <= . ji) (ji . < . di))
                  (unsafe-vector-set! new-js i ji)
                  (loop (+ i 1))]
                 [else  (raise-index-error)])]
          [else  new-js])))

; <refined> Safe version of vector-remove
(: safe-vector-remove (All (I) (~> ([vec : (Vectorof I)]
                                    [k : (Refine [k : Index]
                                                 (< k (len vec)))])
                                   (Refine [v : (Vectorof I)] (= (len v) (+ -1 (len vec)))))))
(define (safe-vector-remove vec k)
  (define n (vector-length vec))
  (define n-1 (sub1 n))
  (cond
    [(not (index? n-1)) (error 'unsafe-vector-remove "internal error")]
    [else
     (define new-vec : (Refine [new-vec : (Vectorof I)]
                               (= n-1 (len new-vec))) (make-vector n-1 (safe-vector-ref vec 0)))
     (let loop ([#{i : Nonnegative-Fixnum} 0])
       (when (i . < . k)
         (safe-vector-set! new-vec i (safe-vector-ref vec i))
         (loop (+ i 1))))
     (let loop ([#{i : Nonnegative-Fixnum} k])
       (cond [(i . < . n-1)
              (safe-vector-set! new-vec i (safe-vector-ref vec (+ i 1)))
              (loop (+ i 1))]
             [else  new-vec]))]))

; <refined-local> Internal refinement on new-vec added
(: unsafe-vector-remove (All (I) ((Vectorof I) Index -> (Vectorof I))))
(define (unsafe-vector-remove vec k)
  (define n (vector-length vec))
  (define n-1 (sub1 n))
  (cond
    [(not (index? n-1)) (error 'unsafe-vector-remove "internal error")]
    [else
     (define new-vec : (Refine [new-vec : (Vectorof I)]
                               (= n-1 (len new-vec))) (make-vector n-1 (safe-vector-ref vec 0)))
     (let loop ([#{i : Nonnegative-Fixnum} 0])
       (when (i . < . k)
         (unsafe-vector-set! new-vec i (unsafe-vector-ref vec i))
         (loop (+ i 1))))
     (let loop ([#{i : Nonnegative-Fixnum} k])
       (cond [(i . < . n-1)
              (safe-vector-set! new-vec i (safe-vector-ref vec (+ i 1)))
              (loop (+ i 1))]
             [else  new-vec]))]))

; <refined> Safe insert function
(: safe-vector-insert (All (I) (~> ([vec : (Vectorof I)]
                                    [k : (Refine [k : Index]
                                                 (<= k (len vec)))]
                                    [v : I])
                                   (Refine [v : (Vectorof I)] (= (len v) (+ 1 (len vec))) ))))
(define (safe-vector-insert vec k v)
  (define n (vector-length vec))
  (define dst-vec : (Refine [dst-vec : (Vectorof I)]
                            (= (+ n 1) (len dst-vec))) (make-vector (+ n 1) v))
  (let loop ([#{i : Nonnegative-Fixnum} 0])
    (when (i . < . k)
      (unsafe-vector-set! dst-vec i (safe-vector-ref vec i))
      (loop (+ i 1))))
  (let loop ([#{i : Nonnegative-Fixnum} k])
    (when (i . < . n)
      (let ([i+1  (+ i 1)])
        (safe-vector-set! dst-vec i+1 (safe-vector-ref vec i))
        (loop i+1))))
  dst-vec)

; <refined-local> Internal refinement on dst-vec added.
(: unsafe-vector-insert (All (I) (~> ([vec : (Vectorof I)]
                                      [k : Index]
                                      [v : I])
                                     (Refine [v : (Vectorof I)] (= (len v) (+ 1 (len vec))) ))))
(define (unsafe-vector-insert vec k v)
  (define n (vector-length vec))
  (define dst-vec : (Refine [dst-vec : (Vectorof I)]
                            (= (+ n 1) (len dst-vec))) (make-vector (+ n 1) v))
  (let loop ([#{i : Nonnegative-Fixnum} 0])
    (when (i . < . k)
      (unsafe-vector-set! dst-vec i (unsafe-vector-ref vec i))
      (loop (+ i 1))))
  (let loop ([#{i : Nonnegative-Fixnum} k])
    (when (i . < . n)
      (let ([i+1  (+ i 1)])
        (safe-vector-set! dst-vec i+1 (safe-vector-ref vec i))
        (loop i+1))))
  dst-vec)

(: port-next-column (Output-Port -> Natural))
;; Helper to avoid the annoying #f column value
(define (port-next-column port)
  (define-values (_line col _pos) (port-next-location port))
  (if col col 0))

; <refine-local> Refinements on visited, new-perm, and new-ds.
; This could possibly be rewritten without the fail input using refinements.
(: apply-permutation (All (A) ((Listof Integer) Indexes (-> Nothing) -> (Values Indexes Indexes))))
(define (apply-permutation perm ds fail)
  (define dims (vector-length ds))
  (unless (= dims (length perm)) (fail))
  (define visited  : (Refine [visited : (Vectorof Boolean)] (= dims (len visited))) (make-vector dims #f))
  (define new-perm : (Refine [new-perm : (Vectorof Index)] (= dims (len new-perm))) (make-vector dims 0))
  (define new-ds   : (Refine [new-ds : Indexes] (= dims (len new-ds))) (make-vector dims 0))
  ;; This loop fails if it writes to a `visited' element twice, or an element of perm is not an
  ;; Index < dims
  (let loop ([perm perm] [#{i : Nonnegative-Fixnum} 0])
    (cond [(i . < . dims)
           (define k (unsafe-car perm))
           (cond [(and (0 . <= . k) (k . < . dims))
                  (cond [(safe-vector-ref visited k)  (fail)]
                        [else  (safe-vector-set! visited k #t)])
                  (safe-vector-set! new-ds i (safe-vector-ref ds k))
                  (safe-vector-set! new-perm i k)]
                 [else  (fail)])
           (loop (unsafe-cdr perm) (+ i 1))]
          [else  (values new-ds new-perm)])))

(: make-thread-local-indexes (~> ([i : Integer])
                                 (-> (Refine [v : Indexes] (= (len v) i)))))
(define (make-thread-local-indexes dims)
  (let: ([val : (Thread-Cellof (U #f (Refine [v : Indexes] (= (len v) dims))))
              (make-thread-cell #f)])
    (λ () (or (thread-cell-ref val)
              (let: ([v : (Refine [v : Indexes] (= (len v) dims))  (make-vector dims 0)])
                ((inst thread-cell-set!
                       (U #f (Refine [v : Indexes] (= (len v) dims))))
                 val v)
                v)))))

(: all-equal? (Any Any * -> Boolean))
(define (all-equal? x . xs)
  (cond [(empty? xs)  #t]
        [else  (define first-xs (first xs))
               (cond [(equal? x first-xs)  (all-equal? first-xs (rest xs))]
                     [else  #f])]))

; <refined> Bunch of refinements added, local included.
(: safe-next-indexes! (~> ([ds : Indexes]
                           [dims : (Refine [dims : Index] (= dims (len ds)))]
                           [js : (Refine [js : Indexes] (= dims (len js)))])
                          Void))
;; Sets js to the next vector of indexes, in row-major order
(define (safe-next-indexes! ds dims js)
  (let loop ([#{k : (Refine [k : Nonnegative-Fixnum] (<= k dims))}  dims])
    (unless (zero? k)
      (let ([k  (- k 1)])
        (define jk (safe-vector-ref js k))
        (define dk (safe-vector-ref ds k))
        (let ([jk  (+ jk 1)])
          (cond [(jk . >= . dk)
                 (safe-vector-set! js k 0)
                 (loop k)]
                [else
                 (safe-vector-set! js k jk)]))))))

(: next-indexes! (Indexes Index Indexes -> Void))
;; Sets js to the next vector of indexes, in row-major order
(define (next-indexes! ds dims js)
  (let loop ([#{k : Nonnegative-Fixnum}  dims])
    (unless (zero? k)
      (let ([k  (- k 1)])
        (define jk (unsafe-vector-ref js k))
        (define dk (unsafe-vector-ref ds k))
        (let ([jk  (+ jk 1)])
          (cond [(jk . >= . dk)
                 (unsafe-vector-set! js k 0)
                 (loop k)]
                [else
                 (unsafe-vector-set! js k jk)]))))))