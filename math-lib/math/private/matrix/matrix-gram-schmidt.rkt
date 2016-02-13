#lang typed/racket/base

(require typed/safe/ops
         racket/fixnum
         racket/list
         "matrix-types.rkt"
         "matrix-basic.rkt"
         "matrix-conversion.rkt"
         "matrix-constructors.rkt"
         "utils.rkt"
         "../unsafe.rkt"
         "../vector/vector-mutate.rkt"
         "../array/array-struct.rkt"
         "../array/array-constructors.rkt"
         "../array/array-indexing.rkt")

(provide matrix-gram-schmidt
         matrix-basis-extension)

; <refined-local> Refinement added to find-nonzero-vector for its use in matrix-gram-schmidt/ns
(: find-nonzero-vector (case-> (~> ([vss : (Vectorof (Vectorof Flonum))])
                                   (U #f (Refine [i : Index] (< i (len vss)))))
                               (~> ([vss : (Vectorof (Vectorof Real))])
                                   (U #f (Refine [i : Index] (< i (len vss)))))
                               (~> ([vss : (Vectorof (Vectorof Float-Complex))])
                                   (U #f (Refine [i : Index] (< i (len vss)))))
                               (~> ([vss : (Vectorof (Vectorof Number))])
                                   (U #f (Refine [i : Index] (< i (len vss)))))))
(define (find-nonzero-vector vss)
  (define n (vector-length vss))
  (cond [(= n 0)  #f]
        [else  (let loop ([#{i : Nonnegative-Fixnum} 0])
                 (cond [(i . fx< . n)
                        (define vs (safe-vector-ref vss i))
                        (if (vector-zero? vs) (loop (fx+ i 1)) i)]
                       [else  #f]))]))

; <refined-local> Safe version of subtract-projections! that requires knowledge about m with respect to rows.
(: safe-subtract-projections!
   (case-> (~> ([rows : (Vectorof (Vectorof Flonum))]
                [i : Nonnegative-Fixnum]
                [m : (Refine [m : Index] (= m (len rows)))]
                [row : (Vectorof Flonum)]) Void)
           (~> ([rows : (Vectorof (Vectorof Real))]
                [i : Nonnegative-Fixnum]
                [m : (Refine [m : Index] (= m (len rows)))]
                [row : (Vectorof Real)]) Void)
           (~> ([rows : (Vectorof (Vectorof Float-Complex))]
                [i : Nonnegative-Fixnum]
                [m : (Refine [m : Index] (= m (len rows)))]
                [row : (Vectorof Float-Complex)]) Void)
           (~> ([rows : (Vectorof (Vectorof Number))]
                [i : Nonnegative-Fixnum]
                [m : (Refine [m : Index] (= m (len rows)))]
                [row : (Vectorof Number)]) Void)))
(define (safe-subtract-projections! rows i m row)
  (let loop ([#{i : Nonnegative-Fixnum} i])
    (when (i . fx< . m)
      (vector-sub-proj! (safe-vector-ref rows i) row #f)
      (loop (fx+ i 1)))))

(: matrix-gram-schmidt/ns (case-> ((Matrix Flonum) Any Integer -> (Array Flonum))
                                  ((Matrix Real) Any Integer -> (Array Real))
                                  ((Matrix Float-Complex) Any Integer -> (Array Float-Complex))
                                  ((Matrix Number) Any Integer -> (Array Number))))
;; Performs Gram-Schmidt orthogonalization on M, assuming the rows before `start' are already
;; orthogonal
(define (matrix-gram-schmidt/ns M normalize? start)
  (define rows (matrix->vector* (matrix-transpose M)))
  (define m : (Refine [m : Index] (= m (len rows))) (vector-length rows))
  (define i (find-nonzero-vector rows))
  (cond [(not (index? start))
         (raise-argument-error 'matrix-gram-schmidt "Index" 2 M normalize? start)]
        [i
         (define rowi (safe-vector-ref rows i))
         (safe-subtract-projections! rows (fxmax start (fx+ i 1)) m rowi)
         (when normalize? (vector-normalize! rowi))
         (let loop ([#{i : Nonnegative-Fixnum} (fx+ i 1)] [bs (list rowi)])
           (cond [(i . fx< . m)
                  (define rowi (safe-vector-ref rows i))
                  (cond [(vector-zero? rowi)  (loop (fx+ i 1) bs)]
                        [else  (safe-subtract-projections! rows (fxmax start (fx+ i 1)) m rowi)
                               (when normalize? (vector-normalize! rowi))
                               (loop (fx+ i 1) (cons rowi bs))])]
                 [else
                  (matrix-transpose (vector*->matrix (list->vector (reverse bs))))]))]
        [else
         (make-array (vector (matrix-num-rows M) 0)
                     ;; Value won't be in the matrix, but this satisfies TR:
                     (zero* (unsafe-vector2d-ref rows 0 0)))]))

(: matrix-gram-schmidt (case-> ((Matrix Flonum)             -> (Array Flonum))
                               ((Matrix Flonum) Any         -> (Array Flonum))
                               ((Matrix Flonum) Any Integer -> (Array Flonum))
                               ((Matrix Real)             -> (Array Real))
                               ((Matrix Real) Any         -> (Array Real))
                               ((Matrix Real) Any Integer -> (Array Real))
                               ((Matrix Float-Complex)             -> (Array Float-Complex))
                               ((Matrix Float-Complex) Any         -> (Array Float-Complex))
                               ((Matrix Float-Complex) Any Integer -> (Array Float-Complex))
                               ((Matrix Number)             -> (Array Number))
                               ((Matrix Number) Any         -> (Array Number))
                               ((Matrix Number) Any Integer -> (Array Number))))
(define (matrix-gram-schmidt M [normalize? #f] [start 0])
  (call/ns (λ () (matrix-gram-schmidt/ns M normalize? start))))

(: matrix-basis-extension/ns (case-> ((Matrix Flonum)   -> (Array Flonum))
                                     ((Matrix Real)   -> (Array Real))
                                     ((Matrix Float-Complex) -> (Array Float-Complex))
                                     ((Matrix Number) -> (Array Number))))
(define (matrix-basis-extension/ns B)
  (define-values (m n) (matrix-shape B))
  (define x00 (matrix-ref B 0 0))
  (define zero (zero* x00))
  (define one (one* x00))
  (cond [(n . < . m)
         (define S (matrix-gram-schmidt (matrix-augment (list B (identity-matrix m one zero))) #f n))
         (define R (submatrix S (::) (:: n #f)))
         (matrix-augment (take (sort/key (matrix-cols R) > matrix-norm) (- m n)))]
        [(n . = . m)
         (make-array (vector m 0) zero)]
        [else
         (raise-argument-error 'matrix-extend-row-basis "matrix? with width < height" B)]))

(: matrix-basis-extension (case-> ((Matrix Flonum) -> (Array Flonum))
                                  ((Matrix Real) -> (Array Real))
                                  ((Matrix Float-Complex) -> (Array Float-Complex))
                                  ((Matrix Number) -> (Array Number))))
(define (matrix-basis-extension B)
  (call/ns (λ () (matrix-basis-extension/ns B))))