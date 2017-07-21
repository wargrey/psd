#lang typed/racket/base

(provide (all-defined-out))
(provide (all-from-out racket/fixnum))

(require racket/fixnum)

(define nonnegative-fixnum? : (-> Any Boolean : Nonnegative-Fixnum) (λ [n] (and (fixnum? n) (fx>= n 0))))

(define positive-byte? : (-> Any Boolean : Positive-Byte) (λ [v] (and (byte? v) (fx> v 0))))
(define positive-index? : (-> Any Boolean : Positive-Index) (λ [v] (and (index? v) (fx> v 0))))
(define positive-fixnum? : (-> Any Boolean : Positive-Fixnum) (λ [n] (and (fixnum? n) (fx> n 0))))
