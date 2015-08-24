#lang racket
(require redex "subst.rkt")
(provide L Ev red types)

(define-language L
  (p (e ...))
  (e (e e ...)
     (λ ((x t) ...) e)
     x
     (+ e ...)
     number
     (amb e ...))
  (t (→ t ... t) num)
  (x variable-not-otherwise-mentioned))

(define-extended-language Ev L
  (P (e ... E e ...))
  (E (v ... E e ...) 
     (+ v ... E e ...)
     hole)
  (v (λ ((x t) ...) e)
     number)
  (Γ · (x : t Γ)))

(define red
  (reduction-relation 
   Ev
   #:domain p
   (--> (in-hole P ((λ ((x t) ..._1) e) v ..._1))
        (in-hole P (subst e (x v) ...))
        "βv")
   (--> (in-hole P (+ number_1 ...))
        (in-hole P (Σ number_1 ...))
        "+")
   (--> (e_1 ... (in-hole E (amb e_2 ...)) e_3 ...)
        (e_1 ... (in-hole E e_2) ... e_3 ...)
        "amb")))

(define-judgment-form
  Ev
  #:contract (types Γ e t)
  #:mode (types I I O)
  
  [(types Γ e_1 (→ t_2 ... t_3))
   (types Γ e_2 t_2) ...
   -----
   (types Γ (e_1 e_2 ...) t_3)]
  
  [(types (x_1 : t_1 Γ) (λ ((x_2 t_2) ...) e) (→ t_2 ... t))
   -----
   (types Γ (λ ((x_1 t_1) (x_2 t_2) ...) e) (→ t_1 t_2 ... t))]
  [(types Γ e t)
   -----
   (types Γ (λ () e) (→ t))]
  
  [(types (x : t Γ) x t)]
  [(types Γ x_1 t_1)
   (side-condition (different x_1 x_2))
   -----
   (types (x_2 : t_2 Γ) x_1 t_1)]
  
  [(types Γ e num) ...
   -----
   (types Γ (+ e ...) num)]
  [(types Γ number num)]
  
  [(types Γ e num) ...
   -----
   (types Γ (amb e ...) num)])

(define-metafunction Ev
  Σ : number ... -> number
  [(Σ number ...) ,(apply + (term (number ...)))])

(define-metafunction Ev
  subst : any (x any) ... -> any
  [(subst any_body (x any_b) ...)
   ,(subst/proc x? (term (x ...))
                (term (any_b ...))
                (term any_body))])
(define x? (redex-match Ev x))

(define-metafunction Ev
  different : x x -> any
  [(different x x) #f]
  [(different x_1 x_2) #t])

(define (progress)
  (redex-check
   Ev
   e
   (if (types? (term e))
       (or (v? (term e))
           (reduces? (term e)))
       #t)))

(define (types? e)
  (not (null? (judgment-holds (types · ,e t)
                              t))))

(define v? (redex-match Ev v))

(define (reduces? e)
  (not (null? (apply-reduction-relation 
               red
               (term (,e))))))
