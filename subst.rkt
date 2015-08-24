#lang racket/base
(require racket/set racket/match
         redex/reduction-semantics)
(provide subst/proc fvs)

(define (subst/proc x? vars replacements body)
  (define replacements-ht
    (for/fold ([m (hash)])
              ([v (in-list vars)]
               [rep (in-list replacements)])
      (hash-set m v rep)))
  (define replacements-free-vars (for/list ([x (in-set (fvs x? replacements))]) x))
  (define replacements-fresh-vars (variables-not-in (cons vars body) 
                                                    replacements-free-vars))
  (define init-fv-map 
    (for/fold ([m (hash)])
              ([fresh (in-list replacements-fresh-vars)]
               [free (in-list replacements-free-vars)])
      (hash-set m free fresh)))
  (let loop ([body body]
             [fvs-inactive init-fv-map]
             [fvs-active (hash)]
             [replacements replacements-ht])
    (match body
      [`(lambda (,xs ...) ,body)
       (define-values (new-xs new-inactive new-active new-replacements)
         (adjust-active-inactive xs fvs-inactive fvs-active replacements))
       `(lambda (,@new-xs)
          ,(loop body new-inactive new-active new-replacements))]
      [(? x? x)
       (cond
         [(hash-ref fvs-active x #f) => values]
         [(hash-ref replacements x #f) => values]
         [else x])]
      [(? list?)
       (map (λ (x) (loop x fvs-inactive fvs-active replacements))
            body)]
      [_ body])))

(define (adjust-active-inactive xs fvs-inactive fvs-active replacements)
  (let loop ([xs xs]
             [new-xs '()]
             [fvs-inactive fvs-inactive]
             [fvs-active fvs-active]
             [replacements replacements])
    (cond
      [(null? xs)
       (values (reverse new-xs)
               fvs-inactive 
               fvs-active
               replacements)]
      [else
       (define x (car xs))
       (define inactive-var? (hash-has-key? fvs-inactive x))
       (define active-var? (hash-has-key? fvs-active x))
       (define new-x
         (cond
           [inactive-var? (hash-ref fvs-inactive x)]
           [active-var? (hash-ref fvs-active x)]
           [else x]))
       (loop (cdr xs)
             (cons new-x new-xs)
             (if inactive-var?
                 (hash-remove fvs-inactive x)
                 fvs-inactive)
             (if inactive-var?
                 (hash-set fvs-active x (hash-ref fvs-inactive x))
                 fvs-active)
             (if (hash-has-key? replacements x)
                 (hash-remove replacements x)
                 replacements))])))

(define (fvs x? body)
  (let loop ([body body])
    (match body
      [`(lambda (,xs ...) ,body)
       (set-subtract (loop body) (apply set xs))]
      [(? x?)
       (set body)]
      [(? list?)
       (for/fold ([fvs (set)])
                 ([e (in-list body)])
         (set-union fvs (loop e)))]
      [_ (set)])))

