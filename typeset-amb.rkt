#lang racket/base
(require "amb.rkt"
         "util.rkt"
         redex
         slideshow
         slideshow/play
         racket/gui/base)

(provide typeset-amb)

;; steps : int
(define steps 
  (cond
    [(getenv "SPEEDUP") 2]
    [else 32]))

(define (typeset-amb)
  (cond
    [printing?
     (slide (nth-pict 0))
     (slide (nth-pict 1))]
    [else
     (play-n
      #:steps (- steps 1)
      (λ (n) (hash-ref nth-pict-table n)))]))

(default-font-size 24)
(label-font-size (default-font-size))
(metafunction-font-size (default-font-size))
(literal-style '(bold . roman))
(label-style "Gill Sans")
(default-style 'roman)

(define (nth-pict n)
  (define-values (x0 y0 w0 h0) (find-p-grammar L-pict))
  (define-values (x1 y1 w1 h1) (find-grammar L-pict))
  (define-values (x2 y2 w2 h2) (find-red red-pict))
  (define-values (x3 y3 w3 h3) (find-types types-pict))
  (define-values (src0 w/src0) (extract-subpict main L-pict x0 y0 w0 h0 n))
  (define-values (src1 w/src1) (extract-subpict w/src0 L-pict x1 y1 w1 h1 n))
  (define-values (src2 w/src2) (extract-subpict w/src1 red-pict x2 y2 w2 h2 n))
  (define-values (src3 w/src3) (extract-subpict w/src2 types-pict x3 y3 w3 h3 n))
  (define dest0 (scale (ghost+launder src0) 2))
  (define dest1 (scale (ghost+launder src1) 2))
  (define dest2 (scale (ghost+launder src2) 2))
  (define dest3 (scale (ghost+launder src3) 2))
  (define w/dest3 (cc-superimpose w/src3 (vc-append 40 
                                                    dest1 dest3
                                                    (blank) (blank)
                                                    dest0 dest2)))
  (cc-superimpose
   (cellophane main (+ 1/4 (* 3/4 (- 1 n))))
   (slide-pict 
    (slide-pict 
     (slide-pict 
      (slide-pict 
       (ghost w/dest3)
       (scale src3 (+ 1 n))
       src3 dest3 n)
      (scale src2 (+ 1 n))
      src2 dest2 n) 
     (scale src1 (+ 1 n))
     src1 dest1 n)
    (scale src0 (+ 1 n))
    src0 dest0 n)))

(define (ghost+launder p) 
  (ghost (launder p)))

(define (find-p-grammar L-pict)
  (define lh (default-line-height))
  (values -10
          0
          120
          lh))

(define (find-grammar L-pict)
  (define lh (default-line-height))
  (values 30
          (* lh 6)
          130
          lh))

(define (find-red red-pict)
  (define lh (default-line-height))
  (values 0
          (+ (* lh 4)
             (* 2 (reduction-relation-rule-separation))
             30 ;; where is this coming from?
             )
          (pict-width red-pict)
          (+ (* 2 lh)
             8  ;; how about this?!
             )))

(define (find-types types-pict)
  (define lh (default-line-height))
  (define width-inset 70)
  (values width-inset
          (+ (* lh 13)
             (* (horizontal-bar-spacing) 14)
             (* 20 ;; gap between rules 
                7)
             0 ;22 ;; where is this coming from?
             )
          (- (pict-width types-pict)
             width-inset
             width-inset)
          (+ (* 2 lh)
             (* (horizontal-bar-spacing) 2))))

(define (default-line-height)
  (pict-height (text "xY" (default-style) (default-font-size))))

(define (extract-subpict main p x y w h n)
  (define dest
    (fade-frame
     n
     (white-backing
      (launder
       (inset/clip p
                   (- x)
                   (- y) 
                   (- (- (pict-width p) x w))
                   (- (- (pict-height p) y h)))))))
  (define-values (mx my) (lt-find main p))
  (values dest
          (pin-over main
                    (+ mx x)
                    (+ my y)
                    dest)))

(define (fade-frame n p)
  (refocus 
   (cc-superimpose p
                   (cellophane
                    (frame (blank (+ (pict-width p) selection-region-extra-space)
                                  (+ (pict-height p) selection-region-extra-space)))
                    n))
   p))
              
(define (white-backing p)
  (refocus (cc-superimpose 
            (colorize (filled-rectangle (+ (pict-width p) selection-region-extra-space)
                                        (+ (pict-height p) selection-region-extra-space))
                      "white")
            p)
           p))

(define selection-region-extra-space 8)

(define (with-rewriters/proc t)
  (with-atomic-rewriter
   ':
   (λ () (text ":" (default-style) (default-font-size)))
   (with-compound-rewriter
    'types
    (λ (lws)
      (define gamma (list-ref lws 2))
      (define exp (list-ref lws 3))
      (define type (list-ref lws 4))
      (list "" gamma " ⊢ " exp " : " type ""))
    (with-compound-rewriter
     'different
     (λ (lws)
       (define lhs (list-ref lws 2))
       (define rhs (list-ref lws 3))
       (list "" lhs " ≠ " rhs ""))
     (with-compound-rewriter
      'subst
      (λ (lws)
        (define xv (lw-e (list-ref lws 3)))
        (define x (list-ref xv 1))
        (define v (list-ref xv 2))
        (define body (list-ref lws 2))
        (list "" body "{" x ":=" v " ...}"))
      (with-compound-rewriter
       'λ
       (λ (lws)
         (match-define (list opn lam params body close) lws)
         (define (open-paren? x)
           (and (lw? x) (equal? "(" (lw-e x))))
         (define new-params
           (for/list ([param (in-list (lw-e params))]
                      [n (in-naturals)])
             (match (lw-e param)
               [(list (? open-paren? op)  x t cp)
                (struct-copy lw param
                             [e 
                              (if (= n 1)
                                  (list 'spring 
                                        x 
                                        'spring
                                        (just-before ":" t)
                                        t)
                                  (list 'spring 
                                        (just-before " " x)
                                        x 
                                        'spring
                                        (just-before ":" t)
                                        t))])]
               [else param])))
         (list opn "λ " 
               (struct-copy lw params
                            [e new-params])
               body close))
       (t)))))))

(define-syntax-rule
  (with-rewriters e)
  (with-rewriters/proc (λ () e)))

(define types-pict
  (with-rewriters
   (judgment-form->pict types)))

(define red-pict
  (with-rewriters
   (reduction-relation->pict
    red
    #:style 'compact-vertical)))

(define L-pict
  (with-rewriters
   (language->pict L
                   #:nts (remove 'x (language-nts L)))))

(define Ev-pict
  (with-rewriters
   (language->pict Ev)))

(define main 
  (evenize
   (hc-append 50
              (vl-append 40
                         L-pict 
                         Ev-pict
                         red-pict)
              types-pict)))

(define nth-pict-table (make-hash))
(unless printing?
  (time
   (begin
     (printf "rendering amb ") (flush-output)
     (for ([x (in-range steps)])
       (define n (/ x (- steps 1) 1.0))
       (hash-set! nth-pict-table n (to-bitmap (nth-pict n)))
       (display ".") (flush-output))
     (printf " done\n"))))

(module+ main
  (typeset-amb))