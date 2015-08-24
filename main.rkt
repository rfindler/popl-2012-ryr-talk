#lang racket
(require slideshow
         "fable/fable.rkt"
         "study.rkt"
         "typeset-amb.rkt"
         "title.rkt"
         "lifecycle.rkt"
         racket/runtime-path)

(define-runtime-path cover.jpg "cover.jpg")

(define-syntax-rule (go f) (begin (printf "building ~a: " 'f) (flush-output) (time (f))))

(title)

(go fable)

(go lifecycle)

(slide
 (vc-append
  20
  (let ([first-line (t "Redex")]
        [second-line (t "our tool designed to fill this niche")])
    (vc-append (scale/improve-new-text (t "Redex") (/ (pict-width second-line)
                                                      (pict-width first-line)))
               second-line))))

(go study)

(define (recap c1 c2 c3 c4)
  (slide
   (inset (para #:width 400 "Recap:") -50 0 0 0)
   (item #:bullet (if c1 checkmark cbullet) #:width 400 "Automatic typesetting")
   (item #:bullet (if c2 checkmark cbullet) #:width 400 "Unit Testing")
   (item #:bullet (if c3 checkmark cbullet) #:width 400 "Exploring Examples")
   (item #:bullet (if c4 checkmark cbullet) #:width 400 "Random testing")))

(define checkmark (colorize (t "✓") "forestgreen"))
(define cbullet (cbl-superimpose (ghost checkmark) bullet))

(recap #f #f #f #f)

(go typeset-amb)

(recap #t #t #t #t)

(slide
 (vl-append
  20
  (inset (para "Takeaways:") -50 0 0 0)
  (item #:fill? #f "Nobody will produce error-free papers")
  (item #:fill? #f "Errors introduce friction into our communication")
  (item #:fill? #f "Redex can help reduce the errors — with about as much effort as LaTeX requires")))

(slide
 (hc-append 
  100
  (scale/improve-new-text
   (vc-append (t "Thank")
              (t "you."))
   3)
  (scale (bitmap cover.jpg)
         1/3)))


(printf "done\n")
