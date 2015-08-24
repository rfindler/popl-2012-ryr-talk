#lang racket
(require slideshow
         slideshow/play
         "color.rkt"
         racket/runtime-path)

(provide study)

(define false-theorem "False Theorem:")
(define unexpected-behavior "Unexpected Behavior:")
(define typesetting-error "Copy & Paste Typesetting Error:")
(define erroneous-example "Erroneous Example:")
             
(define-runtime-path coerce-before.jpg "coerce-before.jpg")
(define-runtime-path coerce-after.jpg "coerce-after.jpg")
(define-runtime-path dswitch.jpg "dswitch.jpg")

(define coerce-before (bitmap coerce-before.jpg))
(define coerce-after (bitmap coerce-after.jpg))

(define (study)
  (slide (study-overview ghost))
  (slide (study-overview values))
  (fraction)
  
  (slide (position typesetting-error (dswitch-pict ghost values)))
  (slide (position typesetting-error (dswitch-pict values values)))
  (slide (position typesetting-error (dswitch-pict values values)
                   "Typesetting should be automatic"))
  (slide (position erroneous-example (fiddle coerce-before ghost)))
  (slide (position erroneous-example (fiddle coerce-before values)))
  (slide (position erroneous-example (fiddle coerce-after ghost)))
  (slide (position erroneous-example (fiddle coerce-after ghost)
                   "Examples can be tested"))
  (slide (position unexpected-behavior (deadlock-main ghost ghost)))
  (slide (position unexpected-behavior (deadlock-main values ghost)))
  (slide (position unexpected-behavior (deadlock-main values values)))
  (slide (position unexpected-behavior (deadlock-main values values)
                   "Found this by playing with examples"))
  (slide (position false-theorem (memo-main ghost)))
  (slide (position false-theorem (memo-main values)))
  (slide (position false-theorem (memo-main values)
                   "Random testing easily finds this")))

(define (position title pict [moral #f])
  (define spacer 
    (ghost 
     (launder
      (lt-superimpose (dswitch-pict ghost ghost)
                      (fiddle coerce-after ghost)
                      (deadlock-main ghost ghost)
                      (memo-main ghost)))))
  (define combined
    (ct-superimpose 
     spacer
     pict))
  (vl-append 20 
             (colorize (bt title) navy-blue)
             (let-values ([(_ y) (cb-find combined pict)])
               (pin-over combined
                         0
                         (+ y 20)
                         (if moral
                             (moral-t moral)
                             (ghost (moral-t "blank moral")))))))

(define (memo-lhs)
  (parameterize ([current-main-font 'roman])
    (hbl-append (t "(λ")
                (sub-t "δ")
                (it " x. x")
                (t ") 1"))))

(define (moral-t str)
  (colorize (bt str) red))

(define (memo-main counter-ex)
  (define but (t "but "))
  (define then (t "then"))
  
  (vl-append
   40
   (vl-append
    (para "If a term reduces with a memo store, then the program"
          "without the memo store reduces the same way"))
   (blank)
   
   (hc-append
    100 
    (vl-append
     (counter-ex (t "Counterexample:"))
     (counter-ex
      (vl-append
       20
       (hbl-append (t "If ")
                   (parameterize ([current-main-font 'roman])
                     (t "σ = {(δ,1) → 2}"))
                   (t " then"))
       (hbl-append
        (ghost but)
        (parameterize ([current-main-font 'roman])
          (hbl-append (memo-lhs)
                      (t ", σ")
                      (t " ⇒* ")
                      (t "2, σ")))
        (t ","))
       (parameterize ([current-main-font 'roman])
         (hbl-append but
                     (memo-lhs)
                     (t " ↦ ")
                     (t "1"))))))
    
    (counter-ex
     (para #:width 300
           "Not a fly-by-night proof; 12 typeset pages in a dissertation chapter")))))

(define deadlock-line2 (t "Deadlock in source but busy waiting in target"))

(define (overline p)
  (define y (* (- (pict-height p)
                  (pict-descent p))
               1/4))
  (define w (pict-width p))
  (cbl-superimpose
   p
   (linewidth
    2
    (dc
     (λ (dc dx dy)
       (send dc draw-line dx (+ dy y) (+ dx w) (+ dy y)))
     (pict-width p)
     (pict-height p)
     (pict-ascent p)
     (pict-descent p)))))

(define problematic-term
  (parameterize ([current-main-font 'roman])
    (hbl-append (t "select(c, ")
                (overline (t "c"))
                (t ")"))))
(define problematic-term-compiled
  (parameterize ([current-main-font 'roman])
    (hbl-append 
     (t "⊙")
     (parameterize ([current-main-font '(subscript . roman)])
       (t "c"))
     (t " | ")
     (refocus
      (vc-append -10 (t "~") problematic-term)
      problematic-term))))

(define (annot prob comp comment comment-f)
  (hbl-append 20
              (lbl-superimpose (prob problematic-term)
                               (comp problematic-term-compiled))
              (comment-f
               (hbl-append (t " – ")
                           (t comment)))))

(define (add-compile-arrow main from to)
  (define ((shift-left f) a b)
    (define-values (x y) (f a b))
    (values (- x 10) y))
  (define-values (from-x from-y) ((shift-left lb-find) main from))
  (define-values (to-x to-y) ((shift-left lt-find) main to))
  (define b (blank))
  (define pull 1/2)
  (pin-over (pin-over (linewidth
                       4
                       (pin-line 
                        main 
                        from (shift-left lb-find)
                        to (shift-left lt-find)
                        #:start-pull pull
                        #:end-pull pull
                        #:start-angle (* pi 5/4)
                        #:end-angle (+ pi (* pi 3/4))))
                      to-x to-y 
                      (refocus (cc-superimpose b (arrowhead 20 (+ pi (* pi 3/4))))
                               b))
            (- from-x 20) 
            (+ from-y (/ (- to-y from-y) 2))
            (refocus (rc-superimpose (t "compile") b)
                     b)))


(define (deadlock-main comp what-happens)
  (define before (annot values ghost "stuck" what-happens))
  (define after (comp (annot ghost values "loops forever" what-happens)))
  (define together (vl-append 30 before after))
  (define compilation (comp (add-compile-arrow (ghost together) before after)))
  (define combined
    (vc-append
     40
     (cc-superimpose compilation together)
     (blank)
     (what-happens (what-happens deadlock-line2))))
  combined)

(define (sub-t str)
  (parameterize ([current-main-font (cons 'subscript (current-main-font))])
    (t str)))

(define (cal x)
  (parameterize ([current-main-font "Apple Chancery"])
    (t x)))

(define (study-overview ans)
  (define q1 (item #:fill? #f "Can random testing find bugs in an existing, well-tested Redex model?"))
  (define q2 (item #:fill? #f "Can Redex find bugs in published papers?"))
  (define yes (cc-superimpose (ghost q1) 
                              (ghost q2)
                              (colorize (bt "Yes") red)))
  (vl-append
   20
   (inset (t "Our study:") -50 0 0 0)
   (blank)
   (cc-superimpose q1)
   (ans yes)
   (blank)
   (cc-superimpose q2)
   (ans yes)))

(define dswitch (scale (bitmap dswitch.jpg) 1/2))

(define (dswitch-pict bubble ts-bug)
  (define h (pict-height dswitch))
  (define w (pict-width dswitch))
  (define red-ellipse (linewidth
                       6
                       (colorize (ellipse 80 40) red)))
  (pin-over dswitch
            (- (/ w 2) (pict-width red-ellipse) 70)
            (- h (pict-height red-ellipse) 4)
            (bubble red-ellipse)))

(define (fiddle bm bubbles)
  (vl-append 20 
             (lt-superimpose 
              (scale (inset/clip bm -20 -90 -50 -300) 1/2)
              (bubbles
               (colorize
                (linewidth
                 6
                 (pin-over
                  (pin-over
                   (blank)
                   100 90
                   (ellipse 100 50))
                  320 90
                  (ellipse 50 50)))
                red)))))


(define (fraction)
  (define (animate n* n4)
    (define-values (n1 n2) (split-phase n*))
    (define (t2 str) (parameterize ([current-font-size 40]) (t str)))
    (define left-right-gap 100)
    (define big-ten (scale/improve-new-text (t "10") 10))
    (define num (hc-append left-right-gap
                           big-ten
                           ((if (= n4 0) values ghost)
                            (vl-append (t2 "papers with")
                                       (t2 "errors")))))
    (define den (hc-append left-right-gap 
                           (launder big-ten)
                           ((if (= n4 0) values ghost)
                            (table
                             2
                             (map t2 '("10" 
                                       "papers in Redex"
                                       "9" "ICFP ’09 papers" 
                                       "8" "written by others"
                                       "2" "mechanically verified"))
                             (cons rbl-superimpose lbl-superimpose)
                             (cons rbl-superimpose lbl-superimpose)
                             (pict-width (t2 " "))
                             0))))
    (define den-start (launder (ghost den)))
    (define den-end (launder (ghost den)))
    (define bar-size (+ (pict-width big-ten) 30))
    (define your-papers-too
      (scale/improve-new-text
       (let* ([big-your-factor 1/3]
              [mk-your (λ (n) (scale/improve-new-text 
                               (it "Your")
                               (+ 1 (* n big-your-factor))))]
              [big-your (ghost (mk-your 1))])
         (inset ((if (= n4 0) ghost values)
                 (colorize 
                  (vl-append (rc-superimpose
                              (inset big-your 0 0 0 0)
                              (mk-your n4))
                             (t "papers")
                             (vl-append
                              -4
                              (t "have")
                              (t "errors")
                              (t "too")))
                  red))
                0 0 40 0))
       2))
    (slide-pict 
     (rc-superimpose (cc-superimpose 
                      (vl-append -40 
                                 (cellophane num n2)
                                 (cellophane 
                                  (dc (λ (dc dx dy)
                                        (define pen (send dc get-pen))
                                        (send dc set-pen "black" 20 'solid)
                                        (send dc draw-line dx dy (+ dx bar-size) dy)
                                        (send dc set-pen pen))
                                      bar-size
                                      10)
                                  n2)
                                 den-end)
                      den-start)
                     your-papers-too)
     den
     den-start
     den-end
     n1))
  (cond
    [printing? 
     (slide (animate 0 0))
     (slide (animate 1 0))
     (slide (animate 1 1))]
    [else
     (play-n #:steps '(30 . 30) animate)]))
