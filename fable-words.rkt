#lang racket
(require slideshow
         slideshow/play)

(define LaTeX (hbl-append (t "L")
                          (inset (lift-above-baseline (scale/improve-new-text (t "A") .8) 4)
                                 -6 0 -4 0)
                          (t "T")
                          (inset (lift-above-baseline (t "E") -6) -4 0 0 0)
                          (t "X")))

(define C (t "C"))
(define is-well-known (t " is well-known to be an effective programming"))
(define is-well-known2 (launder is-well-known))
(define building-systems-software (t "building systems software."))
(define typesetting-technical-papers (t "typesetting technical papers."))
(define first-template
  (vl-append (vl-append
              (lbl-superimpose
               (hbl-append (ghost C) (ghost is-well-known))
               (hbl-append (ghost LaTeX) (ghost is-well-known2)))
              (hbl-append
               (t "language for ")
               (lbl-superimpose (ghost building-systems-software)
                                (ghost typesetting-technical-papers))))))

(define performance (t "performance"))
(define beautiful-output (t "beautiful output"))
(define and-provides (t " and provides"))
(define and-provides2 (launder and-provides))

(define is-designed-for (t " is designed for "))
(define is-designed-for2 (launder is-designed-for))

(define maintain-invariants (t "maintain invariants or detect errors early."))
(define check-formulas (t "check formulas or to run examples."))

(define second-template
  (vl-append 
   (hbl-append
    (t "After all, ")
    (lbl-superimpose
     (hbl-append (ghost C)
                 (ghost is-designed-for)
                 (ghost performance)
                 (ghost and-provides))
     (hbl-append (ghost LaTeX)
                 (ghost is-designed-for2)
                 (ghost beautiful-output)
                 (ghost and-provides2))))
   (hbl-append (t "no help to ")
               (lbl-superimpose (ghost maintain-invariants)
                                (ghost check-formulas)))))

(slide 
 (vl-append 100 
            (pin-over
             (pin-over
              (pin-over
               first-template
               building-systems-software lt-find building-systems-software)
              is-well-known lt-find is-well-known)
             C lt-find C)
            (ghost second-template)))

(play-n
 #:steps 20
 (λ (n12 n34)
   (define-values (n1 n2) (split-phase n12))
   (define-values (n3 n4) (split-phase n34))
   
   (vl-append 100
              (slide-pict 
               (pin-over
                (pin-over
                 first-template
                 building-systems-software
                 lt-find
                 (fade-pict n2
                            (lt-superimpose building-systems-software
                                            (ghost typesetting-technical-papers))
                            (lt-superimpose (ghost building-systems-software)
                                            typesetting-technical-papers)))
                C
                lt-find
                (fade-pict n2 (lt-superimpose C (ghost LaTeX)) LaTeX))
               is-well-known 
               is-well-known is-well-known2
               n1)
              
              (slide-pict 
               (slide-pict 
                (slide-pict 
                 (pin-over
                  (pin-over
                   second-template
                   maintain-invariants
                   lt-find
                   (fade-pict n4
                              (lbl-superimpose maintain-invariants
                                               (ghost check-formulas))
                              (lbl-superimpose (ghost maintain-invariants)
                                               check-formulas)))
                  C
                  lt-find
                  (fade-pict n4 (lt-superimpose C (ghost LaTeX)) LaTeX))
                 (fade-pict n4
                            (lbl-superimpose performance (ghost beautiful-output))
                            (lbl-superimpose (ghost performance) beautiful-output))
                 performance beautiful-output
                 n3)
                is-designed-for
                is-designed-for is-designed-for2
                n3)
               and-provides
               and-provides and-provides2
               n3))))

