#lang racket/base

(require slideshow
         slideshow/play
         racket/class
         racket/math
         (prefix-in 2: 2htdp/image)
         (prefix-in i: mrlib/image-core)
         file/convertible
         racket/gui/base
         "color.rkt")

(define bug-image 
  (2:overlay (2:circle 8 "solid" "black")
             (2:radial-star 16 16 40 "solid"
                            (2:color (send red red)
                                     (send red green)
                                     (send red blue)))))

(define bug-bmp
  (read-bitmap (open-input-bytes (convert bug-image 'png-bytes))))

(define bug-w (send bug-bmp get-width))
(define bug-h (send bug-bmp get-width))

(provide lifecycle)

(define arc-angles
  (list (list -0.03 (* pi 105/320))
        (list (* pi .7) (* pi 1.07))
        (list (* pi 1.3)  (* pi 1.67))))

(define dots 
  (list (list (+ (* pi 4/10) -.05) "misrenamed" "non-terminal")
        (list (* pi 10/10) "forgot" "typing" "rule")
        (list (+ (* pi 5/10) -.05) "lost a case" "in a helper" "function")
        (list (* pi -1/10) "added a" "case to" "wrong fn")
        (list (* pi -3/10) "swappped args")
        (list (+ (* pi 6/10) -.05) "misused the" "inductive hyp.")
        (list (* pi 12/10) "didn’t" "recheck a" "lemma")
        (list (* pi 11/10) "transcribed" "math wrong")
        (list (* pi -2/10) "forgot" "to recheck" "example")))

(define lifecycle-circle-color "black")

(define extra-offset -0.09)

(define (put-at main-pict angle new-pict delta-angle)
  (let ([mw2 (/ (pict-width main-pict) 2)]
        [mh2 (/ (pict-height main-pict) 2)])
    (pin-over
     (pin-over 
      main-pict
      (- (+ mw2 (* mw2 (cos angle)))
         (/ (pict-width new-pict) 2))
      (- (+ mh2 (* mh2 (sin angle)))
         (/ (pict-height new-pict) 2))
      new-pict)
     (+ mw2 (* mw2 (cos (+ angle delta-angle))))
     (+ mh2 (* mh2 (sin (+ angle delta-angle))))
     
     (colorize (pip-arrow-line 
                (cos (+ (/ pi 2) (+ angle delta-angle extra-offset)))
                (sin (+ (/ pi 2) (+ angle delta-angle extra-offset)))
                60)
               lifecycle-circle-color))))

(define (spaced-out a da b db c dc dots)
  (let ([init-angle (* pi -1/2)])
    (cc-superimpose
     (put-at 
      (put-at 
       (put-at 
        (linewidth 4 (colorize (linewidth 16 
                                          (arcs 300))
                               lifecycle-circle-color))
        (+ init-angle (* pi 2/3) (* pi 2/3))
        c
        dc)
       (+ init-angle (* pi 2/3))
       b
       db)
      init-angle
      a
      da)
     (colorize (linewidth 16 (red-dots 300 dots)) red))))

(define (arcs diameter)
  (dc 
   (λ (dc dx dy)
     (define brush (send dc get-brush))
     (send dc set-brush "black" 'transparent)
     (for ([start-θ (in-list (map car arc-angles))]
           [end-θ (in-list (map cadr arc-angles))])
       (send dc draw-arc dx dy diameter diameter 
             start-θ end-θ))
     
     (send dc set-brush brush))
   diameter diameter))

(define (red-dots diameter dots)
  (dc 
   (λ (dc dx dy)
     (define brush (send dc get-brush))
     (send dc set-brush "black" 'transparent)
     
     (for ([θ (in-list (map car dots))])
       (define size 16)
       (i:render-image bug-image dc 
                       (+ dx (* diameter 1/2) (* diameter 1/2 (cos θ)) (- (/ bug-w 2)))
                       (+ dy (* diameter 1/2) (* diameter 1/2 (sin θ)) (- (/ bug-h 2)))))
     
     (send dc set-brush brush))
   diameter diameter))

(define (white-around p)
  (cc-superimpose
   (colorize (filled-rectangle (+ 4 (pict-width p))
                               (+ 25 (pict-height p)))
             "white")
   p))

(define (lifecycle)
  (define (add-title p n)
    (ct-superimpose p
                    (vl-append
                     (blank 0 20)
                     (cellophane 
                      (scale/improve-new-text 
                       (t "The Semantics Lifecycle")
                       2)
                      (sqr (sqr n))))))
  
  (define (animate n)
    (slide-pict (cc-superimpose (add-title (blank client-w client-h) n)
                                (colorize lifecycle-placement2 "red")
                                ((if (= n 1) ghost values)
                                 (scale items (- 1 (* n .95)))))
                (scale (nth-lifecycle 0) (+ (* n .8) .2))
                lifecycle-placement
                lifecycle-placement2
                n))
  
  (cond
    [printing? 
     (slide (animate 0))
     (slide (animate 1))]
    [else
     (play-n #:steps 20 animate)])
  
  (for ([x (in-range 1 (+ (length dots) 1))])
    (slide
     #:layout 'center
     (add-title (cc-superimpose (nth-lifecycle x) (blank client-w client-h))
                1))))

(define (nth-lifecycle x)
  (define-values (left-side right-side) 
    (split-at
     (map (λ (line) (apply vc-append (map t (cdr line))))
          dots)
     (round (/ (length dots) 2))))
  (define-values (left-ghosts right-ghosts)
    (split-at 
     (for/list ([y (in-range 0 (length dots))])
       (if (<= x y)
           ghost
           values))
     (round (/ (length dots) 2))))
  (hc-append
   20 
   (apply vc-append 30 (map (λ (f x) (f x)) left-ghosts left-side))
   (scale/improve-new-text
    (inset (panorama
            (scale
             (spaced-out (white-around (vc-append (t "Prototype")
                                                  (t "model")))
                         -0.4
                         (white-around (vc-append (t "Robust")
                                                  (t "model")))
                         -0.35
                         (white-around (vc-append (t "Write-up")))
                         -0.25
                         
                         (take dots x))
             .4))
           0 0 6 6)
    3.5)
   (apply vc-append 30 (map (λ (f x) (f x)) right-ghosts right-side))))

(define lifecycle-placement (scale (launder (ghost (nth-lifecycle 0))) .2))
(define lifecycle-placement2 (launder (ghost (nth-lifecycle 0))))

(define items
  (vl-append
   40
   (para "A niche for mechanized metatheory:")
   (hbl-append (blank 40 0)
               (item "lightweight: high level of expressiveness (think scripting language)"))
   (hbl-append (blank 40 0)
               (item "supports the entire semantics lifecycle:" 
                     (let ([p (ghost (t "y"))])
                       (refocus (lc-superimpose p lifecycle-placement) p))))))
