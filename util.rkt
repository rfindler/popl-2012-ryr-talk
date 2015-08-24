#lang racket

(require slideshow/pict
         racket/gui/base)
(provide to-bitmap
         evenize)

(define (to-bitmap p [bgcolor #f] #:no-alpha? [no-alpha? #f])
  (define w (inexact->exact (ceiling (pict-width p))))
  (define h (inexact->exact (ceiling (pict-height p))))
  (define bm (make-screen-bitmap w h))
  (define bdc (make-object bitmap-dc% bm))
  (when bgcolor 
    (send bdc set-brush bgcolor 'solid)
    (send bdc draw-rectangle 0 0 w h))
  (send bdc set-smoothing 'aligned)
  (draw-pict p bdc 0 0)
  (send bdc set-bitmap #f)
  (dc 
   (cond
     [no-alpha?
      (define bm2 (make-bitmap w h #f))
      (send bdc set-bitmap bm2)
      (send bdc draw-bitmap bm 0 0)
      (send bdc set-bitmap #f)
      (λ (dc dx dy)
        (send dc draw-bitmap bm2 dx dy))]
     [else
      (λ (dc dx dy)
        (send dc draw-bitmap bm dx dy))])
      w h))

(define (evenize p)
 (define (delta v)
   (let ([v2 (ceiling v)])
     (- v2
        (if (even? v2) -1 0)
        v)))
 (inset p 0 0 (delta (pict-width p)) (delta (pict-height p))))
