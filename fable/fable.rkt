#lang racket
(require racket/runtime-path
         racket/gui/base
         slideshow/play
         slideshow
         "../color.rkt"
         "../util.rkt")
(define-runtime-path fable.txt "fable.txt")
(define-runtime-path here ".")

(provide fable)

(define orig-csa (current-slide-assembler))

(define (black-assembler title sep p)
  (ct-superimpose (inset (colorize (filled-rectangle 1024 768) "black")
                         (- margin))
                  p))

(define-syntax-rule
  (ns n ...)
  (colorize
   (parameterize ([current-font-size 12])
     (hbl-append (t (format "~s = ~s  " 'n n)) ...))
   "white"))

(define (fable)
  (current-slide-assembler black-assembler)
  
  (cond
    [printing?
     (for ([i (in-range 0 (procedure-arity build-main))])
       (slide
        (apply fable-animation-func (build-list (procedure-arity fable-animation-func)
                                                (λ (j) (if (< i j) 0 1))))))
     (slide (apply fable-animation-func (make-list (procedure-arity fable-animation-func) 1)))]
    [else
     (play-n
      #:steps (build-list 12 (λ (i) (if (= i 11) 50 10)))
      fable-animation-func)])
  
  (current-slide-assembler orig-csa))

(define (fable-animation-func n0 n1 n2 n4 n5 n6 n8 n9 n10 n11 n12 n14)
  (define main
    (if (= n14 1) 
        cached-main
        (build-main n0 n1 n2 n4 n5 n6 n8 n9 n10 n11 n12)))
  (scroll-up-picts
   (list main (cc-superimpose moral (blank 0 (pict-height main))))
   (list n14)))

(define (build-main n0 n1 n2 n4 n5 n6 n8 n9 n10 n11 n12)
  (evenize
   (ct-superimpose
    (title-animation n0)
    ((if (= n0 1) values ghost) 
     (vc-append
      (blank 0 80)
      (cc-superimpose
       (hc-append
        20
        (scroll-up-picts (list (scale (img 'koala) 2/3)
                               (scale (img 'happy-koala) 2/3)
                               (scale (img 'orangutan) 2/3)
                               (scale (img 'happy-orangutan) 2/3)
                               (scale (img 'walrus) 2/3))
                         (list n4 n5 n10 n11))
        (scroll-up-picts (list ftp-example
                               (if (n6 . > . 0)
                                   (colorize (buggy-c-code) "white")
                                   (colorize c-code "white"))
                               (if (n12 . > . 0)
                                   (colorize (latex-code values ghost) "white")
                                   (colorize (latex-code ghost values) "white")))
                         (list n1 n8)))
       ((if (zero? n4) values ghost)
        (blink n2 (img 'duncan)))
       ((if (zero? n10) values ghost)
        (blink n9 (img 'fei-ge))))
      (blank 0 10)
      (lines (parse-fable) (list n1 n2 n4 n5 n6 n8 n9 n10 n11 n12)))))))
  
(define (lines strs ns)
  (define first-zero? #t)
  (apply
   ctl-superimpose
   (for/list ([str (in-list strs)]
              [n (in-list (append ns '(0)))])
     ((cond
        [(and (zero? n) first-zero?)
         (set! first-zero? #f)
         values]
        [else
         ghost])
      (parameterize ([current-font-size 16])
        (colorize (para #:width 900 str) "white"))))))

(define (parse-fable)
  (define sp (open-output-string))
  (call-with-input-file fable.txt
    (λ (port)
      (copy-port port sp)))
  (define fable1 (get-output-string sp))
  (define fable2 (regexp-split #rx"\n\n" fable1))
  (define fable3 (map (λ (x) (regexp-replace* #rx"\n" x " "))
                      fable2))
  (define fable4 (map (λ (x) (regexp-replace* #rx"  +" x " "))
                      fable3))
  ;; drop the title
  (cdr fable4))

(define image-specs
  '(("koala.jpg" 1)
    ("duncan.jpg" 1)
    ("happy-koala.jpg" 1)
    ("orangutan.jpg" 1)
    ("fei-ge.jpg" 1) ; ("lucy-surprise.jpg" 1)
    ("happy-orangutan.jpg" 1)
    ("walrus.jpg" 1)))

(define images (make-hash))

(for ([i (in-list image-specs)])
  (define key (string->symbol (regexp-replace #rx"[.](.*)" (list-ref i 0) "")))
  (hash-set! images
             key
             (scale (bitmap (read-bitmap (build-path here (list-ref i 0))))
                    (list-ref i 1))))
(define (img k) 
  (hash-set! used k #f)
  (hash-ref images k))
(define used (make-hash))

(define image-spacer (ghost (apply cc-superimpose (hash-map images (λ (x y) y)))))

(define (title-animation n0)
  (define start-h (- (/ client-h 2) 
                     (/ (pict-height start-title1) 2)))
  (define end-h 20)
    
  (define-values (title1 title2 title3) (fable-title n0))
  
  (slide-pict
   (slide-pict
    (slide-pict
     (ct-superimpose (vc-append 40
                                (parameterize ([current-font-size end-size])
                                  (hbl-append end-title1
                                              (t " ")
                                              end-title2
                                              (t " ")
                                              end-title3)))
                     (vl-append start-title1
                                start-title2
                                start-title3))
     title3
     start-title3
     end-title3
     n0)
    title2
    start-title2
    end-title2
    n0)
   title1
   start-title1
   end-title1
   n0))

(define start-size 127)
(define end-size 50)


(define (fable-title n) 
  (parameterize ([current-font-size (between start-size end-size n)])
    (values 
     (colorize (t "The Koala,") "white")
     (colorize (t "the Orangutan,") "white")
     (colorize (t "and the Walrus") "white"))))
(define (between start-size end-size n)
  (round (inexact->exact (+ start-size (* n (- end-size start-size))))))

(define (ghost3 f x)
  (define-values (a b c) (f x))
  (values (ghost a) (ghost b) (ghost c)))
(define-values (start-title1 start-title2 start-title3) (ghost3 fable-title 0))
(define-values (end-title1 end-title2 end-title3) (ghost3 fable-title 1.0))

(define c-code
  (vl-append
   (tt "int main () {")
   (tt "  if (!(q = 0))")
   (tt "    *((int*)p)=12;")
   (tt "}")))

(define bad-tau2 (tt "\\tau_2"))

(define (latex-code fix bug)
  (vl-append
   (tt "\\[\\Gamma\\ \\vdash\\")
   (hbl-append (tt "  (\\lambda x:")
               (lbl-superimpose (bug (tt "\\tau_2"))
                                (fix (colorize (tt "\\tau_2") light-red)))
               (tt ".e)"))
   (tt "   : \\tau_1\\rightarrow")
   (tt "     \\tau_2 \\]")))

(define (buggy-c-code)
  (rbl-superimpose
   c-code
   (colorize (tt "p == 0 ∨ *p == *q") light-red)))

(define ftp-example
  (colorize
   (vl-append
    (tt "ftp> user anonymous")
    (tt "331 Guest login ok")
    (tt "Password:")
    (tt "230-Welcome to λ.com"))
   "white"))

(define (pale-behind p)
  (cc-superimpose
   (cellophane (colorize (filled-rectangle (pict-width p) (pict-height p)) "black") .8)
   p))
   

(define (scroll-up-picts ps ns)
  (define bkg (ghost (apply cc-superimpose ps)))
  (define h (pict-height bkg))
  (define uniform-ps (for/list ([p (in-list ps)]) 
                       (cc-superimpose
                        (blank (pict-width bkg) 0)
                        (scale p (/ h (pict-height p))))))
  (define combined (apply vc-append uniform-ps))
  (define x 
    (apply
     +
     (for/list ([p (in-list ps)]
                [n (in-list ns)])
       (* n h))))
  (inset/clip combined 0 (- x) 0 (+ (- x (pict-height combined)) h -1)))

(define (blink n p)
  ((cond
     [(<= n .1) ghost]
     [(<= n .3) values]
     [(<= n .5) ghost]
     [(<= n .7) values]
     [(<= n .9) ghost]
     [else values])
   p))

(define raw-moral 
  (evenize
   (colorize
    (let ([first-line 
           (scale/improve-new-text
            (parameterize ([current-font-size 60])
              (t "Moral: bugs are"))
            2)]
          [second-line-w (pict-width (bt "everywhere"))])
      (vc-append first-line
                 (scale/improve-new-text (bt "everywhere")
                                         (/ (pict-width first-line)
                                            second-line-w))))
    "white")))

(define moral
  (if printing?
      raw-moral
      (to-bitmap raw-moral "black" #:no-alpha? #t)))

(define uncached-main (build-main 1 1 1 1 1 1 1 1 1 1 1))

(define cached-main (to-bitmap uncached-main "black" #:no-alpha? #t))

(module+ slideshow (fable))