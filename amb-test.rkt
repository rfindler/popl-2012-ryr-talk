#lang racket
(require redex "amb.rkt")

(test-->> red 
          (term ((+ 1 2 3)))
          (term (6)))
(test-->> red 
          (term ((+ 1 (+ 2 3))))
          (term (6)))
(test-->> red 
          (term ((+ (+ 1 2) 3)))
          (term (6)))
(test-->> red
          (term ((amb (+ 1 2) 4)))
          (term (3 4)))
(test-->> red
          (term ((+ 1 (amb 1 2))))
          (term (2 3)))
(test-->> red
          (term ((((λ ((x num)) (λ ((y num)) x)) 1) 2)))
          (term (1)))
(test-->> red
          (term ((((λ ((x num)) (λ ((y num)) (amb x y))) 1) 2)))
          (term (1 2)))
(test-->> red
          (term ((+ (amb (+ 1 2) (+ 3 4.5))
                    (amb (+ 5 6) (+ 7 8)))))
          (term (14 18 18.5 22.5)))

(test-equal (judgment-holds (types (x : num ·) y t_1) t_1)
            (term ()))
(test-equal (judgment-holds (types (x : num ·) x t_1) t_1)
            (term (num)))
(test-equal (judgment-holds (types (x : num (y : (→ num num) ·)) x t_1) t_1)
            (term (num)))
(test-equal (judgment-holds (types (x : num (y : (→ num num) ·)) y t_1) t_1)
            (term ((→ num num))))
(test-equal (judgment-holds (types (x : num (x : (→ num num) ·)) y t_1) t_1)
            (term ()))
(test-equal (judgment-holds (types · (+ 1 2 3) t_1) t_1)
            (term (num)))
(test-equal (judgment-holds (types · (λ ((y num)) y) t_1) t_1)
            (term ((→ num num))))
(test-equal (judgment-holds (types · (λ ((y num) (z num)) y) t_1) t_1)
            (term ((→ num num num))))
(test-equal (judgment-holds (types · (λ ((y num)) (λ ((z num)) y)) t_1) t_1)
            (term ((→ num (→ num num)))))
(test-equal (judgment-holds (types · (((λ ((x num)) (λ ((y num)) x)) 1) 2) t_1) t_1)
            (term (num)))
(test-equal (judgment-holds (types · (amb) t_1) t_1)
            (term (num)))
(test-equal (judgment-holds (types · (amb 1) t_1) t_1)
            (term (num)))
(test-equal (judgment-holds (types · (amb 1 2) t_1) t_1)
            (term (num)))
(test-equal (judgment-holds (types · (+ (amb (+ 1 2) (+ 3 4.5))
                                        (amb (+ 5 6) (+ 7 8)))
                                   t_1)
                            t_1)
            (term (num)))

(test-results)
