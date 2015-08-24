#lang racket/base
(require racket/gui/base
         racket/class)

(provide red)
(define red (make-object color% #xcc #x33 #xcc))

(provide light-red)
(define light-red (make-object color% #xff #x00 #xcc))

(provide navy-blue)
(define navy-blue "darkblue")