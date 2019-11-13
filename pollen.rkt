#lang pollen/mode racket/base
(require "scribblings/pollen-rkt.scrbl" pollen/tag)
(provide (all-from-out "scribblings/pollen-rkt.scrbl"))

(module setup racket/base
  (provide (all-defined-out)) ;; <- don't forget this line in your config submodule!
  (require pollen/setup racket/path)
  (define (omitted-path? p) (path-has-extension? p #"sh")))

(provide ie-payment-warning)
(define (ie-payment-warning)
  (define div (default-tag-function 'div))
  (define p (default-tag-function 'p))
  (define strong (default-tag-function 'strong))
  ◊div[#:class "ie reader-note"]{◊p{Because of security considerations, my payment links ◊strong{do not support Internet Explorer 11 or earlier}. Please use a different browser.}})