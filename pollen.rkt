#lang racket/base
(require "scribblings/pollen-rkt.scrbl")
(provide (all-from-out "scribblings/pollen-rkt.scrbl"))

(module setup racket/base
  (provide (all-defined-out)) ;; <- don't forget this line in your config submodule!
  (require pollen/setup racket/path)
  (define (omitted-path? p) (path-has-extension? p #"sh"))
  (define publish-directory "~/Dropbox/dropbox_xray/typographyforlawyers.com/public/"))