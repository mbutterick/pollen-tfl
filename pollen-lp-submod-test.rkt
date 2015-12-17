#lang racket
(module test racket
  (require rackunit txexpr "pollen-lp.scrbl")
  (#reader scribble/reader
   ;; always include this at the start of the test submodule
  (check-txexprs-equal? (link "http://foo.com" "link text")
                        '(a ((href "http://foo.com")) "link text"))
  (check-txexprs-equal? @link["http://foo.com"]{link text}
                        '(a ((href "http://foo.com")) "link textz"))))