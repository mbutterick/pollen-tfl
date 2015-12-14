#lang info
(define collection "pollen-tfl")
(define scribblings '())
(define deps '("base" "pollen" "hyphenate" "css-tools" "txexpr" "sugar"))
(define build-deps '("txexpr" "sugar" "rackunit-lib"))
(define update-implies '("txexpr" "hyphenate" "sugar"))
(define test-omit-paths '(all))
