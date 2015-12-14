#lang info
(define collection "pollen-tfl")
(define scribblings '())
(define deps '("base" "pollen" "hyphenate" "css-tools" "txexpr" "sugar"))
(define build-deps '("txexpr" "sugar" "rackunit-lib"))
(define update-implies '("txexpr" "hyphenate" "sugar"))
(define compile-omit-paths 'all)
(define test-omit-paths (list #rx"\\.(pm|ptree|pp)$"))
