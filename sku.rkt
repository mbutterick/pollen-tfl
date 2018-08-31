#lang racket/base
(provide (all-defined-out))

;; name = xexpr in table
;; base price = what it sounds like
;; variant-ids = pairs of (people . big cartel id)
(struct sku (name base-price variant-ids))

(define concourse (sku "Concourse" 150 '((1 . 25256145) (2 . 25256442) (5 . 25256445))))

(define hermes-maia (sku "Hermes Maia" 150 '((1 . 25256145) (2 . 25256442) (5 . 25256445))))

(define triplicate (sku "Triplicate" 100 '((1 . 64167442) (2 . 64167463) (5 . 64167466))))

(define advocate (sku "Advocate" 100 '((1 . 102904009) (2 . 102904012) (5 . 102904015))))

(define equity (sku "Equity" 120 '((1 . 14855773) (2 . 21635815) (5 . 14855775))))
(define valkyrie (sku "Valkyrie" 120 '((1 . 14855773) (2 . 21635815) (5 . 14855775))))
(define century-supra (sku "Century Supra" 120 '((1 . 14855773) (2 . 21635815) (5 . 14855775))))

(define text-font+concourse-discount 30)

(define equity-concourse (sku '(span (a ((href "equity.html")) "Equity") " +" nbsp (a ((href "concourse.html")) "Concourse")) (- (+ (sku-base-price equity) (sku-base-price concourse)) text-font+concourse-discount) '((1 . 25256655) (2 . 25256754) (5 . 25256757))))

(define valkyrie-concourse (sku '(span (a ((href "valkyrie.html")) "Valkyrie") " +" nbsp (a ((href "concourse.html")) "Concourse")) (- (+ (sku-base-price equity) (sku-base-price concourse)) text-font+concourse-discount) '((1 . 25256655) (2 . 25256754) (5 . 25256757))))

(define century-supra-concourse (sku '(span (a ((href "century-supra.html")) "Century Supra") " +" nbsp (a ((href "concourse.html")) "Concourse")) (- (+ (sku-base-price equity) (sku-base-price concourse)) text-font+concourse-discount) '((1 . 25256655) (2 . 25256754) (5 . 25256757))))

(define text-font+3-discount 110)
(define equity-concourse-triplicate-advocate (sku '(span (a ((href "equity.html")) "Equity") " +" nbsp (a ((href "concourse.html")) "Concourse") " +" nbsp (a ((href "triplicate.html")) "Triplicate") (br) " +" nbsp (a ((href "advocate.html")) "Advocate")) (- (apply + (map sku-base-price (list equity concourse triplicate advocate))) text-font+3-discount) '((1 . 102904795)(2 . 102904798)(5 . 102904801))))

(define century-supra-concourse-triplicate-advocate (sku '(span (a ((href "century-supra.html")) "Century Supra") " +" nbsp (a ((href "concourse.html")) "Concourse") " +" nbsp (a ((href "triplicate.html")) "Triplicate") (br) " +" nbsp (a ((href "advocate.html")) "Advocate")) (- (apply + (map sku-base-price (list equity concourse triplicate advocate))) text-font+3-discount) '((1 . 102904795)(2 . 102904798)(5 . 102904801))))

(define valkyrie-concourse-triplicate-advocate (sku '(span (a ((href "valkyrie.html")) "Valkyrie") " +" nbsp (a ((href "concourse.html")) "Concourse") " +" nbsp (a ((href "triplicate.html")) "Triplicate") (br) " +" nbsp (a ((href "advocate.html")) "Advocate")) (- (apply + (map sku-base-price (list equity concourse triplicate advocate))) text-font+3-discount) '((1 . 102904795)(2 . 102904798)(5 . 102904801))))