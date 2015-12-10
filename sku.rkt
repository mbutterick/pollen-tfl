#lang racket/base
(provide (all-defined-out))

;; name = xexpr in table
;; base price = what it sounds like
;; variant-ids = pairs of (people . big cartel id)
(struct sku (name base-price variant-ids))
(define equity (sku "Equity" 120 '((1 . 14855773) (2 . 21635815) (5 . 14855775))))

(define concourse-basic (sku "Concourse Basic" 100 '((1 . 25255860) (2 . 25255890) (5 . 25255893))))

(define concourse-standard (sku "Concourse Standard" 180 '((1 . 25256145) (2 . 25256442) (5 . 25256445))))

(define equity-concourse-basic (sku '(span (a ((href "equity.html")) "Equity") " + " (a ((href "concourse.html")) "Concourse Basic")) (- (+ (sku-base-price equity) (sku-base-price concourse-basic)) 20) '((1 . 25256532) (2 . 25256619) (5 . 25256622))))

(define equity-concourse-standard (sku '(span (a ((href "equity.html")) "Equity") " + " (a ((href "concourse.html")) "Concourse Standard")) (- (+ (sku-base-price equity) (sku-base-price concourse-standard)) 60) '((1 . 25256655) (2 . 25256754) (5 . 25256757))))

(define triplicate (sku "Triplicate" 90 '((1 . 64167442) (2 . 64167463) (5 . 64167466))))

(define equity-concourse-triplicate (sku '(span (a ((href "equity.html")) "Equity") " + " (a ((href "concourse.html")) "Concourse Standard") " + " (a ((href "triplicate.html")) "Triplicate")) (- (apply + (map sku-base-price (list equity concourse-standard triplicate))) 90) '((1 . 64167679) (2 . 64167682) (5 . 64167685))))

(define advocate (sku "Advocate" 100 '((1 . 102904009) (2 . 102904012) (5 . 102904015))))

(define equity-concourse-triplicate-advocate (sku '(span (a ((href "equity.html")) "Equity") " +" nbsp (a ((href "concourse.html")) "Concourse Standard") " +" nbsp (a ((href "triplicate.html")) "Triplicate") (br) " +" nbsp (a ((href "advocate.html")) "Advocate")) (- (apply + (map sku-base-price (list equity concourse-standard triplicate advocate))) 130) '((1 . 102904795)(2 . 102904798)(5 . 102904801))))