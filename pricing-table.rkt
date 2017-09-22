#lang racket/base
(require racket/dict "sku.rkt")
(provide (all-defined-out) (all-from-out "sku.rkt"))


(define (round-cents float)
  (/ (floor (* float 100)) 100))

(define (round-up inc x)
  (* (ceiling (/ x inc)) inc))


(define (calc-multi-price base-price people)
  (round-cents 
   (sub1 (round-up 10 (case people
                        [(1) base-price]
                        [(2) (/ (* base-price 4) 3)]
                        [(5) (* base-price 2)]
                        [(10) (* base-price 3)]
                        [(20) (* base-price 4)]
                        [(40) (* base-price 5)]
                        [(60) (* base-price 6)]
                        [(80) (* base-price 7)]
                        [else (error "Too many people")])))))



(define (calc-multi-license sku people)
  (define base-price (sku-base-price sku))
  (calc-multi-price base-price people))

(define license-increments '(1 2 5 10 20 40 60 80))

(define (make-price-list base-price)
  (map (λ(n) (cons n (calc-multi-license base-price n))) license-increments))

(define (get-price sku [people 1])
  (with-handlers ([exn:fail? (λ(e) (get-price sku (add1 people)))])
    (calc-multi-license sku people)))

(define (get-variant-id sku which)
  (dict-ref (sku-variant-ids sku) which))

(define (make-buy-url sku which)
  (format "http://typo.la/bc.html?item=~a" (get-variant-id sku which)))


(define (grid->table grid)
  (define (table-row row [cell-tag 'td])
    `(tr (th ((style "width:40%")) ,(car row)) ,@(map (λ(c) `(,cell-tag ,c)) (cdr row))))
  (define (table-header row)
    (table-row row 'th))
  
  `(table ((class "buy-table"))
    ,(table-header (car grid))
    ,@(map table-row (cdr grid))))

(define (people->string p)
  (define p-string (format "~a"
                           (if (< p 10)
                               (list-ref '(zero one two three four five six seven eight nine) p)
                               p)))
  (string-append p-string " " (if (= p 1) "person" "people")))

(define (make-buy-grid #:people people-list #:skus sku-list)
  (cons 
   (cons "" (map people->string people-list)) 
   (map (λ(sku) (cons (sku-name sku) 
                      (map (λ(p) `(a ((href ,(make-buy-url sku p))) ,(format "$~a" (get-price sku p)))) people-list))) sku-list)))


(define (make-buy-table #:people people-list #:skus sku-list)
  (grid->table (make-buy-grid #:people people-list #:skus sku-list)))
