<!DOCTYPE html>
◊(define parent-page (parent here))
◊(define previous-page (previous here))
◊(define next-page (next here))
◊(define here-title (or (select-from-metas 'title here) (symbol->string here)))
◊(define toolbar? 
  (and 
    (not (select-from-metas 'redirect-to metas))
    (not (select-from-metas 'toolbar-blank metas))))
◊(define bumper? 
  (and 
    (not (select-from-metas 'redirect-to metas))
    (not (select-from-metas 'bumper-blank metas))))

◊(define (make-side-nav id url text)
  ◊div[#:class "nav-outer" #:id id]{◊(link (or url "") ◊div[#:class "nav-inner"]{◊div[#:class "nav-flex" text]})})
◊(define center-cell-width 14)
◊(define side-cell-width (/ (- 100 (+ 10 (* center-cell-width 2))) 2))
◊(local-require pollen/tag)
◊; the name `link` is already defined as a function that makes hyperlinks, 
◊; so we use `make-default-tag-function` to make a literal `link` tag
◊(define literal-link (make-default-tag-function 'link)) 
◊(define (capitalize-first-letter str) (regexp-replace #rx"^." str string-upcase))

◊(define (make-subnav children)
  (apply ul #:class "subnav"
    (for/list ([child (in-list children)])
      (li (xref (select-from-metas 'title child))))))

