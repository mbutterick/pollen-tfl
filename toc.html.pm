#lang pollen
◊(require pollen/pagetree pollen/template sugar/coerce)

◊(define-meta title "table of contents & excerpts")
◊(section-from-metas metas)

◊(let () (current-pagetree (load-pagetree "index.ptree")) "")

◊(define (node->link node #:capitalize [caps? #f])
    (define node-string (->string node))
    (define link-name
       (let* ([name (if (dev-mode?) 
                       node-string
                       (select-from-metas 'title node))]
             [name (if caps? (capitalize-first-letter name) name)])
         name))
   ◊link[node-string]{◊link-name})

◊(define (make-toc-subsection pagenode)
  (define node-children (children pagenode))
  ◊div{
    ◊h3{◊(node->link pagenode #:capitalize #t)}
    ◊(if node-children
      (apply ul (map (compose1 li node->link) node-children))
      "")})

◊(apply div #:class "toc" 
  (map make-toc-subsection '(foreword.html 
  introduction.html
  why-typography-matters.html
  type-composition.html
  text-formatting.html
  font-recommendations.html
  a-brief-history-of-times-new-roman.html 
  page-layout.html
  sample-documents.html
  appendix.html
  about.html)))