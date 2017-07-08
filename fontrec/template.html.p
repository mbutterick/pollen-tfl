<!DOCTYPE html>
<html>
◊(define here-title (or (select-from-metas 'title here) (symbol->string here)))

<head>
    <meta charset="UTF-8" />
    <title>◊|here-title| | Typography for Lawyers</title>
    <link rel="stylesheet" type="text/css" media="all" href="/styles.css" />
    <link rel="stylesheet" type="text/css" media="all" href="/fonts/non-equity.css" />
</head>


◊(local-require pollen/pagetree txexpr)
◊(define fontrec-pagetree "index.ptree")
◊(current-pagetree (load-pagetree fontrec-pagetree))
◊(define here-path (hash-ref metas 'here-path))
◊(define here-fontrec (path->pagenode here-path fontrec-pagetree))
◊(define child-samples (children here-fontrec))
◊(define page-title (format "~a~a" (select-from-metas 'title metas) (if child-samples " alternatives" "")))

<body>
<div id="content">
    ◊(->html 
        (make-txexpr (get-tag doc)
        (cons (class "font-sample") (get-attrs doc))
        (cons (h3 page-title) (get-elements doc))))

◊(->html (gap 1))


◊when/block[child-samples]{
◊(->html `(ul ((class "font-alternatives")) ,@(map (λ(child) (define title (or (select-from-metas 'title (->complete-path child)) child)) `(li ,(xref (->string title)))) child-samples)))}

</div> ◊; close "content" div

</body>
</html>
<!-- © 2015 Matthew Butterick · website made with Pollen (pollenpub.com) -->