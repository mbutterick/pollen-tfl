<!DOCTYPE html>
◊(define parent-page (parent here))
◊(define previous-page (previous here))
◊(define next-page (next here))
◊(define here-title (or (select-from-metas 'title here) (symbol->string here)))
◊(define toolbar? (not (select-from-metas 'toolbar-blank metas)))

◊(define (make-side-nav id url text)
  ◊div[#:class "nav-outer" #:id id]{◊(link (or url "") ◊div[#:class "nav-inner"]{◊div[#:class "nav-flex" text]})})
◊(define center-cell-width 14)
◊(define side-cell-width (/ (- 100 (+ 10 (* center-cell-width 2))) 2))
◊(local-require pollen/tag)
◊; the name `link` is already defined as a function that makes hyperlinks, 
◊; so we use `make-default-tag-function` to make a literal `link` tag
◊(define literal-link (make-default-tag-function 'link)) 
◊(define (make-subnav children)
  (apply ul #:class "subnav"
    (for/list ([child (in-list children)])
      (li (xref (select-from-metas 'title child))))))

◊(local-require css-tools)

<html>
<head>
  <meta charset="UTF-8">
    <script type="text/javascript">
if (navigator.appVersion.indexOf("Win")!=-1) {
    ◊; got windows
    document.write('<link rel="stylesheet" type="text/css" media="all" href="fonts/source-serif-a.css" />');
} else if (navigator.appVersion.indexOf("Mac")!=-1) {
    if (navigator.userAgent.match(/iPad/i) != null) {
        ◊; got ipad
        ◊; style sheet for ipad 2
        document.write('<link rel="stylesheet" media="only screen and (max-device-width: 1024px)" href="fonts/source-serif-b.css" type="text/css" />');
        ◊; style sheet for ipad 3
        document.write('<link rel="stylesheet" media="only screen and (min-device-width: 768px) and (max-device-width: 1024px) and (-webkit-min-device-pixel-ratio: 2)" type="text/css" href="fonts/source-serif-a.css" />');
    } else {
        ◊; got mac
        document.write('<link rel="stylesheet" type="text/css" media="all" href="fonts/source-serif-b.css" />');
    }
} else {
    ◊; got something else
    document.write('<link rel="stylesheet" type="text/css" media="all" href="fonts/source-serif-a.css" />');
}

</script>

  <title>◊(capitalize-first-letter here-title) | Typography for Lawyers</title>
  <link rel="stylesheet" type="text/css" media="all" href="/styles.css" />
  <link rel="stylesheet" type="text/css" media="all" href="/fonts/non-equity.css" />
  <meta name="format-detection" content="telephone=no">

<script type="text/javascript">
var isFirefox = typeof InstallTrigger !== 'undefined';
if (isFirefox) {
document.write('<link rel="stylesheet" type="text/css" media="all" href="/firefox.css" />');
}
var maybe_ie_ua = window.navigator.userAgent;
 // IE 10
// maybe_ie_ua = 'Mozilla/5.0 (compatible; MSIE 10.0; Windows NT 6.2; Trident/6.0)';

// IE 11
// maybe_ie_ua = 'Mozilla/5.0 (Windows NT 6.3; Trident/7.0; rv:11.0) like Gecko';

// 'MSIE' key detects IE10 and before; 'Trident' key detects IE11
if (maybe_ie_ua.indexOf('MSIE ') > 0 || maybe_ie_ua.indexOf('Trident/') > 0) {
  document.write('<link rel="stylesheet" type="text/css" media="all" href="/ie.css" />');
}

</script>

<script type="text/javascript">
  function jumpToCode() {
    code_value = document.forms['codeform'].elements['codefield'].value;
    window.location.href = "http://typo.la/" + code_value;
    return false;
  }
</script>
◊(when/block (hash-ref metas 'tfl-font-template #f)
    ◊string-append{
<script src="https://checkout.stripe.com/checkout.js"></script>
<script src="https://mbtype.com/core/checkout.js"></script>})

</head>

◊(define (empty-string) "")

◊(define (default-body)
    ◊body{  ◊; use this body for all other pages
      ◊div[#:id "top-stripe"]{}
      ◊div[#:id "content"]{
        ◊doc
        ◊(gap 1)
        ◊(make-subnav (or (children here) null))}

      ◊(if previous-page ◊make-side-nav["prev" previous-page]{<} "")
      ◊(if next-page ◊make-side-nav["next" next-page]{>} "")

      ◊;<!-- bottom nav -->
      ◊(if (not toolbar?) 
        (empty-string)
        ◊div[#:class "nav-outer" #:id "bottom"]{
            ◊div[#:class "nav-inner"]{
                  ◊span[#:id "left"]{◊(if (eq? here 'toc.html)
                    ◊xref["index.html"]{home} 
                    ◊xref{◊(select 'title previous-page)})}
                    ◊span{◊xref["/index.html"]{TFL home}}
                    ◊span[#:id "right"]{◊(if next-page ◊xref{◊(select 'title next-page)} ◊xref["https://www.google.com/search?q=boxer+puppies&safe=off&tbm=isch"]{boxer puppies})}}})})

◊(->html (body (default-body)))
                      

</html>
<!-- © 2008–2017 Matthew Butterick · website made with Pollen (pollenpub.com) -->