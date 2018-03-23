#lang pollen
◊(require pollen/template)

◊(define-meta tfl-font-template "true")
◊(define-meta title "Advocate")

◊margin-note{
◊div[#:style "text-align:center"]{
◊link["http://typo.la/ats" #:class 'pdf]{◊image{advocate-type-specimen.png}}

◊link["http://typo.la/ats" #:class 'buylink]{PDF specimen}

◊link["https://mbtype.com/fonts/advocate/" #:class 'buylink]{Web demo}

}}


◊mb-font-specimen{◊div[#:style "line-height:1.1;margin-top:-0.5rem"]{◊span[#:style "font-family:'advocate-c43';font-size:115%"]{THE SENATORS AND REPRESENTATIVES 
shall receive a Compensation for their
Services, to be ascertained by Law, and
◊strong{paid out of the Treasury of the United
States. They shall in all Cases, except}
◊span[#:style "font-family:'advocate-slab-c43'"]{Felony, be privileged from Arrest.}
◊span[#:style "font-family:'advocate-c45'"]{You can edit this paragraph.}}}}



◊make-buy-table[#:people '(1 2 5) #:skus (list 
advocate
equity-concourse-triplicate-advocate)]

◊(ie-payment-warning)

◊font-details{Advocate includes 270 font files:
= 3 weights (regular, medium, bold)
× 3 widths (narrow, regular, wide)
× 2 series (sans, slab)
+ 12 cloned styles
× 3 stylistic variants (normal, tab, mid)
× 3 file formats (OpenType, TrueType-compatible OpenType TT, and WOFF)

Read the ◊link["http://mbtype.com/license"]{font license} (it’s short) or the ◊link["http://mbtype.com/faq"]{FAQ}
For details on character set and OpenType features, visit ◊link["https://mbtype.com/fonts/advocate/"]{MB Type}
For more than five people, visit ◊link["https://mbtype.com/fonts/advocate/buy.html"]{MB Type}
}


◊numbered-list{
An assertive display face for ◊xref{letterhead}, logos, and titles.


Three widths and three weights, with sans and slab serif versions.


Short, plain-English license.


30-day return option.
}
