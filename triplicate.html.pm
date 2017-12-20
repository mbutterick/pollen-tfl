#lang pollen
◊(require pollen/template)

◊(define-meta tfl-font-template "true")
◊(define-meta title "Triplicate")

◊margin-note{
◊div[#:style "text-align:center"]{
◊type-specimen{◊link["http://typo.la/trts" #:class 'pdf]{◊image{triplicate-type-specimen.png}}}
              
◊link["http://typo.la/trts" #:class 'buylink]{PDF specimen}
}}


◊mb-font-specimen{◊div[#:style "line-height:1.2;margin-top:-0.5rem"]{◊span[#:style "font-family:'triplicate-t4';font-size:85%"]{EACH HOUSE SHALL BE THE JUDGE OF THE
Elections, Returns and Qualifications 
of its own Members, and a Majority of 
◊em{each shall constitute a Quorum to do} 
◊strong{Business; but a smaller Number may} 
adjourn from day to day. If you like, 
you can edit this paragraph.}}}



◊make-buy-table[#:people '(1 2 5) #:skus (list 
triplicate
equity-concourse-triplicate
equity-concourse-triplicate-advocate)]


◊font-details{Triplicate includes 144 font files:
= 3 weights (light, book, bold)
× 3 series (roman, italic, caps)
+ 3 cloned styles
× 4 variants (regular, code, poly, short)
× 3 file formats (OpenType, TrueType-compatible OpenType TT, and WOFF)

Read the ◊link["http://mbtype.com/license"]{font license} (it’s short) or the ◊link["http://mbtype.com/faq"]{FAQ}
For details on character set and OpenType features, see the ◊link["http://typo.la/trts"]{PDF specimen}
For more than five people, email ◊link["mailto:mb@mbtype.com"]{mb@mbtype.com}}


◊numbered-list{
A ◊xref{monospaced font} that's more readable and useful than Courier.


Three weights, with true italics, real ◊xref{small caps}, oldstyle figures, swash caps, box-drawing characters, and other superfluities.


Short, plain-English license.


30-day return option.
}


