#lang pollen
◊(require pollen/template)

◊(define-meta tfl-font-template "true")
◊(define-meta title "Equity")

◊margin-note{
◊div[#:style "text-align:center"]{
◊link["http://typo.la/eqts" #:class 'pdf]{◊image{equity-type-specimen.png}}

◊link["http://typo.la/eqts" #:class 'buylink]{PDF specimen}

◊link["https://mbtype.com/fonts/equity/" #:class 'buylink]{Web demo}
}}


◊mb-font-specimen{◊span[#:style "font-family:equity-caps"]{WE THE PEOPLE OF THE UNITED}
States, in Order to form a more perfect Union,
establish Justice, insure domestic Tranquility,
provide for the common Defense, promote the 
◊em{general Welfare, and secure the Blessings of}
◊strong{Liberty to ourselves and our Posterity.}
If you like, you can edit this paragraph.}

◊make-buy-table[#:people '(1 2 5) #:skus (list 
equity 
equity-concourse-basic 
equity-concourse-standard 
equity-concourse-triplicate
equity-concourse-triplicate-advocate)]

◊(ie-payment-warning)

◊font-details{Equity includes 72 font files:
= 6 styles (regular, italic, bold, bold italic, regular caps, bold caps)
× 2 weight grades (A and B)
× 2 variants (regular and Tab, with tabular figures as the defaults)
× 3 file formats (OpenType, TrueType-compatible OpenType TT, and WOFF)

Read the ◊link["https://mbtype.com/license"]{font license} (it’s short) or the ◊link["https://mbtype.com/faq"]{FAQ}
For details on character set and OpenType features, visit ◊link["https://mbtype.com/fonts/equity/"]{MB Type}
For more than five people, visit ◊link["https://mbtype.com/fonts/equity/buy.html"]{MB Type}
}

◊numbered-list{
A workhorse serif font for ◊xref{body text}.


Fits as many words on the page as ◊xref{Times New Roman}, and stays legible down to small ◊xref{point sizes}.


Designed to perform well on both high-end output devices and office printers.


Comes with a separate set of caps fonts, which contain real ◊xref{small caps} and already include my recommended ◊xref{letterspacing}.


Short, plain-English license.


30-day return option.
}
