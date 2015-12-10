#lang pollen

◊(define-meta title "monospaced fonts")
◊hanging-topic[(topic-from-metas metas)]{Don't use these either}

The ◊xref{system fonts} Courier, Monaco, and Consolas are examples of ◊em{monospaced fonts}, so named because every character is the same width. When the characters vary in width, the font is called ◊em{proportional}.

◊indented[#:hyphenate #f]{◊font-scale[1.6]{
◊mono{abcdefghijklmnopqrstuvwxyz!}
abcdefghijklmnopqrstuvwxyz!
◊; next two lines contain nonbreaking spaces
◊mono{Jill, did you buy the milk?} 
Jill, did you buy the milk?}}

The samples above are set at the same point size. But the monospaced font (first and third rows) takes up more horizontal space than the proportional font (second and fourth rows). The differences are most noticeable in characters that are narrow in the proportional font (like f i j l r t) and the punctuation characters.

Monospaced fonts were invented to meet the mechanical requirements of typewriters. They were not invented to win beauty contests. Compared to proportional fonts, monospaced fonts are harder to read. And because they take up more horizontal space,  you’ll always get fewer words per page with a monospaced font.

There are no good reasons to use monospaced fonts. So don’t. Use proportional fonts.