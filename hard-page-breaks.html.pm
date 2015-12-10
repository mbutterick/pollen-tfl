#lang pollen

◊(define-meta title "hard page breaks")
◊hanging-topic[(topic-from-metas metas)]{Move to the top of the next page}

The ◊em{hard page break} puts the next word at the top of a new page.

To move text to the next page, use one hard page break, not multiple ◊xref{carriage returns}. If you use carriage returns, your document will become impossible to edit — as soon as you change anything before the page break, the text will go out of alignment. The hard page break guarantees consistent behavior.