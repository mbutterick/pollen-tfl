#lang pollen

◊(define-meta title "keep lines together")
◊hanging-topic[(topic-from-metas metas)]{Always use with headings}

◊em{Keep lines together} ensures that all lines in a paragraph appear on the same page. If the last line of the paragraph won’t fit on the current page, the whole paragraph gets moved to the next page.

Use this option with ◊xref{headings} to prevent them from starting at the bottom of one page and continuing at the top of the next. That looks bad.

Like ◊xref{widow and orphan control}, keeping lines together will create gaps at the bottom of pages. But unlike widow and orphan control, you only want to keep lines together in special situations, not as part of your default text formatting.