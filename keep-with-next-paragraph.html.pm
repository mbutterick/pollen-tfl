#lang pollen

◊(define-meta title "keep with next paragraph")
◊hanging-topic[(topic-from-metas metas)]{Always use with headings}

◊em{Keep with next paragraph} binds the last line of a paragraph to the first line of the next. It ensures that no page break occurs between the two paragraphs. It’s like ◊xref{keep lines together}, except it works between paragraphs instead of within a paragraph.

Always use this option with ◊xref{headings}. It looks bad if a heading appears at the bottom of a page and the text it’s introducing starts on the next page. Keeping with the next paragraph prevents this.