#lang pollen

◊(define-meta title "page break before")
◊hanging-topic[(topic-from-metas metas)]{Alternative to hard page breaks}

◊em{Page break before} forces a paragraph to start at the top of a new page. Visually, there’s no difference between using the page-break-before option and typing a ◊xref{hard page break} in front of the paragraph. But that’s only efficient for the occasional paragraph.

The page-break-before option is intended to be incorporated into ◊xref{paragraph and character styles} so all paragraphs of a particular style will start at the top of a new page. For instance, you might apply it to your top-level heading style. In a long document, typing hard page breaks in front of each heading would be tedious.