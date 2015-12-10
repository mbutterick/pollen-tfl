#lang pollen

◊(define-meta title "hyphenation")
◊hanging-topic[(topic-from-metas metas)]{Mandatory for justified text; optional otherwise}


◊em{Hyphenation} is the automated process of breaking words between lines to create more consistency across a text block.

In ◊xref{justified text}, hyphenation is mandatory.

In left-aligned text, hyphenation evens the irregular right edge of the text, called the rag. Hyphenation is optional for left-aligned text because the rag will still be somewhat irregular, even with hyphenation. Hyphenation doesn’t improve text legibility. Consider turning it off.

As ◊xref{line length} gets shorter, hyphenation becomes essential. Why? With hyphenation off, your word processor can only break lines at word spaces. As the lines get shorter, there are fewer words and hence fewer possible break points in each line, making awkward breaks more likely.