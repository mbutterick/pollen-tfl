#lang pollen

◊(define-meta title "paragraph and section marks")
◊hanging-topic[(topic-from-metas metas)]{Add a nonbreaking space}


The ◊em{paragraph mark} ◊glyph{¶} is used when citing documents with sequentially numbered paragraphs (e.g., declarations or complaints).

The ◊em{section mark} ◊glyph{§} is used when citing documents with numbered or lettered sections (e.g., statutes).

A paragraph mark or section mark should always be followed by a nonbreaking space. The ◊xref["nonbreaking-spaces.html"]{nonbreaking space} acts like glue that keeps the mark joined with the numeric reference that follows.

Without the nonbreaking space, the mark and the reference can end up on separate lines or pages. This can confuse readers.

◊captioned["wrong"]{The defendant has the option under Civil Code §
1782 to offer a correction to affected buyers. But ¶
17 of the agreement suggests it is required.}

◊captioned["right"]{The defendant has the option under Civil Code
§ 1782 to offer a correction to affected buyers. But
¶ 17 of the agreement suggests it is required.}


If the paragraph or section reference comes at the start of a sentence, don’t use the mark — spell out the whole word (◊em{Section 17200 applied to the transaction, but § 17500 did not}). In a reference to multiple paragraphs or sections, double the mark ◊glyph{¶¶ or §§}.