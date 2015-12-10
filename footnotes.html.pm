#lang pollen

◊(define-meta title "footnotes")
◊hanging-topic[(topic-from-metas metas)]{Mind the separators and alignment}

Footnotes are included with the advanced layout topics not because they’re typographically difficult, but because prying their typography from the maw of your word processor can be a chore. If you’re undaunted, the default formatting of footnotes is flawed in several ways that are worth fixing:

◊numbered-list{
The footnote separator is a horizontal line. Delete the line and just use some extra white space. (It’s fine, however, to use a line separator for a footnote continuation.)


Footnote-reference numbers in the ◊xref{body text} are set in the same font, just smaller and superscripted. These numbers should be noticeable despite their size. If they don’t stand out enough, try making them bold, or even running them in a different font (a sans serif, perhaps).


The reference numbers on the footnotes are also shrunken superscripts. There’s no need for this. They can be the same size as the rest of the footnote text.


The reference numbers on the footnotes are indented. I prefer footnotes to have a negative ◊xref{first-line indent} so that the reference number is aligned to the left edge of the text block, and then all the lines of the footnote can be indented the same amount. (An effect demonstrated in this paragraph.)

}