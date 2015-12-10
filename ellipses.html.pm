#lang pollen

◊(define-meta title "ellipses")
◊hanging-topic[(topic-from-metas metas)]{Avoid using periods and spaces}

An ◊em{ellipsis} (plural ◊em{ellipses}) is a sequence of three dots used to indicate an omission in quoted material.

The ellipsis is frequently approximated by typing three periods in a row, which puts the dots too close together, or three periods with spaces in between, which puts the dots too far apart. So use your font’s ellipsis character, not the approximations.

◊(omission)

The problem with using periods and word spaces is that it permits your word processor to break the ellipsis across lines or pages, like so:

◊captioned["wrong"]{◊font-scale[2]{imperative to . . 
. courts}}


◊(omission)

Should you put word spaces around an ellipsis? As with the em dash (see ◊xref{hyphens and dashes}), that’s up to you. Typically you’ll want spaces before and after, but if that looks odd, you can take them out. If there’s text on only one side of the ellipsis, use a ◊xref{nonbreaking space} on that side so the ellipsis doesn’t get separated from the text.

◊btw{
 I’ve often wondered whether the zigzagging illogic of the ◊em{Bluebook} is calculated to protect its franchise—after all, if legal citation were distilled to a few simple rules, no one would need the ◊em{Bluebook}. Its subtitle—“A Uniform System of Citation”—compresses a lot of dark humor into five words.

One problem with the ◊em{Bluebook}’s four-dot-sequence rules is that they use the same visual mark—four periods separated by spaces— to denote at least four distinct conditions. Namely: a deletion before a sentence-ending period (rule 5.3(b)(iii)); a sentence-ending period before a deletion (rule 5.3(b)(v)); a deletion both at the end and after the end of a sentence (rule 5.3(b)(vi)); and a deletion of one or more paragraphs (rule 5.1(a)(iii)). This invites ambiguity. When readers come upon a four-dot sequence, how do they know what it signifies? It may not be clear from context. Proper ellipses would help distinguish these conditions.
}