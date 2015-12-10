#lang pollen

◊(define-meta title "hard line breaks")
◊hanging-topic[(topic-from-metas metas)]{Exactly one at a time}

The ◊em{hard line break} moves the next word to the beginning of a new line without starting a new paragraph.

A hard line break can help control text flow when a carriage return won’t work. For instance, this heading breaks awkwardly:

◊indented{◊strong{IV.   The defendant is entitled to judgment as a matter of
law.}}

Suppose you want the line to break after ◊em{judgment}. That way, the first line will end in a more logical place and the two lines will be balanced. If you use a ◊xref{carriage return}, you’ll get:

◊captioned["wrong"]{◊strong{IV.   The defendant is entitled to judgment

V.   as a matter of law.}}

Not what you hoped. Instead, put a hard line break after ◊em{judgment}:

◊captioned["right"]{◊strong{IV.   The defendant is entitled to 
judgment as a matter of law.}}

Hard line breaks are also useful for separating the lines of an address (for instance, on ◊xref{letterhead} or a ◊xref{caption page}). See ◊xref{centered text} for another example of the hard line break in use.