#lang pollen

◊(define-meta title "justified text")
◊hanging-topic[(topic-from-metas metas)]{Your choice, but hyphenation
is required}


◊em{Justified text} is spaced so the left and right sides of the text block both have a clean edge. The usual alternative to justified text is ◊em{left-aligned text}, which has an uneven right edge. Compared to left-aligned text, justification gives text a cleaner, more formal look.

Justification works by adding white space between the words in each line so all the lines are the same length. This alters the ideal spacing of the font, but in paragraphs of reasonable width it’s usually not distracting.

◊indented{◊image[#:border #f "justified-text-sample-1.svg"]}

If you’re using justified text, you must also turn on ◊xref{hyphenation} to prevent gruesomely large spaces between words. I’ve been surprised at how many lawyers quibble with this advice. On what grounds? “It’s just not how we do things.” I’m afraid there’s no room for debate on this point, as the consequences are often dire—

◊indented{◊image[#:border #f "justified-text-sample-2.svg"]}

Justification is a matter of personal preference. It is not a signifier of professional typography. For instance, most major U.S. newspapers and magazines use a mix of justified and left-aligned text. Books, on the other hand, tend to be justified.