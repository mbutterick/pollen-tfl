#lang pollen

◊(define-meta title "white-space characters")
◊hanging-topic[(topic-from-metas metas)]{For control and predictability}

You’re now familiar with the essential alphabetic, numeric, and symbol characters. We turn to the frequently overlooked ◊em{white-space characters} — the keyboard characters that put blank space between point A and point B.

There are six important white-space characters: the ◊xref{word space}, the ◊xref{nonbreaking space}, the ◊link["tabs-and-tab-stops.html"]{tab}, the ◊xref{hard line break}, the ◊xref{carriage return}, and the ◊xref{hard page break}. 

◊(omission)

“But if all white space looks the same when printed, why should I care?” Two reasons: ◊em{control} and ◊em{predictability}.

◊em{Control} means you get the intended result with the minimum keystrokes. Suppose you need a paragraph to start at the top of the next page. What do you do? If you use a hard page break rather than a sequence of carriage returns, you get the job done with one keystroke.

◊em{Predictability} means that as you edit and reformat, you’ll always get the same result. If you approximate a hard page break with carriage returns, at some point in your editing, your text will reflow and you’ll have a large, unexpected gap where you intended a page break. Then you’ll have a new problem to diagnose and fix. But a hard page break will always do the right thing.

The time you invest in learning how to use white-space characters will be paid back in layouts that snap together faster and require less fiddling.