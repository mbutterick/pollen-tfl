#lang pollen

◊(define-meta title "tabs and tab stops")
◊hanging-topic[(topic-from-metas metas)]{For horizontal space in the middle of a line}

On typewriters, the ◊em{tab} key moved the carriage to a fixed horizontal position, marked with a ◊em{tab stop}. This allowed typists to create columns of text or numbers, also known as tabular layouts (hence the name tab).

Tabs and tab stops still work the same way. A tab stop marks a location; typing a tab moves the cursor to that location.

These days, the tab is used only for inserting horizontal space in the middle of a line. If you need horizontal space at the beginning of a paragraph, adjust the ◊xref{first-line indent}. For a true tabular layout, use a ◊xref{table}, not tabs.

The tab is not as vital as it once was, but word processors still shortchange its capabilities. A new word-processing document has default tab stops every half inch. These default tab stops exist so that something happens when you type a tab in the new document. But this default behavior also suggests that what the tab key does is move the cursor a half inch at a time. Not true.

To get the most out of tabs, you should set your own tab stops. Avoid relying on the default tab stops — they undermine the goals of control and predictability. As with ◊xref{word spaces}, also avoid using sequences of tabs to move the cursor around the screen.