#lang pollen

◊(define-meta title "signature lines")
◊hanging-topic[(topic-from-metas metas)]{Type a sequence of underscores}

  A ◊em{signature line} is a horizontal line aligned with adjacent text.

Typography purists avoid accomplishing anything by holding down keys on the keyboard. But in this case it’s the simplest solution. To make a signature line, hold down the underscore key (shift + hyphen) until you get the length you need.

__________________________________

A rival school of thought suggests you should type a series of word spaces and format them with underlining.

__________________________________

Same thing, right? Not quite. There are three good reasons to prefer underscores to underlined word spaces.

◊numbered-list{
If you need to quickly rid a document of underlining, you might want to select all the text and then uncheck the underlining option. But this will wreck signature lines made out of underlined word spaces — they will disappear.


If you need to quickly ensure you only have one space between sentences, you might want to search for and replace any double spaces. But this will also wreck signature lines made out of word spaces — by partially deleting them.


Underscore characters don’t depend on formatting, so they will look the same no matter where they’re copied and pasted. Underlined word spaces may not.
}