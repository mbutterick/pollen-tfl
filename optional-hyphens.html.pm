#lang pollen

◊(define-meta title "optional hyphens")
◊hanging-topic[(topic-from-metas metas)]{Mark a hyphenation location}

The ◊em{optional hyphen} is usually invisible. The optional hyphen marks where a word should be hyphenated if the word lands at the end of a line. You can put multiple optional hyphens in a word.

Why would you want to do this? Some words bedevil hyphenation engines. For instance, ◊em{TrueType} will often get hyphenated as ◊em{Tru-eType}. To prevent this, I put an optional hyphen in the middle (◊em{True~Type}) so it will be hyphenated correctly.

How do you know whether a word won’t be hyphenated correctly? The problem usually afflicts words that aren’t in a standard hyphenation dictionary, like jargon words, unusual proper names, and other words with nonstandard spellings, like trade names. As Justice Potter Stewart might have said, you’ll know it when you see it.