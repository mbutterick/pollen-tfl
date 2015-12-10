#lang pollen

◊(define-meta title "kerning")
◊hanging-topic[(topic-from-metas metas)]{Turn it on}

◊em{Kerning} is the adjustment of specific pairs of letters to improve spacing and fit. (It differs from ◊xref{letterspacing}, which affects all pairs.) Most fonts come with hundreds and sometimes thousands of kerning pairs inserted by the font designer.

Below, notice how kerning reduces the large gaps between certain letter pairs, making them consistent with the rest of the font.

◊indented{◊image[#:border #f "kerning-example.svg"]}

Always use kerning. By default, kerning is not activated in Word or WordPerfect, so you have to turn it on yourself. If you use ◊xref{paragraph and character styles}, turn on kerning as part of your style definitions.
