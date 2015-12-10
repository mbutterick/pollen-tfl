#lang pollen

◊(define-meta title "grids of numbers")
◊hanging-topic[(topic-from-metas metas)]{Vertical alignment is the key}


With grids of numbers, the typographic logic must follow the mathematical logic. If it doesn’t, your typography is likely to confuse or mislead your readers about the meaning of the numbers.

Unlike letters in words, a digit in a number has independent meaning based on its position relative to the decimal point (which may be implied). That’s how we can tell that the digits in ◊em{.49} represent a number that’s smaller than ◊em{49}. Also unlike words, different types of numbers have different rules about how they can be combined and compared.

Your goal when typesetting grids of numbers is to make sure the typography reflects the underlying meaning of the number. To do this, there is one golden rule: ◊strong{in any column, digits with the same meaning must be vertically aligned with each other}. This means that you shouldn’t merely select everything and apply the same formatting. Different kinds of numbers need different typography.

◊before-and-after-pdfs["number-grid"]