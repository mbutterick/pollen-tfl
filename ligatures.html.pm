#lang pollen

◊(define-meta title "ligatures")
◊hanging-topic[(topic-from-metas metas)]{Optional unless the letters f and i collide}


◊em{Ligatures} were invented to solve a practical typesetting problem. In the days of metal fonts, certain characters had features that physically collided with other characters. To fix this, font foundries cast ligatures with their fonts, which combined the troublesome letters into one piece of type.

The most common ligatures involve the lowercase f because of its overhanging shape. Other ligatures also exist — some practical, some decorative, some ridiculous.

◊indented{◊image[#:border #f]{ligature_comparison.jpg}}

Digital fonts don’t have physical collisions, of course, but certain letter combinations might still overlap visually. The only time ligatures are mandatory is when you have an actual overlap between the letters f and i. Check this combination in the bold and italic styles too.

◊indented{◊image[#:border #f]{ligature_comparison_2.jpg}}

Beyond that, ligatures are largely a stylistic choice. To my eye, they can make ◊xref{body text} look somewhat quaint or old-fashioned. If you like that look, great. I don’t. So unless characters are actually colliding, I generally keep ligatures turned off.