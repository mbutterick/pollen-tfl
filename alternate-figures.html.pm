#lang pollen

◊(define-meta title "alternate figures")
◊hanging-topic[(topic-from-metas metas)]{Consider the context}

Though we think of a font as a set of characters with a uniform visual appearance, the genesis of these characters is anything but uniform. Our writing system brings together characters that were originally handwritten by people in different countries, in different centuries. To achieve a uniform appearance, a type designer has to harmonize these disparate forms.

Our uppercase alphabet came from the inscriptional capitals of the Romans. Our lowercase alphabet came from the European uncial alphabets of the Middle Ages, which themselves evolved from scribal approximations of the uppercase alphabet. 

But our figures were invented in India. They spread west through the influence of Persian and Arab mathematicians. Traditionally they were known as ◊first-use{Arabic numerals}, but latterly as ◊first-use{Hindu-Arabic numerals}. Indic and Arabic languages, of course, look very different from European languages. Thus, figures have always presented a challenge for type designers, as they rely on shapes that are found nowhere in the uppercase and lowercase alphabets.

Type designers have met this challenge by devising sets of ◊em{alternate figures}, intended for different typographic contexts. Three things to know in advance:

◊numbered-list{
It's never wrong to use the default figures in your font — namely, the ones you get when typing the keys 0–9. Those are put in the default position because they're intended to work well across a range of contexts.


Not every font has every set of alternate figures listed here. Alternate figures are added based on the type designer's impression of how the font will be used and whether the alternates will be useful.


If alternate figures are included in your font, they'll be implemented as ◊xref{OpenType features}. The caveats there also apply, especially pertaining to software support.
}