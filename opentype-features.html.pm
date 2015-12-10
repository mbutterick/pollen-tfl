#lang pollen

◊(define-meta title "OpenType features")
◊hanging-topic[(topic-from-metas metas)]{Font support + application support}


◊em{OpenType} is a font format invented in the 1990s by Microsoft and Adobe, and later adopted by Apple. A major goal of OpenType was to provide better support for international languages and writing systems than previous formats.

To do this, OpenType includes layout features — commonly known as ◊em{OpenType features} — that allow fonts to specify alternate letterforms, and rules for how they should be inserted into the text. These features are mandatory for handling languages like Arabic and Urdu.

They're not mandatory in English. But as a side effect, OpenType layout features have allowed type designers to add luxuries to their fonts — like ◊xref{alternate figures}, ◊xref{small caps}, ◊xref{ligatures}, ◊xref{ordinals}, and fractions — that had previously been difficult or impossible. That's the good news.

The bad news is that merely selecting an OpenType font doesn’t make its features available. Rather, your typesetting program also has to support the features you want to use. Even though many OpenType-enhanced fonts are available today, software companies have been slow to upgrade their programs.

This bit of background just sets the stage for the annoying truth — that OpenType features can add a lot of typographic sophistication to your document, but you can only use a given feature if it’s supported by both the font and the application.