#lang pollen

◊(define-meta title "emails")
◊hanging-topic[(topic-from-metas metas)]{System fonts or don't bother}

What about typography within ◊em{emails}? Unfortunately, due to the technical constraints of email systems, your options are limited.

Unlike a PDF, fonts don’t get transmitted with an email. So even though you can compose an email in any font you like, recipients won’t see that font unless they also happen to have it installed. Moreover, recipients get their email using a variety of hardware and software, which have inconsistent and unpredictable typographic capabilities.

This leaves two plausible policies:

◊numbered-list{
If you must format your emails, stick with common ◊xref{system fonts}, and make sure your messages don’t rely on spacing tricks specific to the font. (Those of you who insist on aligning things with multiple word spaces were already warned.) Simpler is better.


Or you can just treat email as a typography-free zone. This is my policy.}