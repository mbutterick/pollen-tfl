#lang pollen

◊(define-meta title "nonbreaking spaces")
◊hanging-topic[(topic-from-metas metas)]{Prevent awkward breaks}

Your word processor assumes that a word space marks a safe place to flow text onto a new line or page. A ◊em{nonbreaking space} is the same width as a word space, but it prevents the text from flowing to a new line or page. It’s like invisible glue between the words on either side.

Put a nonbreaking space before any numeric or alphabetic reference to prevent awkward breaks. Recall this example from ◊xref{paragraph and section marks}:

◊captioned["wrong"]{The defendant has the option under Civil Code §
1782 to offer a correction to affected buyers. But ¶
17 of the agreement suggests it is required.}

◊captioned["right"]{The defendant has the option under Civil Code
§ 1782 to offer a correction to affected buyers. But
¶ 17 of the agreement suggests it is required.}

In the top example, normal word spaces come after the § and ¶ sym- bols, so the numeric references incorrectly appear on the next line.

In the bottom example, nonbreaking spaces come after the § and ¶ symbols. This time, the symbols and the numeric references stay together.

Use nonbreaking spaces after other abbreviated reference marks (◊em{Ex. A, Fig. 23}), after copyright symbols (see ◊xref{trademark and copyright symbols}), and between the dots in ◊em{Bluebook}-compliant ellipses.

In citations, use your judgment. In the citation ◊em{Fed. R. Evid. 702}, you can put a nonbreaking space before the ◊em{702} so it won’t get separated from ◊em{Evid.} But certain citation formats, like the ◊em{California Style Manual}, don’t use spaces in the abbreviated name of the source (◊em{116 Cal.App.4th 602}). In those cases, the nonbreaking space can cause more problems than it solves because it creates a large, unbreakable chunk of characters.