#lang pollen


◊(define-meta title "hierarchical headings")
◊hanging-topic[(topic-from-metas metas)]{Consider tiered numbers}


Traditionally, ◊em{hierarchical headings} in legal documents start with roman numerals at the top level (I, II, III); then switch to capital letters (A, B, C); then numerals (1, 2, 3); then lowercase letters (a, b, c); then romanettes (i, ii, iii); and then variations of the above using two parentheses instead of one, or other barely visible changes.

This is a terrible way to label hierarchical headings.

◊numbered-list{
Roman numerals and romanettes stink. They’re difficult to read. (Quick, what number is XLIX?) They’re easy to confuse at a glance. (II vs. III, IV vs. VI, XXI vs. XII.) If what we mean by I, II, III is 1, 2, 3, then let’s just say so.


Letters aren't much better. Though we immediately recognize A, B, C as equivalent to 1, 2, 3, the letter-to-number correlation gets weaker as we go past F, G, H. (Quick, what number is T?) If what we mean by J, K, L is 10, 11, 12, then let’s just say so.


Mixing roman numerals and letters results in ambiguous references. When you see a lowercase ◊em{i}, does it denote the first item or the ninth item? Does a lowercase ◊em{v} denote the fifth item or the 22nd item?


By using only one index on each header, it’s easy to lose track of where you are in the hierarchy. If I’m at subheading ◊em{(d)}, is that ◊em{(d)} under superheading ◊em{(2)} or ◊em{(3)}?
}

Lawyers should take a cue from technical writers, who solved this problem long ago — by using tiered numbers as indexes for hierarchical headings.

So instead of:

◊indented{◊image[#:border #f #:width "80%" "hierarchical-headings-example.svg"]}

You’d have:

◊indented{◊image[#:border #f #:width "80%" "hierarchical-headings-example-2.svg"]}

To my eyes, this system is more understandable — because it only uses numbers, it avoids ambiguity or miscues. It’s also more navigable — because every tiered number is unique, it’s always clear where you are in the hierarchy. And every word processor can automatically produce tiered numbering. Consider it.