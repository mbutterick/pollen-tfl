#lang pollen

◊(define-meta title "web and email addresses")
◊hanging-topic[(topic-from-metas metas)]{Don't hyphenate}

◊em{Web addresses} identify a location on the Internet. They usually look like http://www.somelongname.com/folder/subfolder/page.html. ◊em{Email addresses} usually take the form nameofperson@somelong name.com.

Web addresses present two problems.

The first problem: web addresses can be long. Really, really long. Running the whole web address may be fine if you’re writing a law-review footnote and just need to show where you got your material. But it’s useless if you’re hoping readers will type the address on their own.

For a more usable web address, use an address-shortening service like ◊link["http://tinyurl.com"]{TinyURL} or ◊link["http://bit.ly"]{Bit.ly}. These services take a web address of any length and convert it into a short address like http://tinyurl.com/p5wf3c. This is easier to read and type. But it doesn’t reveal the underlying web address. It also isn’t guaranteed to work permanently.

If you put a web address in a citation, consider running the long version with a shortened version next to it. Then you’re covered.

The second problem: web addresses are difficult to wrap onto multiple lines. A web address is one unbroken string of characters. You don’t want your web address hyphenated, because readers will likely mistake the hyphens for part of the address. Therefore, use ◊xref{hard line breaks} to set the points where the web address should wrap onto the next line.

Email addresses are shorter than web addresses and thus not as painful. But they shouldn’t be hyphenated either, for the same reasons.