#lang scribble/lp2
@(require scribble/manual)
@(require (for-label racket txexpr sugar pollen/decode pollen/tag hyphenate rackunit) scribble/eval pollen/scribblings/mb-tools)
@(define my-eval (make-base-eval))
@declare-exporting[pollen-tfl/pollen] 
@(my-eval '(require txexpr sugar racket/list))

@title[#:style manual-doc-style]{pollen.rkt} 

@(define lang @racket[#, @hash-lang[]])

We can write a @filepath{pollen.rkt} file in any @|lang|. Most commonly, that will be is @|lang| @racketmodname[racket/base]. @|lang| @racketmodname[racket] is more convenient because it loads more libraries by default, but as a result it can be slightly slower to load. So @racketmodname[racket/base] is the more virtuous habit.

This particular @filepath{pollen.rkt}, however, is written in @|lang| @racketmodname[scribble/lp2], which is the literate-programming variant of Racket's Scribble documentation language (which is also the basis of Pollen). @racketmodname[scribble/lp2] is like a text-based version of @racketmodname[racket/base]. The "literate programming" angle is that we can mix documentation and source code in one file. Probably not the option you'd choose for your own project, but in this teaching project, it's the right tool for the job.

Chunks of source code look like this:

@chunk[<chunk-name>
  (define (racket-source-code ...) ...)]

There's nothing special about the chunk name — it's just a label that @racketmodname[scribble/lp2] will use to snap the code together @seclink["Finally"]{at the end}. The result is that if you @racket[require] this file normally, you'll get the usual functions and values; but if you run it with Scribble, it turns into the documentation you see here.

@section{What is @filepath{pollen.rkt} for?}

The @filepath{pollen.rkt} source file is the main source of functions and values for the source files in a Pollen project. Everything provided from a @filepath{pollen.rkt} is automatically available to Pollen source files in the
same directory or subdirectories (unless superseded by another @filepath{pollen.rkt} below, for instance in the @filepath{fonts} subdirectory in this project).

For more, see @secref["Using_the__pollen_rkt__file"
         #:tag-prefixes '("tutorial-3")
         #:doc '(lib "pollen/scribblings/pollen.scrbl")] in the main Pollen docs.


@section{Imports}

If we were using @|lang| @racketmodname[racket], these next libraries would already be imported. But because we're using @racketmodname[racket/base] (via @racketmodname[scribble/lp2]), we need to @racket[require] them explicitly.

@chunk[<base-require>
       (require
         (for-syntax racket/base racket/syntax) ; enables macros
         racket/list
         racket/format
         racket/string
         racket/function
         racket/contract
         racket/match
         racket/system)]

Other libraries we'll be using.

@chunk[<project-require>
       (require
         sugar
         txexpr
         pollen/decode
         pollen/tag
         hyphenate
         pollen-tfl/pricing-table)]


@section{Exports}

Note that @racket[all-defined-out] would only export the definitions that are created in this file. To make
imported definitions available too, we need to re-export them with @racket[all-from-out].

@chunk[<provides>
       (provide (all-defined-out) (all-from-out pollen-tfl/pricing-table))
       ]




@section{Definitions}

@subsection{Values}


Definitions in a pollen.rkt can be functions or values.
Here are a couple values.

@racket[no-hyphens-attr]: an attribute we'll use to signal that some X-expression should not be hyphenated.

@CHUNK[<values>
       (define content-rule-color "#444") ; for CSS classes
       (define buy-url "http://typo.la/oc") ; link to buy the Typography for Lawyers paperback
       (define no-hyphens-attr '(hyphens "none"))
       ]


@subsection{Tag functions}



@subsubsection{Making tagged X-expressions (txexprs)}

In a "pollen.rkt" file you'll be making a lot of tagged X-expressions (txexprs for short).
A txexpr is just a Racket list, so you can make one with any of Racket's list-making functions
(which are plentiful). Let's run through a few of them, so they start to become familiar.

Suppose we want to generate the txexpr '(div ((class "big")) "text"). Here are some ways to do it.

@subsection{@racket[txexpr]}
A utility function from the @racket[txexpr] module. We used it in the @racket[link] function above.
The major advantage of @racket[txexpr] is that it will raise an error if your arguments are invalid
types for a tagged X-expression.

@(define-syntax-rule (eg xs ...)
   (examples #:eval my-eval xs ...))

@eg[(txexpr 'div '((class "big")) '("text"))]


The second and third arguments to @racket[txexpr] are lists, so you can use any list notation.
If your txexpr doesn't have attributes, you can pass @racket[empty] or @racket[null] for the second argument.

@eg[(txexpr 'div (list '(class "big")) (list "text"))]

@subsection{@racket[list] and @racket[list*]}

@racket[list*] is particularly useful for making txexprs, because it automatically splices the last argument.

@eg[(list 'div '((class "big")) "text")
    (list* 'div '((class "big")) '("text"))]

@subsection{@racket[cons]}

All lists are ultimately made of @racket[cons] cells.
So you can make txexprs with it too, though it's more cumbersome than the other methods.
In most cases, @racket[list*] is clearer & more flexible (@racket[cons] can only take two arguments;
@racket[list*] can take any number)

@eg[(cons 'div (cons '((class "big")) (cons "text" empty)))
    (cons 'div (list '((class "big")) "text"))]

@subsection{@racket[quasiquote]}

As the name suggests, quasiquote works like quote, but lets you "unquote" variables within.
Quasiquote notation is pleasingly compact for simple cases, but can be unruly for complex ones.
The unquote operator (,) puts a variable's value into the list.
The unquote splicing operator (,@"@") does the same thing, but if the variable holds a list of items,
it merges those items into the list (i.e., does not leave them as a sublist).

Below,       ; we unquote @racket[attrs] because we want them as a sublist
; but we splice @racket[elements] because we don't want them in a sublist


@eg[(let ([tag 'div]
          [attrs '((class "big"))]
          [elements '("text")])
      `(,tag ,attrs ,@elements))]



@defproc[
 (link
  [url path-string?]
  [#:class css-class (or/c #f string?) #f]
  [pollen-args (listof xexpr?)] ...)
 txexpr?]
Make a hyperlink.

In Pollen notation, we'll invoke the tag function like so:

◊link[url]{text of link}
◊link[url #:class "name"]{text of link}

This will become, in Racket notation:

(link url "text of link")
(link url #:class "name" "text of link")

The definition of the tag function will follow this syntax.

Learning to see the duality of Pollen & Racket notation is a necessary part of the learning curve.
Pollen notation is optimized for embedding commands in text.
Racket notation is optimized for writing code.

The relationship between the two, however, is dependable and consistent.

By contrast, most "template languages" either make you use syntax that's different from the
underlying language, or restrict you to a subset of commands.

Whereas any Racket command can be expressed in Pollen notation. So having two equivalent notation
systems ultimately lets you do more, not less.

The definition of @racket[link] follows the arguments above.

@racket[_url] is a mandatory argument.

@racket[_css-class] is a keyword argument (= must be introduced with #:class) and also optional (if it's not
provided, it will default to @racket[#f]).

@racket[_pollen-args] is a rest argument, as in ``put the rest of the arguments here.'' Most definitions of
tag functions should end with a rest argument. Why? Because in Pollen notation, the @racket[{text ...}]
in @racket[◊func[arg]{text ...}] can return any number of arguments. Maybe one (e.g., if @racket[text] is a word)
or maybe more (e.g, if @racket[text ...] is a multiline block).

If you @italic{don't} use a rest argument, and pass multiple text arguments to your tag function, you will get
an error (namely an ``arity error,'' which means the function got more arguments than it expected).

The result of our tag function will be a tagged X-expression that looks like this:

'(a ((href "url")) "text to link")
'(a ((href "url")(class "name")) "text to link")

X-expressions and tagged X-expressions are introduced in the Pollen docs.

If we don't have any text to link, use @racket[_url] as the link text too.

Otherwise, create the basic tagged X-expression, and then add the @racket[_url] and (maybe) @racket[_class] attribute.

@margin-note{@racket[let*] is the idiomatic Racket way to do what looks like mutation. Though you're not really mutating the variable — you're creating copies, all of which have the same name. For true mutation, you could also use @racket[set!] — not wrong, but not idiomatic.}

@chunk[<link>
       (define (link url #:class [class-name #f] . text-args)
         (define no-text-arguments? (empty? text-args))
         (if no-text-arguments?
             (let ([text-to-link url])
               (link #:class class-name url text-to-link))
             (let*
                 ([link-tx (txexpr 'a empty text-args)]        
                  [link-tx (attr-set link-tx 'href url)]
                  [link-tx (if class-name
                               (attr-set link-tx 'class class-name)
                               link-tx)])
               link-tx)))]



The next three tag functions are just convenience variations of @racket[link].
But they involve some crafty (and necessary) uses of @racket[apply].

@defproc[
 (buy-book-link)
 txexpr?]
Make a link with a particular URL.
@racket[buylink]: creates a link styled with the "buylink" class.
@racket[home-link]: creates a link styled with the "home-link" class.


Notice that we have to use @racket[apply] to correctly pass our @racket[text-args] rest argument to @racket[link].
Why? Because @racket[link] expects its text arguments to look like this:

(link url text-arg-1 text-arg-2 ...)

Not like this:

(link url (list text-arg-1 text-arg-2 ...))

But that's what will happen if we just do @racket[(link text-args)], and @racket[link] will complain. (Try it!)

The role of @racket[apply] is to take a list of arguments and append them to the end of the function call, so

(apply link url (list text-arg-1 text-arg-2 ...))

Is equivalent to:

(link url text-arg-1 text-arg-2 ...)

The difference here is that we're not providing a specific URL. Rather, we want to pass through
whatever URL we get from the Pollen source. So we add a @racket[url] argument.


@chunk[<buy-book-link>
       (define (buy-book-link . text-args)
         (apply link buy-url text-args))
       
       (define (buylink url . text-args)
         (apply link url #:class "buylink" text-args))
       
       (define (home-link url . text-args)
         (apply link url #:class "home-link" text-args))]


@defproc[
 (image
  [image-path path-string?]
  [#:width width string? "100%"]
  [#:border border? boolean? #t])
 txexpr?]
Make an img tag

We proceed as we did with @racket[link]. But in this case, we don't need a rest argument
because this tag function doesn't accept text arguments.

"Right, but shouldn't you use a rest argument just in case?" It depends on how you like errors
to be handled. You could capture the text arguments with a rest argument and then just silently
dispose of them. But this might be mysterious to the person editing the Pollen source (whether you
or someone else). "Where did my text go?"

Whereas if we omit the rest argument, and try to pass text arguments anyhow, @racket[image] will immediately
raise an error, letting us know that we're misusing it.

@chunk[<image>
       (define (image src #:width [width "100%"] #:border [border? #t])
         (define img-tag (attr-set* '(img) 'style (format "width: ~a" width)
                                    'src (build-path "images" src)))
         (if border?
             (attr-set img-tag 'class "bordered")
             img-tag))]


@defproc[
 (div-scale
  [factor (or/c string? number?)]
  [pollen-args (listof xexpr?)] ...)
 txexpr?]
Wrap tag in a 'div' with a scaling factor.

@chunk[<div-scale>
       (define (div-scale factor . text-args)
         ; use @racket[format] on factor because it might be either a string or a number
         (define base (txexpr 'div null text-args))
         (attr-set base 'style (format "width: ~a" factor)))]


@defproc[
 (font-scale
  [ratio (or/c string? number?)]
  [pollen-args (listof xexpr?)] ...)
 txexpr?]
Wrap tag in a 'span' with a relative font-scaling factor

@chunk[<font-scale>
       (define (font-scale ratio . text-args)
         (define base (txexpr 'span null text-args))
         (attr-set base 'style (format "font-size: ~aem" ratio)))]


@defproc[
 (home-image
  [image-path path-string?])
 txexpr?]
Make an image with class "home-image"

@chunk[<home-image>
       (define (home-image image-path)
         (attr-set (image image-path) 'class "home-image"))]

@defproc[
 (home-overlay
  [image-name path-string?]
  [pollen-args (listof xexpr?)] ...)
 txexpr?]
Create nested divs where the text sits atop a background image.
This is an example of how fiddly HTML chores can be encapsulated / hidden inside a tag function.
This makes your source files tidier.
It also makes it possible to change the fiddly HTML markup from one central location.

@chunk[<home-overlay>
       (define (home-overlay img-path . text-args)
         `(div ((class "home-overlay")(style ,(format "background-image: url('~a')" img-path)))
               (div ((class "home-overlay-inner")) ,@text-args)))]

@defproc[
 (glyph
  [text string?])
 txexpr?]
Create a span with the class "glyph".

Here, we'll use @racket[make-default-tag-function], which is an easy way to make a simple tag function.
Any keywords passed in will be propagated to every use of the tag function.

@chunk[<glyph>
       (define glyph (make-default-tag-function 'span #:class "glyph"))]


@defproc[
 (image-wrapped
  [image-path path-string?])
 txexpr?]
Like @racket[image] but with some extra attributes

@chunk[<image-wrapped>
       (define (image-wrapped img-path)
         (attr-set* (image img-path) 'class "icon" 'style "width: 120px;" 'align "left"))]

@defproc[
 (detect-list-items
  [elems txexpr-elements?])
 (listof txexpr?)]
Helper function for other tag functions that make HTML lists.

The idea is to automatically convert a sequence of three (or more) linebreaks
into a new list item (i.e., <li> tag).

Why three? Because later on, we'll make one linebreak = new line and two linebreaks = new paragraph.

This function will be used within a @racket[decode] function (more on that below)
in a position where it will be passed a list of X-expresssion elements,
and needs to return a list of X-expression elements.

The idiomatic Racket way to enforce requirements on input & output values is with a function contract.
For simplicity, I'm not using them here.

;; We need to do some defensive preprocessing here.
;; Our list of elements could contain sequences like "\n" "\n" "\n"
;; that should mean the same thing as "\n\n\n".
;; So we combine adjacent newlines with @racket[merge-newlines].
;; Then, a list item break is denoted by any element that matches three or more newlines.
;; Python people will object to the @racket[(string? elem)] test below
;; as a missed chance for "duck typing".
;; You can do duck typing in Racket (see @racket[with-handlers]) but it's not idiomatic.
;; IMO this is wise. Duck typing is an anti-pattern: it substitutes an explicit, readable test
;; for an implicit test ("I know if such-and-such isn't true, then a certain error will arise."
;; @racket[filter-split] will divide a list into sublists based on a certain test.
;; the result will be a list of lists, each representing the contents of an 'li tag.
;; We convert any paragraphs that are inside the list items.
;; Finally we wrap each of these lists of paragraphs in an 'li tag.


@chunk[<detect-list-items>
       (define (detect-list-items elems)
         (define elems-merged (merge-newlines elems))
         (define (list-item-break? elem)
           (define list-item-separator-pattern (regexp "\n\n\n+"))
           (and (string? elem) (regexp-match list-item-separator-pattern elem)))
         (define list-of-li-elems (filter-split elems-merged list-item-break?))
         (define list-of-li-paragraphs 
           (map (λ(li) (detect-paragraphs li #:force? #t)) list-of-li-elems))
         (define li-tag (make-default-tag-function 'li))
         (map (λ(lip) (apply li-tag lip)) list-of-li-paragraphs))]

@defproc[
 (make-list-function
  [tag txexpr-tag?]
  [attrs empty (listof txexpr-attrs?)])
 procedure?]
Helper function that makes other tag functions that make lists.

In Racket you will often see functions that make other functions.
This is a good way to avoid making a bunch of functions that have small variations.

One way to write this function is like so:

@racketblock[
 (define (make-list-function tag [attrs empty])
   (define (listifier . args)
     (list* tag attrs (detect-list-items args))
     listifier))]

That is, explicitly define a new function called @racket[listifier] and then return that function.
That's the best way to do it in many programming languages.

In Racket, it's not wrong, but you should feel comfortable
with the idea that any function can be equivalently expressed in lambda notation,
which is the more Rackety idiom.

@chunk[<make-list-function>
       (define (make-list-function tag [attrs empty])
         (λ args (list* tag attrs (detect-list-items args))))]

@deftogether[(
              @defproc[
 (bullet-list
  [pollen-args txexpr?] ...)
 txexpr]
               @defproc[
 (numbered-list
  [pollen-args txexpr?] ...)
 txexpr])]

Now we can define @racket[bullet-list] and @racket[numbered-list] using @racket[make-list-function].

@chunk[<bullet-list>
       (define bullet-list (make-list-function 'ul))]

@chunk[<numbered-list>
       (define numbered-list (make-list-function 'ol))]


@defproc[
 (btw
  [pollen-args txexpr?] ...)
 txexpr]
Make the "By the Way" list at the bottom of many pages,
e.g. http://typographyforlawyers.com/what-is-typography.html

Another example of using a tag function to handle fiddly HTML markup.
The @racket[btw] tag expands to an HTML list, which we will then crack open and add a headline div.

@chunk[<btw>
       (define (btw . text-args)
         (define btw-tag-function (make-list-function 'ul '((class "btw"))))
         ;; Why is @racket[apply] needed here? See the explanation for @racket[buy-book-link] above.
         (define btw-list (apply btw-tag-function text-args))
         (list* (get-tag btw-list)
                (get-attrs btw-list)
                '(div ((id "btw-title")) "by the way")
                (get-elements btw-list)))]

@defproc*[(
           [(xref
             [target string?])
            txexpr?]
           [(xref
             [url string?]
             [target string?])
            txexpr?]
           )]
Create a styled cross-reference link, with optional destination argument.

◊xref{target}
◊xref["url"]{target}

For this tag function, we will assume that target is a single text argument,
because that's how it will be used.
But to be safe, we'll raise an arity error if we get too many arguments.

;; What makes this function a little tricky is that the url argument is optional,
;; but if it appears, it appears first.
;; This is a good job for @racket[case-lambda], which lets you define separate branches for your function
;; depending on the total number of arguments provided.
;; one argument: must be a target. Note the Rackety recursive technique here:
;; we'll create a second argument and then call @racket[xref] again.
;; two arguments: must be a url followed by a target.
;; more than two arguments: raise an arity error.

@chunk[<xref>
       (define xref
         (case-lambda 
           [(target) 
            (xref (target->url target) target)]
           [(url target) 
            (apply attr-set* (link url target) 'class "xref" no-hyphens-attr)]
           [more-than-two-args 
            (apply raise-arity-error 'xref (list 1 2) more-than-two-args)]))]


@defproc[
 (target->url
  [target string?])
 string?]
Convert the target text of an xref into a url.

This function depends on my commitment to name my source files in a logical, predictable way,
e.g., "Why Does Typography Matter?" becomes "why-does-typography-matter.html".
If you needed to associate targets with URLs arbitrarily, you could store the targets and URLs
in an association list or hashtable.

I do it this way so that it's easy to add new pages and xrefs, without the extra housekeeping step
The name of the source file for a page is determined by its title.

@chunk[<target->url>
       (define (target->url target)
         (define nonbreaking-space (~a #\u00A0))
         (let* ([xn target]
                [xn (string-trim xn "?")] ; delete a question mark at the end
                [xn (string-downcase xn)] ; put string in all lowercase
                [xn (regexp-replace* #rx"é" xn "e")] ; remove accented é
                [xn (if (regexp-match #rx"^foreword" xn) "foreword" xn)] ; special rule for foreword
                [xn (if (regexp-match #rx"^table of contents" xn) "toc" xn)] ; special rule for toc
                [xn (string-replace xn nonbreaking-space "-")] ; replace nbsp with hyphen
                [xn (string-replace xn " " "-")]) ; replace word space with hyphen
           (format "~a.html" xn)))]


@defproc[
 (xref-font
  [font-name string?])
 txexpr?]
Special version of @racket[xref] for the fontrec directory.

@chunk[<xref-font>
       (define (xref-font font-name)
         (xref (format "fontrec/~a" (target->url font-name)) font-name))]


@defform[(define-heading heading-name tag-name)]
Macro for defining a function that makes a heading.

This could also be done with @racket[make-default-tag-function]. And as a rule of thumb, it's wise to reserve
macros for the times you can't avoid using them. Otherwise, use a function.

We'll bend that rule here because this is a quick & easy example macro. What makes it suitable to be
handled as a macro is that we want to use the name of the identifier (for instance 'topic') as an
argument to the function. Ordinarily we can't do that, but with a macro, we can.

@racket[define-syntax-rule] is the easiest macro form: essentially you're writing a code template
with arguments that will be filled in when you invoke the macro.

; first, heading-name is used as an identifier
; then it's used as a symbol that is converted to a string.


@chunk[<define-heading>
       (define-syntax-rule (define-heading heading-name tag)
         (define heading-name
           (make-default-tag-function tag #:class (symbol->string 'heading-name))))]

@deftogether[(
              @defproc[
 (topic
  [pollen-args (listof xexpr?)] ...)
 txexpr?]
               @defproc[
 (subhead
  [pollen-args (listof xexpr?)] ...)
 txexpr?]
               @defproc[
 (font-headline
  [pollen-args (listof xexpr?)] ...)
 txexpr?]
               @defproc[
 (section
  [pollen-args (listof xexpr?)] ...)
 txexpr?]
               @defproc[
 (chapter
  [pollen-args (listof xexpr?)] ...)
 txexpr?])]

@chunk[<headings>
       (define-heading topic 'h3)
       (define-heading subhead 'h3)
       (define-heading font-headline 'h3)
       (define-heading section 'h2)
       (define-heading chapter 'h1)]

@defform[(define-heading-from-metas heading-name)]
Macro for defining a function that makes a heading by relying on data in the metas.

This macro relies on @racket[syntax-case] rather than @racket[define-syntax-rule].
It's a little more complicated, but also more flexible (and more idiomatic in Racket).
@racket[define-syntax-rule] is actually a special simplified version of @racket[syntax-case].
The best advice on learning macros is to start with @racket[syntax-case], because you can't live without it.
Good tutorial: http://www.greghendershott.com/fear-of-macros/pattern-matching.html

Otherwise this macro is similar to @racket[define-heading], except that we want to introduce a new identifier
based on the name given to the macro. So if we pass @racket[topic] to the macro, it will define
an identifier called @racket[topic-from-metas]. You can't do that with @racket[define-syntax-rule].

@chunk[<define-heading-from-metas>
       (define meta-key-for-page-title 'title)
       (define-syntax (define-heading-from-metas stx)
         (syntax-case stx ()
           [(_ heading-name)
            (with-syntax ([heading-from-metas (format-id stx "~a-from-metas" #'heading-name)])
              #'(define (heading-from-metas metas)
                  (heading-name (hash-ref metas meta-key-for-page-title))))]))]


@deftogether[(
              @defproc[
 (topic-from-metas [metas hash?])
 txexpr?]
               @defproc[
 (section-from-metas [metas hash?])
 txexpr?]
               @defproc[
 (chapter-from-metas [metas hash?])
 txexpr?])]

@chunk[<headings-from-metas>
       (define-heading-from-metas topic)
       (define-heading-from-metas section)
       (define-heading-from-metas chapter)]

@defproc[
 (hanging-topic
  [topic-xexpr xexpr?]
  [pollen-args (listof xexpr?)] ...)
 txexpr?]
Convert a topic + subhead into one HTML markup unit

@chunk[<hanging-topic>
       (define (hanging-topic topic-xexpr . pollen-args)
         (txexpr 'div (list '(class "hanging-topic") no-hyphens-attr)
                 (list topic-xexpr (list* 'p (list no-hyphens-attr) pollen-args))))]

@defproc[
 (quick-table
  [table-rows (listof xexpr?)] ...)
 txexpr?]
Make an HTML table using simplified notation

◊quick-table{heading left | heading center | heading right
upper left | upper center | upper right
lower left | lower center | lower right}

In HTML, wrapping every paragraph in <p> tags is a terrible and dull task.
But formatting tables is even worse.

This function lets you make simple tables using "|" to signify columns,
and line breaks to signify rows.

Let's uncork a few more whizzy Racket commands while we're at it.

This function assumes that each row has the same number of columns.
You could improve it to fill in blank cells in rows that need them.


@chunk[<quick-table>
       (define (quick-table . text-args)
         
         ;; In Pollen, a multiline text-args block arrives as a list of lines and linebreak characters.
         ;; (A situation we already encountered in @racket[detect-list-items].)
         (define rows-of-text-cells
           (let ([text-rows (filter-not whitespace? text-args)]) ; throw out the linebreak characters
             ;; @racket[for/list] is very handy: a @racket[for] loop that gathers the results into a list.
             ;; Think of it as a more flexible version of @racket[map].
             (for/list ([text-row (in-list text-rows)])
                       ;; the cells are delimited within a row by "|", so split on this char
                       (for/list ([text-cell (in-list (string-split text-row "|"))])
                                 (string-trim text-cell))))) ; trim remaining whitespace from cell text
         
         ;; Racket's @racket[match] functions are very useful.
         ;; Among other things, they can be used for Python-style data unpacking.
         ;; The expression on the right will produce three tag functions;
         ;; the @racket[match-define] assigns them to three new identifiers.
         (match-define (list tr-tag td-tag th-tag) (map make-default-tag-function '(tr td th)))
         
         ;; now we'll take our rows of text cells and apply cell-level HTML tags.
         ;; the first row will get 'th tags; the other rows get 'td tags.
         (define html-rows
           ;; another use of @racket[match]. Notice how this @racket[cons] is used to separate a list into parts ...
           (match-let ([(cons header-row other-rows) rows-of-text-cells])
             ;; ... whereas this @racket[cons] is used to combine parts into a list
             (cons (map th-tag header-row)
                   (for/list ([row (in-list other-rows)])
                             (map td-tag row)))))
         
         ;; With the cells tagged up, add the row tags and finally the table tag.
         ;; Notice that we use @racket[apply] with @racket[tr-tag] to unpack the list of cells in each html-row.
         ;; Remember that @racket[apply] does something very simple:
         ;; Converts an expression of the form @racket[(apply func (list arg1 arg2 ...))]
         ;; Into @racket[(func arg1 arg2 ...)]
         (cons 'table (for/list ([html-row (in-list html-rows)])
                                (apply tr-tag html-row))))]

@defproc[
 (pdf-thumbnail
  [pdf-path path-string?])
 txexpr?]
Create a thumbnail of a PDF that links to the PDF.

This function will only work properly if you have @racket[sips] on your system
(command-line image-processing program, included with OS X).

This shows how you can fold other kinds of project housekeeping into Pollen commands.
Here, the function generates the thumbnail it needs when the page is compiled.

One disadvantage of this approach is that the thumbnail will *always* be generated on recompile,
though you could put in some logic to avoid this (e.g., check the modification date of the PDF).
In this case, @racket[sips] is fast enough that it's not bothersome.

@chunk[<pdf-thumbnail>
       (define (pdf-thumbnail-link pdf-pathstring)
         (define img-extension "gif")
         (define img-pathstring (->string (add-ext (remove-ext pdf-pathstring) img-extension)))
         (define sips-command
           (format "sips -Z 2000 -s format ~a --out '~a' '~a' > /dev/null"
                   img-extension img-pathstring pdf-pathstring))
         (link pdf-pathstring (if (system sips-command)
                                  `(img ((src ,img-pathstring)))
                                  ;; usually one would raise an error on the next line,
                                  ;; but for instructional purposes, we'll have a graceful fail
                                  "sips not available")))]

@deftogether[(
              @defproc[
 (pdf-thumbnail-link-from-metas
  [metas hash?])
 txexpr?]
               @defproc[
 (before-and-after-pdfs
  [base-name string?])
 txexpr?]
               @defproc[
 (alternate-after-pdf
  [base-name string?])
 txexpr?]
               )]
A few convenience variants of @racket[pdf-thumbnail-link]

@chunk[<pdf-thumbnail-variants>
       (define (pdf-thumbnail-link-from-metas metas)
         (define-values (dir fn _) (split-path (add-ext (remove-ext* (hash-ref metas 'here-path)) "pdf")))
         (pdf-thumbnail-link (->string fn)))
       
       (define (before-and-after-pdfs base-name)
         `(div 
           (div ((class "pdf-thumbnail"))
                "before" (br)
                ,(pdf-thumbnail-link (format "pdf/sample-doc-~a-before.pdf" base-name)))
           (div ((class "pdf-thumbnail"))
                "after" (br)
                ,(pdf-thumbnail-link (format "pdf/sample-doc-~a-after.pdf" base-name)))))
       
       (define (alternate-after-pdf base-name)
         `(div ((class "pdf-thumbnail"))
               "after (alternate)" (br)
               ,(pdf-thumbnail-link (format "pdf/sample-doc-~a-after-alternate.pdf" base-name))))]

@defproc[
 (root
  [pollen-args (listof txexpr?)] ...)
 txexpr?]
Decode page content

In a Pollen markup source, the output is a tagged X-expression that starts with @racket[root]:

(root (div ((class "headline")) "Page title") ...)

Recall that every Pollen tag calls a function with the same name (if it exists, otherwise it just
becomes a tag). This is also true of @racket[root].

@racket[root] has slightly special status inasmuch as it is the top tag of the X-expression,
and thus the last tag function that will get called. Therefore, @racket[root] is a good place to put any
processing that should happen once all the page content has been filled in.

Often, you'll want to use a @racket[decode] function, which can recursively perform different kinds of
processing on different types of page elements.

@chunk[<root>
       (define (root . elems)
         ;; We will do the decoding in two steps.
         ;; Detect paragraphs first so that they're treated as block-txexprs in next phase.
         (define elements-with-paragraphs (decode-elements elems #:txexpr-elements-proc detect-paragraphs))
         ;; Then do the rest of the decoding normally.
         (list* 'div '((id "doc"))
                (decode-elements elements-with-paragraphs
                                 #:block-txexpr-proc hyphenate-block
                                 #:string-proc (compose1 make-quotes-hangable
                                                         fix-em-dashes
                                                         smart-quotes)
                                 #:exclude-tags '(style script))))]

@defproc[
 (hyphenate-block
  [block-tx txexpr?])
 txexpr?]
Helper function for root decoder 

@chunk[<hyphenate-block>
       (define (hyphenate-block block-tx)
         ;; The basic @racket[hyphenate] function comes from the @racket[hyphenate] module.
         ;; We could attach @racket[hyphenate] to our decoder as a string processor rather than block processor.
         ;; But we want to be able to handle our "no-hyphens" flag (aka @racket[no-hyphens-attr]).
         ;; So we want to look at blocks, not strings.
         (define (no-hyphens? tx)
           (or (member (get-tag tx) '(th h1 h2 h3 h4 style script)) ; don't hyphenate these, no matter what
               (member no-hyphens-attr (get-attrs tx)))) ; also don't hyphenate blocks with @racket[no-hyphens-attr]
         (hyphenate block-tx
                    #:min-left-length 3
                    #:min-right-length 3
                    #:omit-txexpr no-hyphens?))]

@defproc[
 (make-quotes-hangable
  [str string?])
 txexpr?]
Perform tricky processing on quotation marks.

Because I'm a typography snob I like to push quotation marks into the margin a little bit
when they appear at the left edge of a line (aka "hanging quotes").

This function just wraps left-hand quote marks in two little tags ("push" and "pull")
that I can then manipulate in CSS to get the effect.

@chunk[<make-quotes-hangable>
       (define (make-quotes-hangable str)
         ;; using @racket[regexp-match*] with #:gap-select? makes it act like a funny kind of string splitter
         (define substrs (regexp-match* #px"\\s?[“‘]" str #:gap-select? #t))
         (if (= (length substrs) 1) ; no submatches
             (car substrs)
             (cons 'quo (append-map (λ(str)
                                      (let ([strlen (string-length str)])
                                        (if (> strlen 0)
                                            (case (substring str (sub1 strlen) strlen)
                                              [("‘") (list '(squo-push) `(squo-pull ,str))]
                                              [("“") (list '(dquo-push) `(dquo-pull ,str))]
                                              [else (list str)])
                                            (list str)))) substrs))))]

@defproc[
 (fix-em-dashes
  [str string?])
 txexpr?]
Helper function for root decoder

When I type an em dash in my sources, I will often leave a space around it,
but I don't want spaces in the output, so this function removes them.

@chunk[<fix-em-dashes>
       (define (fix-em-dashes str)
         ;; \u00A0 = nbsp, \u2009 = thinsp (neither included in \s)   
         (let* ([str (regexp-replace* #px"(?<=\\w)[\u00A0\u2009\\s]—" str "—")]
                [str (regexp-replace* #px"—[\u00A0\u2009\\s](?=\\w)" str "—")])
           str))]


@subsubsection{Miscellaneous tag functions}

Presented without docs or comment, as it should be obvious at this point what they do.

@chunk[<misc-functions>
       (define omission (make-default-tag-function 'div #:class "omission"))
       
       (define mono (make-default-tag-function 'span #:class "mono"))
       
       (define font-details (make-default-tag-function 'div #:class "font-details"))
       
       (define mb-font-specimen
         (make-default-tag-function 'div #:class "mb-font-specimen" #:contenteditable "true"))
       
       (define (margin-note . xs)
         `(div ((class "margin-note") ,no-hyphens-attr) ,@xs))
       
       (define os (make-default-tag-function 'span #:class "os"))
       
       (define (gap [size 1.5])
         `(div ((style ,(format "height: ~arem" size)))))
       
       (define (center . xs)
         `(div ((style "text-align:center")) ,@xs))
       
       (define (indented #:hyphenate [hyphenate #t]  . xs)
         `(p ((class "indented"),@(if (not hyphenate) (list no-hyphens-attr) null)) ,@xs))
       
       (define caption-runin (make-default-tag-function 'span #:class "caption-runin"))
       
       (define caption (make-default-tag-function 'span #:class "caption"))
       
       (define (captioned name . xs)
         `(table ((class "captioned indented"))
                 (tr (td ((style "text-align:left")) ,@xs) (td  ,(caption name)))))]


@section{Utility functions}

@defproc[
 (dev-mode?)
 boolean?]
Check whether Pollen is running in development mode, which means that it was started from the command line with the environment variable @tt{POLLEN} set to the value @tt{DEV}:

@terminal{
> POLLEN=DEV raco pollen ...}

Rather than the ordinary:

@terminal{
> raco pollen ...}

This functions will be useful later when we want to change the behavior of certain functions when Pollen runs in dev mode. For instance, we might want to run certain functions in a higher speed / lower quality mode. Or output debug information about others.

@chunk[<dev-mode>
       (define (dev-mode?)
         (equal? (getenv "POLLEN") "DEV"))
       ]

Though the environment variable name is fixed as @tt{POLLEN}, there's no special magic to @tt{DEV}. We could pick any value we wanted to denote development mode:

@terminal{
> POLLEN=FLUGELHORN raco pollen ...}


@defproc[
 (capitalize-first-letter
  [str string?])
 string?]
For use in our HTML templates. We could also define this function inside a template. But since we have more than one template in this project, we'll put it here, so it can be available to all the templates.

@chunk[<capitalize-first-letter>
       (define (capitalize-first-letter str)
         (regexp-replace #rx"^." str string-upcase))]

@section{Finally}

This last incantation is needed so @racketmodname[scribble/lp2] knows how to put together all the code chunks we've introduced in this file.

@chunk[<*>
       <base-require>
       <project-require>
       <provides>
       <dev-mode>
       <values>
       <link>
       <buy-book-link>
       <image>
       <div-scale>
       <font-scale>
       <home-image>
       <home-overlay>
       <glyph>
       <image-wrapped>
       <detect-list-items>
       <make-list-function>
       <bullet-list>
       <numbered-list>
       <btw>
       <xref>
       <target->url>
       <xref-font>
       <define-heading>
       <headings>
       <define-heading-from-metas>
       <headings-from-metas>
       <hanging-topic>
       <quick-table>
       <pdf-thumbnail>
       <pdf-thumbnail-variants>
       <root>
       <hyphenate-block>
       <make-quotes-hangable>
       <fix-em-dashes>
       <capitalize-first-letter>
       <misc-functions>
       ]