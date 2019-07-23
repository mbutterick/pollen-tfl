#lang scribble/lp2
@(require scribble/manual)
@(require (for-label racket txexpr sugar pollen/decode pollen/tag hyphenate rackunit) scribble/eval pollen/scribblings/mb-tools)
@(define my-eval (make-base-eval))
@declare-exporting[pollen-tfl/pollen] 
@(my-eval '(require txexpr sugar racket/list))

@title[#:style manual-doc-style]{@filepath{pollen.rkt}} 

@(define lang @racket[#, @hash-lang[]])


We can write a @filepath{pollen.rkt} file in any @|lang|. Most commonly, you'll choose @|lang| @racketmodname[racket/base]. @|lang| @racketmodname[racket] is a tiny bit more convenient because it loads more libraries by default. But as a result, it can be slightly slower to load. So @racketmodname[racket/base] is the more virtuous habit.

This particular @filepath{pollen.rkt}, however, is written in @|lang| @racketmodname[scribble/lp2], which is the literate-programming variant of Racket's Scribble documentation language (which is also the basis of Pollen). @|lang| @racketmodname[scribble/lp2] is like a text-based version of @|lang| @racketmodname[racket/base]. The ``literate programming'' angle is that we can mix documentation and source code in one file. Probably not the option you'd choose for your own project. But in this teaching project, it's the right tool for the job.

In this documentation, chunks of source code look like this:

@chunk[<chunk-name>
       (define (racket-source-code ...) ...)]

There's nothing special about the chunk name — it's just a label that @|lang| @racketmodname[scribble/lp2] will use to snap the code together @seclink["Finally"]{at the end}. The result is that if you @racket[require] this file normally, you'll get the usual functions and values; but if you run it with Scribble, it turns into the documentation you see here.

Aside from that wrinkle, all the code shown here is standard @racketmodname[racket/base], and can be copied & adapted for your own @filepath{pollen.rkt} files written in @|lang| @racketmodname[racket/base] (or @|lang| @racketmodname[racket]).

@section{What is @filepath{pollen.rkt} for?}

The @filepath{pollen.rkt} source file is the main source of functions and values for the source files in a Pollen project. Everything provided from a @filepath{pollen.rkt} is automatically available to @|lang| @racketmodname[pollen] source files in the
same directory or subdirectories (unless superseded by another @filepath{pollen.rkt} within a subdirectory).

For more, see @secref["Introducing__pollen_rkt_"
                      #:tag-prefixes '("tutorial-3")
                      #:doc '(lib "pollen/scribblings/pollen.scrbl")] in the main Pollen docs.


@section{Imports}

Though @racket[require] declarations can live anywhere at the top layer of a source file, it's typical to consolidate them at the top.

If we were using @|lang| @racketmodname[racket], these next libraries would already be imported. But we aren't, so they're not.

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

Other libraries we'll be using. @racketmodname[sugar] and @racketmodname[txexpr] are utility libraries installed with Pollen. @racketmodname[hyphenate] is separate, but can be installed easily:

@terminal{> raco pkg install hyphenate}

It's up to you whether you store all your functions and definitions in @filepath{pollen.rkt}, or spread them over different source files. Regardless of where you store them, @filepath{pollen.rkt} remains the central gathering point for everything you want to propagate to the @|lang| @racketmodname[pollen] source files in your project.

@margin-note{If you end up making reusable functions, you can share them between Pollen projects (and with other Racket users, if you want) by moving them into a @italic{package}.  For more, see @secref["how-to-create" #:doc '(lib "pkg/scribblings/pkg.scrbl")].}

@chunk[<project-require>
       (require
         pollen/decode
         pollen/file
         pollen/tag
         sugar
         txexpr
         hyphenate
         pollen/unstable/typography)]


@section{Exports}

Note that @racket[all-defined-out] would only export the definitions that are created in this file. To make
imported definitions available too, we need to re-export them explicitly with @racket[all-from-out].

@chunk[<provides>
       (provide (all-defined-out))
       ]




@section{Definitions}

@subsection{Values}

Definitions in a @filepath{pollen.rkt} can be functions or values. Because they get propagated to other Pollen source files, they're almost like global definitions. As usual with global definitions, you should use them when you need them, but it's still wise to localize things within specific directories or source files when you can. Otherwise the main @filepath{pollen.rkt} can get to be unwieldy.

@racket[content-rule-color] is a CSS value.

@racket[buy-url] is the master URL for buying the TFL paperback. In general, it's wise to store hard-coded values like these in a variable so that you can change the value from one location later if you need to.

@racket[no-hyphens-attr] is an X-expression attribute we'll use to signal that a certain X-expression should not be hyphenated. The need for this will be explained later.

@CHUNK[<values>
       (define content-rule-color "#444") ; for CSS classes
       (define buy-url "http://typo.la/oc") ; link to buy the Typography for Lawyers paperback
       (define no-hyphens-attr '(hyphens "none"))
       ]


@subsection{Tag functions}



@subsubsection{Making tagged X-expressions (txexprs)}

In a @filepath{pollen.rkt} you'll be making a lot of tagged X-expressions (txexprs for short). A txexpr is just a Racket list, so you can make txexprs with any of Racket's list-making functions — which are plentiful. Which one you use depends on what fits most naturally with the current task.

@margin-note{@secref["X-expressions" #:doc '(lib "pollen/scribblings/pollen.scrbl")] are introduced in the Pollen docs.}


Let's run through a few of them, so they start to become familiar. Suppose we want to generate the txexpr '(div ((class "big")) "text"). Here are some ways to do it.

@subsection{@tt{txexpr}}

A utility function from the @racket[txexpr] module. We used it in the @racket[link] function above. The major advantage of @racket[txexpr] is that it will raise an error if your arguments are invalid types for a tagged X-expression.

@(define-syntax-rule (eg xs ...)
   (examples #:eval my-eval xs ...))

@eg[(txexpr 'div '((class "big")) '("text"))
    (txexpr 42 '((class "big")) '("text"))
    (txexpr 'div 'invalid-input "text")
    (txexpr 'div '((class "big")) 'invalid-input)]


The second and third arguments to @racket[txexpr] are lists, so you can use any list notation. If your txexpr doesn't have attributes, you pass @racket[empty] or @racket[null] for the second argument.

@eg[(txexpr 'div (list '(class "big")) (list "text"))]

@margin-note{Because @racket[txexpr] is designed to check its arguments for correctness, it insists on getting an explicit argument for the attributes, even if @racket[empty]. When you're using generic list-making functions (see below) to create txexprs, you can omit the attribute list if it's empty.}

@subsection{@tt{list} and @tt{list*}}

@racket[list*] is particularly useful for making txexprs, because it automatically splices the last argument.

@eg[(list 'div '((class "big")) "text")
    (list* 'div '((class "big")) '("text"))]

Unlike @racket[txexpr], however, @racket[list] and @racket[list*] will happily let you create invalid txexprs.

@eg[(define not-a-tx (list* 42 '((class "big")) "text"))
    (txexpr? not-a-tx)]

This isn't necessarily a bad thing. When a txexpr needs to pass through multiple layers of processing, it can be useful to create intermediate results that are txexpr-ish, and simplify them at the end.

@subsection{@tt{cons}}

All lists are ultimately made of @racket[cons] cells. So you can make txexprs with @racket[cons] too, though it's more cumbersome than the other methods. In most cases, @racket[list*] is clearer & more flexible, because @racket[cons] can only take two arguments, whereas @racket[list*] can take any number.

@eg[(cons 'div (cons '((class "big")) (cons "text" empty)))
    (cons 'div (list '((class "big")) "text"))]

@subsection{@tt{quasiquote}}

As the name suggests, @racket[quasiquote] works like @racket[quote], but lets you @racket[unquote] variables within. Quasiquote notation is pleasingly compact for simple cases. But it can be unruly for complex ones.
The unquote operator (@litchar{,}) puts a variable's value into the list.
The unquote splicing operator (@litchar{,@"@"}) does the same thing, but if the variable holds a list of items, it merges those items into the list (i.e., does not leave them as a sublist).

Below, we unquote @racket[attrs] because we want them to remain a sublist. But we splice @racket[elements] because we want them to be merged with the main list.

@eg[(let ([tag 'div]
          [attrs '((class "big"))]
          [elements '("text")])
      `(,tag ,attrs ,@elements))]


@subsection{Functions}

@defproc[
 (link
  [url string?]
  [#:class css-class (or/c #f string?) #f]
  [tx-element xexpr?] ...)
 txexpr?]
Make a hyperlink.

In Pollen notation, we'll invoke the tag function like so:

@terminal{
 ◊link[url]{text of link}
 ◊link[url #:class "name"]{text of link}}

This will become, in Racket notation:

@terminal{
 (link url "text of link")
 (link url #:class "name" "text of link")}

@margin-note{Getting a feel for the duality of Pollen & Racket notation is a necessary part of the learning curve. If it seems like an annoying complication, consider that the two styles are optimized for different contexts: Pollen notation is for embedding commands in text, and Racket notation is for writing code. The fact that the two are interchangeable is what guarantees that everything that can be done in Racket can also be done in Pollen.}


The result of our tag function will be a tagged X-expression that looks like this:

@terminal{
 '(a ((href "url")) "text to link")
 '(a ((href "url")(class "name")) "text to link")}


The definition of @racket[link] follows the arguments above.

@racket[_url] is a mandatory argument.

@racket[_css-class] is a keyword argument (= must be introduced with @racket[#:class]) and also optional (if it's not provided, it will default to @racket[#f]).

@racket[_tx-elements] is an optional argument that represents the text (or other content) that gets linked. If we don't have anything to link, use @racket[_url] as the link text too.

@racket[_tx-elements] is a @seclink["contracts-rest-args" #:doc '(lib "scribblings/guide/guide.scrbl")]{@italic{rest argument}}, as in ``put the rest of the arguments here.'' Most definitions of
tag functions should end with a rest argument. Why? Because in Pollen notation, the @tt{{text ...}}
in @tt{◊func[arg]{text ...}} can return any number of arguments. Maybe one (e.g., if @tt{{text ...}} is a word)
or maybe more (e.g, if @tt{{text ...}} is a multiline block).

If you don't use a rest argument, and pass multiple text arguments to your tag function, you'll get an error (namely an ``arity error,'' which means the function got more arguments than it expected).

@margin-note{@racket[let*] is the idiomatic Racket way to do what looks like mutation. Though you're not really mutating the variable — you're creating copies, all of which have the same name. For true mutation, you could also use @racket[set!] — not wrong, but not idiomatic.}

@chunk[<link>
       (define (link url #:class [class-name #f] . tx-elements)
         (let* ([tx-elements (if (empty? tx-elements)
                                 (list url)
                                 tx-elements)]
                [link-tx (txexpr 'a empty tx-elements)]        
                [link-tx (attr-set link-tx 'href url)])
           (if class-name
               (attr-set link-tx 'class class-name)
               link-tx)))]



The next three tag functions are just convenience variations of @racket[link]. But they involve some crafty (and necessary) uses of @racket[apply].

@deftogether[(
              @defproc[
 (buy-book-link
  [tx-element xexpr?] ...)
 txexpr?]
               @defproc[
 (buylink
  [url string?]
  [tx-element xexpr?] ...)
 txexpr?]
               @defproc[
 (home-link
  [url string?]
  [tx-element xexpr?] ...)
 txexpr?])]
Make a link with a particular URL. The link resulting from @racket[buylink] is styled with the @tt{buylink} class, and the one from @racket[home-link] is styled with the @tt{home-link} class.

Notice that we have to use @racket[apply] to correctly pass our @racket[tx-elements] rest argument to @racket[link].
Why? Because @racket[link] expects its text arguments to look like this:

@terminal{(link url arg-1 arg-2 ...)}

Not like this:

@terminal{(link url (list arg-1 arg-2 ...))}

But that's what will happen if we just do @racket[(link tx-elements)], and @racket[link] will complain. (Try it!)

The role of @racket[apply] is to take a list of arguments and append them to the end of the function call, so

@terminal{(apply link url (list arg-1 arg-2 ...))}

Is equivalent to:

@terminal{(link url arg-1 arg-2 ...)}

The difference here is that we're not providing a specific URL. Rather, we want to pass through whatever URL we get from the Pollen source. So we add a @racket[_url] argument.


@chunk[<buy-book-link>
       (define (buy-book-link . tx-elements)
         (apply link buy-url tx-elements))
       
       (define (buylink url . tx-elements)
         (apply link url #:class "buylink" tx-elements))
       
       (define (home-link url . tx-elements)
         (apply link url #:class "home-link" tx-elements))]


@defproc[
 (image
  [image-path path-string?]
  [#:width width string? "100%"]
  [#:border border? boolean? #t])
 txexpr?]
Make an image tag. We proceed as we did with @racket[link]. But in this case, we don't need a rest argument because this tag function doesn't accept text arguments.

``Right, but shouldn't we use a rest argument just in case?'' It depends on how you like errors to be handled. You could capture the text arguments with a rest argument and then just silently dispose of them. But this might be mysterious to the person editing the Pollen source (whether you or someone else). "Where did my text go?"

Whereas if we omit the rest argument, and try to pass text arguments anyhow, @racket[image] will immediately raise an error, letting us know that we're misusing it.

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
  [tx-element xexpr?] ...)
 txexpr?]
Wrap tag in a @racket['div] with a scaling factor. Keep in mind that with X-expressions, numbers like @racket[2048] are not interchangeable with strings like @racket["2048"]. Moreover, all values inside attributes have to be strings. So if an argument will be used as an attribute value, it's wise to explicitly convert it to a strings explicitly. This has the side benefit of allowing the function to accept either a string or number for @racket[_factor].

@chunk[<div-scale>
       (define (div-scale factor . tx-elements)
         (define base (txexpr 'div null tx-elements))
         (attr-set base 'style (format "width: ~a" factor)))]


@defproc[
 (font-scale
  [ratio (or/c string? number?)]
  [tx-element xexpr?] ...)
 txexpr?]
Like @racket[div-scale] — wrap tag in a @racket['span] with a relative font-scaling factor.

@chunk[<font-scale>
       (define (font-scale ratio . tx-elements)
         (define base (txexpr 'span null tx-elements))
         (attr-set base 'style (format "font-size: ~aem" ratio)))]


@defproc[
 (home-image
  [image-path path-string?])
 txexpr?]
Make an image with class @tt{home-image}.

@chunk[<home-image>
       (define (home-image image-path)
         (attr-set (image image-path) 'class "home-image"))]

@defproc[
 (home-overlay
  [image-name path-string?]
  [tx-element xexpr?] ...)
 txexpr?]
Create nested divs where the text sits atop a background image.
This is an example of how fiddly HTML chores can be encapsulated / hidden inside a tag function.
This makes your source files tidier.
It also makes it possible to change the fiddly HTML markup from one central location.

@chunk[<home-overlay>
       (define (home-overlay img-path . tx-elements)
         `(div ((class "home-overlay")
                (style ,(format "background-image: url('~a')" img-path)))
               (div ((class "home-overlay-inner")) ,@tx-elements)))]

@defproc[
 (glyph
  [text string?])
 txexpr?]
Create a span with the class @tt{glyph}. 

Here, we'll use @racket[default-tag-function], which is an easy way to make a simple tag function. Any keywords passed in will be propagated to every use of the tag function.

@chunk[<glyph>
       (define glyph (default-tag-function 'span #:class "glyph"))]


@defproc[
 (image-wrapped
  [image-path path-string?])
 txexpr?]
Like @racket[image], but with some extra attributes.

@chunk[<image-wrapped>
       (define (image-wrapped img-path)
         (attr-set* (image img-path) 
                    'class "icon" 
                    'style "width: 120px;" 
                    'align "left"))]

@defproc[
 (detect-list-items
  [elems (listof txexpr?)])
 (listof txexpr?)]
Helper function for other tag functions that make HTML lists.

The idea is to interpret a sequence of three (or more) linebreaks
in the text as a list-item delimiter (i.e., drop in a @tt{<li>} tag).
Why three linebreaks? Because later on, we'll use one linebreak to denote a new line, and two linebreaks to denote a new paragraph.

This function will be used within a @racket[decode] function (more on that below) in a position where it will be passed a list of X-expresssion elements, so it also needs to return a list of X-expression elements.

@margin-note{The idiomatic Racket way to enforce requirements on input & output values is with a @seclink["function-contracts"
                                                                                                          #:doc '(lib "scribblings/reference/reference.scrbl")]{@italic{function contract}}. For simplicity, I'm not using them here, but they are another virtuous habit  .}

Our list of elements could contain sequences like @racket['("\n" "\n" "\n")], which should mean the same thing as @racket["\n\n\n"]. So first, we'll combine adjacent newlines with @racket[merge-newlines].

@racket[filter-split] will divide a list into sublists based on a test for the list-item delimiter. The result will be a list of lists, each representing the contents of an @racket['li] tag. We'll convert any paragraphs that are inside the list items. Finally we'll wrap each of these lists of paragraphs in an @racket['li] tag.

@chunk[<detect-list-items>
       (define (detect-list-items elems)
         (define elems-merged (merge-newlines elems))
         (define (list-item-break? elem)
           (define list-item-separator-pattern (regexp "\n\n\n+"))
           (and (string? elem) (regexp-match list-item-separator-pattern elem)))
         (define list-of-li-elems (filter-split elems-merged list-item-break?))
         (define list-of-li-paragraphs 
           (map (λ(li) (decode-paragraphs li #:force? #t)) list-of-li-elems))
         (define li-tag (default-tag-function 'li))
         (map (λ(lip) (apply li-tag lip)) list-of-li-paragraphs))]

@margin-note{Explicit type checking — e.g., @racket[(string? elem)] — is common in Racket. You can do ``duck typing'' (see @racket[with-handlers]) but it's not idiomatic. IMO this is wise — better to have an explicit, readable test (like @racket[string?]) rather than an implicit, indirect one (``If the input isn't a @racket[string?], then a certain error will arise.'')}


Because of the expression-based structure of Racket, it's often possible to write functions in an absurdly nested style. For instance, the last function could be written like so:

@chunk[<bad-idea>
       (define (detect-list-items elems)
         (map (compose1 (curry apply (default-tag-function 'li))
                        (curryr decode-paragraphs #:force? #t))
              (filter-split (merge-newlines elems)
                            (λ(x) (and (string? x)
                                       (regexp-match #rx"\n\n\n+" x))))))]


This is a good way to lose your friends, and then your mind. You may not care to spell everything out the way I've been doing in this sample project. But readability is a virtuous habit.


@defproc[
 (make-list-function
  [tag txexpr-tag?]
  [attrs empty (listof txexpr-attrs?)])
 procedure?]
Helper function that makes other tag functions that make lists.

In Racket you'll often see functions that make other functions. (These are sometimes called @seclink["Additional_Higher-Order_Functions"
                                                                                                     #:doc '(lib "scribblings/reference/reference.scrbl")]{@italic{higher-order functions}}.) This is a good way to avoid making a bunch of functions that have small variations.

One way to write this function is like so:

@racketblock[
 (define (make-list-function tag [attrs empty])
  (define (listifier . args)
    (list* tag attrs (detect-list-items args)))
  listifier)]

That is, explicitly define a new function called @racket[listifier] and then return that function. That's the best way to do it in many programming languages.

In Racket, it's wouldn't be wrong. But you should feel comfortable
with the idea that any function can be equivalently expressed in @racket[lambda] notation, which is the more idiomatic form.

@chunk[<make-list-function>
       (define (make-list-function tag [attrs empty])
         (λ args (list* tag attrs (detect-list-items args))))]

@deftogether[(
              @defproc[
 (bullet-list
  [tx-element txexpr?] ...)
 txexpr]
               @defproc[
 (numbered-list
  [tx-element txexpr?] ...)
 txexpr])]
These can now be easily defined using the @racket[make-list-function] helper.

@chunk[<bullet-list>
       (define bullet-list (make-list-function 'ul))]

@chunk[<numbered-list>
       (define numbered-list (make-list-function 'ol))]


@defproc[
 (btw
  [tx-element txexpr?] ...)
 txexpr]
Make the ``By the Way'' list that appears at the bottom of many pages, @link["http://typographyforlawyers.com/what-is-typography.html"]{like this one}.

Another example of using a tag function to handle fiddly HTML markup.
The @racket[btw] tag expands to an HTML list. We will then crack this open and slip in a @racket[div] for the headline.

@chunk[<btw>
       (define (btw . tx-elements)
         (define btw-tag-function (make-list-function 'ul '((class "btw"))))
         (define btw-list (apply btw-tag-function tx-elements))
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
Create a styled cross-reference link, with optional destination argument, so it can be used two ways:

@terminal{
 ◊xref{target}
 ◊xref["url"]{target}}

For this tag function, we'll assume that @racket[_target] is a single text argument, because that's how it will be used. But to be safe, we'll raise an error if we get too many arguments.

What makes this function a little tricky is that @racket[_url] is optional, but if it appears, it appears first. That makes this a good job for @racket[case-lambda], which lets you define separate branches for your function depending on the number of arguments provided. 

In the one-argument case, rather than duplicate the line of code in the two-argument case, we call the function again with a second argument.

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
Convert the target text of an @racket[xref] into a URL.

This function depends on a personal commitment to name source files in a logical, predictable way, e.g., ``Why Does Typography Matter?'' becomes @tt{why-does-typography-matter.html}. This way, the name of the source file for a page can be derived from its title.

If you needed to associate targets with URLs arbitrarily, you could store the targets and URLs in an association list or hashtable. But I prefer this approach, because it's easy to add new pages and cross-references, without the extra housekeeping step.

Well, almost. One wrinkle that arises is connecting singular and plural versions of the target text to the right URL. For instance, ``typewriter habit'' and ``typewriter habits'' should both link to @tt{typewriter-habits.html}. But ``point size'' and ``point sizes'' should both link to @tt{point-size.html}. Again, you could keep a list manually. But that's a drag. Instead, let's make the singular and plural versions of the target (called @racket[target-variants]) and compare these against a list of all possible HTML files in the project directory (called @racket[actual-filenames]). When we find a match, that will be the URL we're looking for.


@chunk[<target->url>
       (define (format-as-filename target)
         (define nonbreaking-space (string #\u00A0))
         (let* ([x target]
                [x (string-trim x "?")] ; delete a question mark at the end
                [x (string-downcase x)] ; put string in all lowercase
                [x (regexp-replace* #rx"é" x "e")] ; remove accented é
                [x (if (regexp-match "times new roman" x) "a-brief-history-of-times-new-roman" x)] ; special rule for TNR
                [x (if (regexp-match "about the author" x) "about" x)] ; special rule for about
                [x (if (regexp-match "foreword" x) "foreword" x)] ; special rule for foreword
                [x (if (regexp-match "table of contents" x) "toc" x)] ; special rule for toc
                [x (string-replace x nonbreaking-space "-")] ; replace nbsp with hyphen
                [x (string-replace x " " "-")]) ; replace word space with hyphen
           (format "~a.html" x)))

       (define (target->url target)         
         (define actual-filenames
           (map path->string (remove-duplicates (map ->output-path (directory-list (string->path "."))))))
         (define target-variants (let* ([plural-regex #rx"s$"]
                                        [singular-target (regexp-replace plural-regex target "")]
                                        [plural-target (string-append singular-target "s")])
                                   (list singular-target plural-target)))
         (or (for*/first ([tfn (in-list (map format-as-filename target-variants))]
                          [afn (in-list actual-filenames)]
                          #:when (equal? tfn afn))
                         tfn)
             (error (format "no URL found for ~v" target))))]


@defproc[
 (xref-font
  [font-name string?])
 txexpr?]
Special version of @racket[xref] for the @filepath{fontrec} subdirectory.

@chunk[<xref-font>
       (define (xref-font font-name)
         (xref (format "fontrec/~a" (format-as-filename font-name)) font-name))]


@defform[(define-heading heading-name tag-name)]
Macro for defining a function that makes a heading.

This could also be done with @racket[default-tag-function]. And as a rule of thumb, it's wise to reserve
macros for the times you can't avoid using them. Otherwise, use a function.

We'll bend that rule here because this is a quick & easy example macro. What makes it suitable to be
handled as a macro is that we want to use the name of the identifier (for instance @racket[topic]) as an
argument to the function. With a function, we can't do that. But with a macro, we can.

@racket[define-syntax-rule] is the easiest macro form: essentially you're writing a code template with arguments that will be filled in when you invoke the macro. Notice how @racket[heading-name] appears in two roles: first as an identifier name, and then as a literal symbol.

@chunk[<define-heading>
       (define-syntax-rule (define-heading heading-name tag)
         (define heading-name
           (default-tag-function tag 
             #:class (symbol->string 'heading-name))))]

``Wait, why does @racket['heading-name] not produce the literal symbol @racket['heading-name]?'' The @racket['heading-name] syntax is just shorthand for @tt{(quote heading-name)}. Because this is a macro, the @racket[heading-name] inside this expression gets replaced with the value of the macro argument @racket[heading-name] before @racket[quote] is evaluated.


@deftogether[(
              @defproc[
 (topic
  [tx-element xexpr?] ...)
 txexpr?]
               @defproc[
 (subhead
  [tx-element xexpr?] ...)
 txexpr?]
               @defproc[
 (font-headline
  [tx-element xexpr?] ...)
 txexpr?]
               @defproc[
 (section
  [tx-element xexpr?] ...)
 txexpr?]
               @defproc[
 (chapter
  [tx-element xexpr?] ...)
 txexpr?])]
Make a heading with a certain tag, using the @racket[define-heading] macro.

@chunk[<headings>
       (define-heading topic 'h3)
       (define-heading subhead 'h3)
       (define-heading font-headline 'h3)
       (define-heading section 'h2)
       (define-heading chapter 'h1)]

@defform[(define-heading-from-metas heading-name)]
Macro for defining a function that makes a heading by pulling the page title out of the metas.

This macro relies on @racket[syntax-case] rather than @racket[define-syntax-rule]. It's a little more complicated, but also more flexible (and more idiomatic in Racket). @racket[define-syntax-rule] is actually a special-case version of @racket[syntax-case]. The best advice on learning macros is to start with @racket[syntax-case], because you can't live without it.

@margin-note{Greg Hendershott's @link["http://www.greghendershott.com/fear-of-macros/pattern-matching.html"]{Fear of Macros} is a great place to start if you're new to Racket macros.}

Otherwise this macro is similar to @racket[define-heading], except that we want to introduce a new identifier
with a different name, but based on the argument given to the macro. So if we pass @racket[topic] to the macro, it will define
an identifier called @racket[topic-from-metas]. You can't do that with @racket[define-syntax-rule].

@chunk[<define-heading-from-metas>
       (define meta-key-for-page-title 'title)
       (define-syntax (define-heading-from-metas stx)
         (syntax-case stx ()
           [(_ heading-name)
            (with-syntax ([heading-from-metas 
                           (format-id stx "~a-from-metas" #'heading-name)])
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
  [tx-element xexpr?] ...)
 txexpr?]
Convert a topic + subhead into one HTML markup unit. Notice the use of @racket[no-hyphens-attr], which we defined in the @secref["Values"] section.

@chunk[<hanging-topic>
       (define (hanging-topic topic-xexpr . tx-elements)
         (txexpr 'div (list '(class "hanging-topic") no-hyphens-attr)
                 (list topic-xexpr (list* 'p (list no-hyphens-attr) tx-elements))))]

@defproc[
 (quick-table
  [table-rows xexpr?] ...)
 txexpr?]
Make an HTML table using simplified notation. In HTML, wrapping every paragraph in <p> tags is a terrible and dull task. But formatting tables is even worse. This function lets you make simple tables using @litchar{|} to signify columns, and line breaks to signify rows.

@terminal{
 ◊quick-table{
  heading left | heading center | heading right
  upper left | upper center | upper right
  lower left | lower center | lower right}}

This function assumes that each row has the same number of columns. You could improve it to fill in blank cells in rows that need them.

The idea is to break down the input into table headings and cells, and then work back up, wrapping each layer in the appropriate tags.

@chunk[<quick-table>
       (define (quick-table . tx-elements)
         (define rows-of-text-cells
           (let ([text-rows (filter-not whitespace? tx-elements)])
             (for/list ([text-row (in-list text-rows)])
                       (for/list ([text-cell (in-list (string-split text-row "|"))])
                                 (string-trim text-cell))))) 
         
         (match-define (list tr-tag td-tag th-tag) (map default-tag-function '(tr td th)))
         
         (define html-rows
           (match-let ([(cons header-row other-rows) rows-of-text-cells])
             (cons (map th-tag header-row)
                   (for/list ([row (in-list other-rows)])
                             (map td-tag row)))))
         
         (cons 'table (for/list ([html-row (in-list html-rows)])
                                (apply tr-tag html-row))))]

@defproc[
 (pdf-thumbnail
  [pdf-path path-string?])
 txexpr?]
Create a thumbnail of a PDF that links to the PDF.

This function will only work properly if you have @tt{sips} on your system (= command-line image-processing program, included with OS X).

This shows how you can fold other kinds of project housekeeping into Pollen commands. Here, the function generates the thumbnail it needs when the page is compiled.

One disadvantage of this approach is that the thumbnail will always be generated on recompile, though you could put in some logic to avoid this (e.g., check the modification date of the PDF). In this case, @tt{sips} is fast enough that it's not bothersome.

@chunk[<pdf-thumbnail>
       (define (pdf-thumbnail-link pdf-pathstring)
         (define img-extension "gif")
         (define img-pathstring 
           (->string (add-ext (remove-ext pdf-pathstring) img-extension)))
         (define sips-command
           (format "sips -Z 2000 -s format ~a --out '~a' '~a' > /dev/null"
                   img-extension img-pathstring pdf-pathstring))
         (link pdf-pathstring (if (system sips-command)
                                  `(img ((src ,img-pathstring)))
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
A few convenience variants of @racket[pdf-thumbnail-link].

@chunk[<pdf-thumbnail-variants>
       (define (pdf-thumbnail-link-from-metas metas)
         (define-values (dir fn _) 
           (split-path (add-ext (remove-ext* (hash-ref metas 'here-path)) "pdf")))
         (pdf-thumbnail-link (->string fn)))
       
       (define (before-and-after-pdfs base-name)
         `(div 
           (div ((class "pdf-thumbnail"))
                "before" (br)
                ,(pdf-thumbnail-link 
                  (format "pdf/sample-doc-~a-before.pdf" base-name)))
           (div ((class "pdf-thumbnail"))
                "after" (br)
                ,(pdf-thumbnail-link 
                  (format "pdf/sample-doc-~a-after.pdf" base-name)))))
       
       (define (alternate-after-pdf base-name)
         `(div ((class "pdf-thumbnail"))
               "after (alternate)" 
               (br)
               ,(pdf-thumbnail-link 
                 (format "pdf/sample-doc-~a-after-alternate.pdf" base-name))))]

@defproc[
 (root
  [tx-element txexpr?] ...)
 txexpr?]
Decode page content.

In a  @seclink["Writing_with_Pollen_markup"
               #:doc '(lib "pollen/scribblings/pollen.scrbl")]{Pollen markup} source, the output is a tagged X-expression that starts with @racket[root]:

@terminal{(root (div ((class "headline")) "Page title") ...)}

Recall that every Pollen tag calls a function with the same name (if it exists, otherwise it just becomes a tag). This is also true of @racket[root].

@racket[root] has slightly special status inasmuch as it is the top tag of the X-expression, and thus the last tag function that will get called. Therefore, @racket[root] is a good place to put any processing that should happen once all the page content has been filled in.

Often, you'll want to use a @racket[decode] or @racket[decode-elements] function, which can recursively perform different kinds of processing on different types of page elements.

In this case, we'll use @racket[decode-elements] twice. First, we'll use it just to detect paragraphs. We'll do this so that they're treated as blocks (see @racket[block-txexpr?]) in the second phase, which does the rest of the processing.

@chunk[<root>
       (define (root . elems)
         (define elements-with-paragraphs 
           (decode-elements elems #:txexpr-elements-proc decode-paragraphs))
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
Helper function for @racket[root] decoder that handles hyphenation.

The basic @racket[hyphenate] function comes from the @racketmodname[hyphenate] module. We could attach @racket[hyphenate] to our @racket[root] decoder as a string processor rather than block processor. But we want to be able to handle our no-hyphens flag (aka @racket[no-hyphens-attr]), which is stored in the attributes of the X-expression. Therefore, we have to look at blocks, not just strings.

@chunk[<hyphenate-block>
       (define (hyphenate-block block-tx)
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

Because I'm a typography snob I like to push quotation marks into the margin a little bit when they appear at the left edge of a line (aka hanging quotes). This function just wraps left-hand quote marks in two little tags (@tt{push} and @tt{pull}) that I can then manipulate in CSS to get the effect.

@chunk[<make-quotes-hangable>
       (define (make-quotes-hangable str)
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
Helper function for @racket[root] decoder.

When I type an em dash in my sources, I will often leave a space around it, but I don't want spaces in the output, so this function removes them.

@chunk[<fix-em-dashes>
       (define (fix-em-dashes str)
         (let* ([str (regexp-replace* #px"(?<=\\w)[\u00A0\u2009\\s]—" str "—")]
                [str (regexp-replace* #px"—[\u00A0\u2009\\s](?=\\w)" str "—")])
           str))]


@subsubsection{Miscellaneous tag functions}

Presented without docs or comment, as it should be obvious at this point what they do.

@chunk[<misc-functions>
       (define omission (default-tag-function 'div #:class "omission"))
       
       (define mono (default-tag-function 'span #:class "mono"))
       
       (define font-details (default-tag-function 'div #:class "font-details"))
       
       (define mb-font-specimen
         (default-tag-function 'div #:class "mb-font-specimen" #:contenteditable "true"))
       
       (define (margin-note . xs)
         `(div ((class "margin-note") ,no-hyphens-attr) ,@xs))
       
       (define os (default-tag-function 'span #:class "os"))
       
       (define (gap [size 1.5])
         `(div ((style ,(format "height: ~arem" size)))))
       
       (define (center . xs)
         `(div ((style "text-align:center")) ,@xs))
       
       (define (indented #:hyphenate [hyphenate #t]  . xs)
         `(p ((class "indented"),@(if (not hyphenate) (list no-hyphens-attr) null)) ,@xs))
       
       (define caption-runin (default-tag-function 'span #:class "caption-runin"))
       
       (define caption (default-tag-function 'span #:class "caption"))
       
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

This last incantation is needed so this @racketmodname[scribble/lp2] document knows how to put together all the code chunks we've introduced in this file.

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
