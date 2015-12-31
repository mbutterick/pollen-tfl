#lang pollen/mode racket/base

#|
UNIT TESTS

Testing, as always, is optional, but strongly recommended. Unit tests are little one-line tests that
prove your function does what it says. As you refactor and reorganize your code, your unit tests will
let you know if you broke anything.

You can make unit tests with the `rackunit` library. Though you can put your unit tests in a separate
source file, I generally prefer to put them close to the function that they're testing. (For details
on the testing functions used below, see the docs for `rackunit`)

The ideal way to do this is with a `test` submodule. The code in a `test` submodule will only be used
a) when you run the file in DrRacket or
b) when `raco test` runs the file.
Otherwise, it is ignored.

We'll use the `module+` syntax for this. As the name suggests, `module+` creates a submodule that
incorporates everything else already in the source file. Moreover, all of our `module+ test` blocks
will be combined into a single submodule.
|#

(module+ test
  (require rackunit txexpr "pollen.rkt") ;; always include this at the start of the test submodule
  
  ;; We use `check-txexprs-equal?` rather than `check-equal?` because it's a little more lenient:
  ;; it allows the attributes of two txexprs to be in a different order,
  ;; yet still be considered equal (because ordering of attributes is not semantically significant).
  (check-txexprs-equal? (link "http://foo.com" "link text")
                        '(a ((href "http://foo.com")) "link text"))
  
  ;; The last test was fine, but it can be even better if we use a Pollen-mode command on the left.
  ;; That way, we can directly compare the command as it appears in Pollen input
  ;; with how it appears in the output.
  (check-txexprs-equal? ◊link["http://foo.com"]{link text}
                        '(a ((href "http://foo.com")) "link text"))
  
  ;; It's wise to test as many valid input situations as you can.
  (check-txexprs-equal? ◊link["http://foo.com" #:class 'main]{link text}
                        '(a ((href "http://foo.com")(class "main")) "link text"))
  (check-txexprs-equal? ◊link["http://foo.com"]
                        '(a ((href "http://foo.com")) "http://foo.com"))
  
  ;; Strictly speaking, you could also write the last Pollen command like so:
  (check-txexprs-equal? ◊link{http://foo.com} '(a ((href "http://foo.com")) "http://foo.com"))
  
  ;; That's not wrong. But in the interests of code readability,
  ;; I like to reserve the curly brackets in a Pollen command
  ;; for material that I expect to see displayed in the output
  ;; (e.g., textual and other content),
  ;; and use the square brackets for the other arguments.
  
  ;; You can also check that errors arise when they should.
  ;; Note that when testing for exceptions, you need to wrap your test expression in a function
  ;; (so that its evaluation can be delayed, otherwise you'd get the error immediately.)
  ;; The `(λ _ expression)` notation is a simple way.
  ;; (The `_` is the idiomatic way to notate something that will be ignored, in this case arguments.)
  (check-exn exn:fail? (λ _ ◊link[])) ; no arguments
  (check-exn exn:fail? (λ _ ◊link[#:invalid-keyword 42])) ; invalid keyword argument
  (check-exn exn:fail? (λ _ ◊link[#f]))) ; invalid argument

;; For the sake of brevity, I'm going to write just one test for the remaining functions.
;; But you're encouraged to add more tests (or break the existing ones and see what happens).


(module+ test
  ;; notice that we use `buy-url` in our test result.
  ;; That way, if we change the value of `buy-url`, the test won't break.
  (check-txexprs-equal? ◊buy-book-link{link text} `(a ((href ,buy-url)) "link text")))

(module+ test
  (check-txexprs-equal? ◊buylink["http://foo.com"]{link text}
                        '(a ((href "http://foo.com")(class "buylink")) "link text")))

(module+ test
  (check-txexprs-equal? ◊home-link["http://foo.com"]{link text}
                        '(a ((href "http://foo.com")(class "home-link")) "link text")))

(module+ test
  (check-txexprs-equal? ◊image["pic.gif"]
                        '(img ((style "width: 100%") (class "bordered")(src "images/pic.gif"))))
  (check-txexprs-equal? ◊image[#:border #f "pic.gif"]
                        '(img ((style "width: 100%")(src "images/pic.gif"))))
  (check-txexprs-equal? ◊image[#:width "50%" "pic.gif"]
                        '(img ((style "width: 50%")(class "bordered")(src "images/pic.gif")))))

(module+ test
  (check-txexprs-equal? ◊div-scale[.5]{Hello} '(div ((style "width: 0.5")) "Hello")))

(module+ test
  (check-txexprs-equal? ◊font-scale[.75]{Hello}
                        '(span ((style "font-size: 0.75em")) "Hello")))

(module+ test
  (check-txexprs-equal? ◊home-image["pic.gif"]
                        '(img ((style "width: 100%") (class "home-image") (src "images/pic.gif")))))

(module+ test
  (check-txexprs-equal? ◊home-overlay["pic.gif"]{Hello}
                        '(div ((class "home-overlay") (style "background-image: url('pic.gif')"))
                              (div ((class "home-overlay-inner")) "Hello"))))

(module+ test
  (check-txexprs-equal? ◊glyph{X}
                        '(span ((class "glyph")) "X"))
  (check-txexprs-equal? ◊glyph[#:id "top"]{X}
                        '(span ((class "glyph")(id "top")) "X")))

(module+ test
  (check-txexprs-equal? ◊image-wrapped{my-path}
                        '(img ((class "icon")
                               (style "width: 120px;")
                               (align "left")
                               (src "images/my-path")))))

(module+ test
  (check-equal? (detect-list-items '("foo" "\n" "bar")) ; linebreak, not list item break
                '((li (p "foo" (br) "bar"))))
  (check-equal? (detect-list-items '("foo" "\n" "\n" "bar")) ; paragraph break, not list item break
                '((li (p "foo") (p "bar"))))
  (check-equal? (detect-list-items '("foo" "\n" "\n" "\n" "bar")) ; list item break
                '((li (p "foo")) (li (p "bar"))))
  (check-equal? (detect-list-items '("foo" "\n\n\n" "bar")) ; list item break, concatenated
                '((li (p "foo")) (li (p "bar"))))
  (check-equal? (detect-list-items '("foo" "\n" "\n" "\n\n\n" "bar")) ; list item break
                '((li (p "foo")) (li (p "bar")))))

(module+ test
  (check-txexprs-equal? ◊bullet-list{foo} '(ul (li (p "foo"))))
  (check-txexprs-equal? ◊numbered-list{foo} '(ol (li (p "foo")))))

(module+ test
  (check-txexprs-equal? ◊btw{foo
                             
                             
 bar}
                        '(ul ((class "btw"))
                             (div ((id "btw-title")) "by the way")
                             (li (p "foo"))
                             (li (p "bar")))))


(module+ test
  (check-txexprs-equal? ◊xref{target}
                        `(a ((class "xref") (href "target.html") ,no-hyphens-attr) "target"))
  (check-txexprs-equal? ◊xref["url"]{target}
                        `(a ((class "xref") (href "url") ,no-hyphens-attr) "target"))
  (check-exn exn:fail:contract:arity? (λ _ (xref "url" "target" "spurious-third-argument"))))

(module+ test
  (check-equal? (target->url "foo?") "foo.html")
  (check-equal? (target->url "FOO") "foo.html")
  (check-equal? (target->url "foé") "foe.html")
  (check-equal? (target->url "Foreword Lengthy Title") "foreword.html")
  (check-equal? (target->url "Table of Contents and Other Nonsense") "toc.html")
  (check-equal? (target->url "Nonbreaking Space and Spaces") "nonbreaking-space-and-spaces.html"))


(module+ test
  (check-txexprs-equal? ◊topic{foo}
                        '(h3 ((class "topic")) "foo"))
  (check-txexprs-equal? ◊subhead{foo}
                        '(h3 ((class "subhead")) "foo"))
  (check-txexprs-equal? ◊font-headline{foo}
                        '(h3 ((class "font-headline")) "foo"))
  (check-txexprs-equal? ◊section{foo}
                        '(h2 ((class "section")) "foo"))
  (check-txexprs-equal? ◊chapter{foo}
                        '(h1 ((class "chapter")) "foo")))

(module+ test
  (let ([my-fake-metas (hash 'title "Fake Title" 'white "noise")])
    (check-txexprs-equal? ◊topic-from-metas[my-fake-metas]
                          '(h3 ((class "topic")) "Fake Title"))
    (check-txexprs-equal? ◊section-from-metas[my-fake-metas]
                          '(h2 ((class "section")) "Fake Title"))
    (check-txexprs-equal? ◊chapter-from-metas[my-fake-metas]
                          '(h1 ((class "chapter")) "Fake Title"))))

(module+ test
  (check-txexprs-equal? ◊hanging-topic["Topic name"]{One-line explanation}
                        `(div ((class "hanging-topic") ,no-hyphens-attr) "Topic name"
                              (p (,no-hyphens-attr) "One-line explanation"))))

(module+ test
  (check-txexprs-equal?
   ◊(quick-table "heading-one | heading-two" "\n"
                 "   three | four" "\n"
                 "five | six   ")
   '(table (tr (th "heading-one") (th "heading-two"))
           (tr (td "three") (td "four"))
           (tr (td "five") (td "six")))))

(module+ test
  (check-txexprs-equal? (hyphenate-block `(div "snowman" (span (,no-hyphens-attr) "snowman")))
                        `(div "snow\u00ADman" (span (,no-hyphens-attr) "snowman"))))

(module+ test
  (check-txexprs-equal? (make-quotes-hangable "“Who is it?”")
                        '(quo "" (dquo-push) (dquo-pull "“") "Who is it?”")))

(module+ test
  (check-equal? (fix-em-dashes "Hey — you!") "Hey—you!")
  (check-equal? (fix-em-dashes "Hey—you!") "Hey—you!"))

(module+ test
  (check-equal? (capitalize-first-letter "foo dog") "Foo dog"))