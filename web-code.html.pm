#lang pollen

◊(define-meta title "Enter a web code")
◊(section-from-metas metas)
◊(define-meta toolbar-blank "true")
 
Type the code below and press return.

◊form[#:action "javascript:jumpToCode();" #:name "codeform"]{◊input[#:type "text" #:style "border: 1px solid gray;padding: 0.5em" #:name "codefield" #:size "8" #:autofocus "42"]}



