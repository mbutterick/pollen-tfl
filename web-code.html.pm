#lang pollen
◊(require pollen/unstable/convert)

◊(define-meta title "Enter a web code")
◊(section-from-metas metas)
◊(define-meta toolbar-blank "true")
 
◊margin-note{}

Type the code below and press return.

◊html->xexpr{
<form name="codeform" action="javascript:jumpToCode();"> 
<input style="border: 1px solid gray;font-size:200%;font-family:advocate-c43;padding:.25em" class="codebox" name="codefield" size="8" type="text" />
</form>}

