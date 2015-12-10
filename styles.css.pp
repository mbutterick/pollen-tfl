#lang pollen

◊(require css-tools/column css-tools/core css-tools/transition)

◊(make-media-query 23 11 1000 40)   

◊(define default-margin "2.5rem")
◊(define paragraph-space "0.8rem")
◊(define anchor-hover-color "hsla(30, 20%, 90%, 1)")
* {
  padding: 0;
  margin: 0;
  border: 0;
  font-size: inherit;
  font-weight: inherit;
  box-sizing: border-box;
}


p {
  line-height: 1.4;
  margin-bottom: ◊|paragraph-space|;
}

strong {
  font-weight: bolder;
}

h1, .home-link {
  text-transform: uppercase;
  font-family: "advocate-c43";
  font-size: 2rem;
  line-height: 1.1;
}

.home-image {
 width: 22rem;
 margin-top: 1rem;
 margin-bottom: 1rem;
}

.home-overlay, .home-link {
  background: rgb(250, 250, 250);
}

.home-link {
  display: inline-block;
  padding: 1rem;
  width: 22rem;
}

.home-overlay {
    background-size: 22rem;
    background-repeat: no-repeat;
    width: 22rem;
    overflow: auto;
    border: 1px solid gray;
     margin-top: 2rem;
     margin-bottom: 2rem;
}

.home-overlay-inner {
  background: rgb(250, 250, 250);
  opacity: 0;
  transition: opacity 0.3s;
  min-height: 23rem;
  }

.home-overlay-inner:hover {
  opacity: 0.98;
  transition: opacity 0.3s;
  }

.home-overlay-inner ul {
 padding: 0;
 margin: 2rem;
 margin-left: 3rem;
 margin-right: 1.5rem;
 height: 100%;
 }

.home-overlay-inner p {
  font-size: 85%;
  font-family: "concourse-t3";
}

.toc h3 {
  font-weight: bold;
  margin-bottom: 1rem;
}

.toc div {
  margin-bottom: 2rem;
}

.toc ul {
  width: 100%;
  margin-bottom: 2rem;
  -webkit-columns: 2;
}

.toc li {
  line-height: 1.2;
  margin: 0;
  margin-bottom: 0.5rem;
  display: inline-block; ◊; prevents borders from wrapping between columns
  width: 100%; ◊; makes each li take one row
}



.chapter {
  font-family: "advocate-c43";
  text-transform: uppercase;
  letter-spacing: 0.07rem;
  margin-top: 2.5rem;
  font-size: 2rem;
  margin-bottom: 5rem;
}

.section {
  font-family: "advocate-c43";
  font-weight: normal;
  font-size: 170%;
  text-transform: uppercase;
  letter-spacing: 0.07rem;
  margin-top: 2rem;
  font-size: 1.7rem;
  margin-bottom: 4rem;
}

.section + p:first-line {
  font-weight: bolder;
  font-family: "equity-caps";
  ◊(make-css-small-caps)
}

.font-headline {
  font-family: "advocate-c43";
  font-size: 180%;
  margin-bottom: 1rem;
  text-transform: uppercase;
}

.dmb:after{
    content: "the TFL font collection";
    weight: normal;
    font-size: 70%;
    margin-left: 1rem;
    ◊(make-css-small-caps)
}

.hanging-topic, .margin-note {
  position: absolute;
  left: 2rem;
  width: 7rem;
  float: left;
  text-align: right;
}

.margin-note {
  font-size: 82%; 
  margin-bottom: 1rem; ◊; prevents two asides from adjacency 
  font-family: "concourse-t3";
  line-height: 1.35;
  color: #666;
}

.topic {
  font-weight: bolder;
  margin-top: 0.1rem;
  margin-bottom: 0.8rem;
  font-size: 110%;
  line-height: 1.1;
}


.hanging-topic p {
  font-style: italic;
  font-size: 95%;
  line-height: 1.2;
}

~h2.section + p:first-line {
  font-family: "equity-caps";
  font-weight: bolder;
  text-transform: lowercase;
  ◊make-css-ot-features['("c2sc") '(1)]
}


html {
    height: 100%;
}

body {
  position: relative; ◊; this establishes body as reference container 
  padding: 0;
  margin-left: auto;
  margin-right: auto;
  width:100%;
  max-width:1000px;
  min-width:520px;
  min-height: 100%;
  ◊(make-css-kerning)
  ◊(make-css-ligatures)
  color: #444;
}

◊(define body-left-margin "11rem")

body > * {
font-size: 100%;
margin-left: ◊|body-left-margin|;
margin-right: ◊|default-margin|;
}


◊(define content-width "29rem")
#content {
    border-top: 0.3rem solid ◊|content-rule-color|;
    border-bottom: 0.3rem solid ◊|content-rule-color|;
    width: ◊|content-width|;
    padding: 3rem 0 8rem 0; 
}




#doc {
  font-family: "equity-text";
}

h2 {
  font-weight: bolder;
}


em {
  font-style: italic;
}

a {
  text-decoration: none;
  color: inherit;
  border-bottom: 1px dotted #aba3a3;
}

a:hover {
  background: ◊|anchor-hover-color|;
}

a, a:hover {
  transition: background 0.2s;
}

.xref {
  font-family: "equity-caps";
  ◊(make-css-small-caps)
}


img {
  width: 100%;
}

.bordered {
  border: 1px solid gray;
}


p + .indented {
  margin-top: 1rem;
}

.indented + p {
  margin-top: 1rem;
}

.indented, .book-description {
  margin-left: 2.5rem;
  margin-right: 2.5rem;
  margin-bottom: ◊|paragraph-space|;
}

.book-description {
  margin-bottom: 1.2rem;
}

font-desc {
  display: block;
  font-size: 75%;
  -webkit-columns: 2;
}

font-desc a.xref {
  font-family: "concourse-c6";
}

ul.subnav {
  ◊make-css-columns[#:count 2]
}

ul.subnav li {
  display: inline-block;
  width: 100%;
  margin-bottom: 0.8rem;
}

table.captioned {
  width: 90%;
}

table.captioned + table.captioned {
  margin-top: -0.5rem;
}

table.captioned td {
    border: 0;
    vertical-align: baseline;
}

.caption, .caption-runin {
  font-family: "concourse-c4";
}

.caption-runin {
    margin-right: 0.3rem;
}

.caption {
  right: 4rem;
  border: 0;
}

.os {
  font-family: "concourse-c6";
  ◊(make-css-small-caps);
}

.os:after {
  content: " | ";
}

sig {
  display: block;
  margin-top: 1.5rem;
  text-align: right;
  font-family: "equity-caps";
  ◊(make-css-small-caps)
}

sig:before {
  content: "— ";
}


.omission {
  height: 0.2rem;
  border-bottom: 1px dashed gray;
  margin-bottom: ◊|paragraph-space|;
}

ol li {
◊; see firefox.css for an override of this value.
margin-left: ◊|default-margin|; ◊; tweak this based on list-item font 
padding-left: 0rem; 
list-style-position: outside;
font-family: "concourse-t3-index"; ◊; changes font for list indexes
◊make-css-ot-features['("liga" "ss01") '(1 0)]
}

ol li p {
  font-family: "equity-text"; ◊; changes font back for body of list item
}

ul.btw {
  margin-top: 2rem;
}

#btw-title {
display: block;
text-align: center;
font-family: "concourse-c6";
font-size: 80%;
◊(make-css-caps)
font-weight: bolder;
letter-spacing: 2px;
margin-bottom: 1em;
padding-bottom: 0.6em;
border-bottom: 4px double gray;
margin-left: 35%;
margin-right: 35%;
}

.btw li {
list-style: none;
margin-left: 1.35rem;
padding-left: 1.15rem; ◊; adds to 2.5rem, same as margin-left for ol li 
margin-bottom: 0.7em;
font-size: 95%;
}

.btw li:before {
font-family: "concourse-t3-index";
content: "•"; ◊; using concourse-t3-index, this will come out as an arrow
float:left;
margin-left: -2.5rem;
margin-top: 0.1rem;
}


.subhead {
  font-family: "equity-caps";
  font-weight: bolder;
  ◊(make-css-small-caps)
  margin-top: 1.2rem;
  margin-bottom: 0.5rem;
}

.mono {
  font-family: "triplicate-t4";
}


.glyph:before {
  content: "( ";
}

.glyph:after {
  content: " )";
}


.madlib {
  display: inline-block;
  font-family: "concourse-c6";
  font-size: 50%; 
  font-weight: bolder;
  position: relative;
  top: 0.35rem;
  padding-left: 0.2rem;
  padding-right: 0.2rem;
  text-transform: uppercase;
}

.font-details {
  padding-top: 0.5rem;
  margin-top: 1.5rem;
  margin-bottom: 1.5rem;
  padding: 1rem 1rem 0.5rem 1rem; 
  border-top: 1px solid gray;
  border-bottom: 1px solid gray;
  background: #f6f6f6;
  font-family: "concourse-t3";
  line-height: 1.40;
}

.font-details p {
    font-size: 80%;
}

table { ◊; basic table format, based on shortcut table 
margin-bottom: 1.5em;
border-collapse: collapse;
width: 95%;
}

table a {
  border: 0;
}

◊; technique for getting inner borders only 
tr + tr + tr { 
border-top: 1px solid gray;
}
td + td { 
border-left: 1px solid gray;
}


td {
padding: 0.4em;
text-align: center;
}

.buy-table {
    width: 100%;
    margin-top: 1.5rem;
}

th {
font-family: "concourse-c4";
font-weight: normal;
text-transform: lowercase;
font-size: 85%;
padding: 0.3rem 0.5rem 0.3rem 0.5rem;
line-height: 1.05;
}



.buy-table td {
    padding: 0;
    height: 3.5rem;
    border: 0;
}

a.buylink, .buy-table td a {
    height: 100%;
    width: 100%;
    background: #a33;
    color: white;
    padding: 0.3em 0.5em 0.3em 0.5em;
    border-radius: 1em;
    font-family: "concourse-t3";
    ◊(make-css-caps);   
    letter-spacing: 0.04rem;
    border: none;
}

.font-sample a.buylink {
  font-size: 80%;
}

.font-sample a.buylink:before {
  content: "available from "
}

.font-sample a[href$="pdf"] {
    display: inline-block;
    border: 2px solid gray;
    width: 70%;
}

.font-sample h3 {
    font-family: "advocate-c43";
    font-size: 170%;
    font-weight: normal;
    text-transform: uppercase;
    width: 71%;
    margin-bottom: 3rem;
}

a.buylink:hover, .buy-table td a:hover {
    background: #e33;
    text-decoration: none;
}

a.buylink:active, .buy-table td a:active {
    background: #ccc;
}



.mb-font-specimen {
font-size:150%;
line-height:1.20;
white-space:nowrap;
◊(make-css-editable);
}




◊(define double-quote-width .50)
◊(define single-quote-width (/ double-quote-width 2))

dquo, dquo-pull {
margin-left: ◊(- double-quote-width)em;
}

dquo-push {
margin-left: ◊(+ double-quote-width)em; 
}

squo, squo-pull {
margin-left: ◊(- single-quote-width)em;
}

squo-push {
margin-left: ◊(+ single-quote-width)em;
}

◊; hanging quotes don't work on right-aligned blocks
.margin-note dquo-push, .margin-note squo-push {
  display: none;
} 
.margin-note dquo-pull, .margin-note squo-pull {
  margin-left: inherit;
} 





◊;;;;;;;;;;;;;;;;;;;;;;;;;
◊; Special styles for -main.html template
◊;;;;;;;;;;;;;;;;;;;;;;;;;

◊(define nav-width "3rem")
◊(define big-nav-width "8rem")
◊(define border-width "0px")


.nav-inner {
    border: solid ◊border-width grey;
    position: fixed;
}


#bottom .nav-inner {
    opacity: 0.95;
    position: absolute;
}


#navtable #left .xref:before {
  content: "← ";
}

#navtable #right .xref:after {
  content: " →";
}

#navtable {
  margin: 0;
  height: 100%;
  width: ◊|content-width|;
}

#navtable td {
  vertical-align: top; ◊; otherwise vertical centering happens
  padding: 0; ◊; to allow links to fill whole td
}

#navtable a.xref {
  display: block;
  box-sizing: content-box; ◊; must override border-box setting here
  padding: 0.3rem;
  padding-bottom: 0.6rem;
  height: 100%;
  background: none;
  color: black;
  line-height: 1.1;
  font-size: 85%;
  opacity: 0.5; ◊; this works better than straight gray, which Webkit renders heavy
}

#navtable a.xref:hover {
      opacity: 1;
}

#navtable td:hover, #tfl-fonts-nav tr + tr td:hover {
      ◊make-css-background-gradient[`("#ffffff" ,anchor-hover-color) '("17%" "100%") #:radial #t]
}

#navtable a.xref, #navtable a.xref:hover {
  transition: opacity 0.2s;
}


div.pdf-thumbnail {
  font-family: "concourse-c6";
  display: inline-block;
  width: 40%;
}

div.pdf-thumbnail + div.pdf-thumbnail  {
  margin-left: 10%;
}


.pdf-thumbnail a {
  display: inline-block;
  background: inherit;
  margin-top: 1rem;
  margin-bottom: 1rem;
}


◊; move the hover style onto the thumbnail so it's snug
.pdf-thumbnail a, .pdf-thumbnail a:hover {
  border: none;
}

.pdf-thumbnail img {
  border: 2px solid #ddd;
}

.pdf-thumbnail img:hover {
  border: 2px solid black;
}


.nav-inner, .nav-inner:hover {
  color: ◊|anchor-hover-color|;
}

.nav-inner:hover {
  opacity: 1;
}

.nav-outer, .nav-inner {
  margin: 0;
  padding: 0;
}

◊; flex layout for side nav (to center glyphs vertically)
.nav-inner {
  display:flex;
  flex-direction: row;
  flex-wrap: nowrap;
  justify-content: space-between;
  align-content: center;
  align-items: center;
  opacity: 0;
}

.nav-flex {
  width: 100%;
  text-align: center;
  font-size: 3.5rem;
  font-weight: bolder;
}

◊; side nav hover
.nav-inner, .nav-inner:hover {
  transition: opacity 0.2s;
}

#top .nav-inner {
left: 0;
width: ◊|big-nav-width|;
top: 0;
height: ◊|big-nav-width|;
}

#up .nav-inner {
left: ◊|big-nav-width|;
right: 0;
top: 0;
height: 3rem; ◊; make this smaller so it doesn't overlap content
}

#prev .nav-inner {
left: 0;
top: 0;
bottom: 0;
width: ◊|nav-width|;
}

#next .nav-inner {
right: 0;
top: 0;
bottom: 0;
width: ◊|nav-width|;
}


#bottom .nav-inner {
left: ◊|body-left-margin|;
right: ◊|default-margin|;
max-width:1000px; ◊; same as body
min-width:520px; ◊; same as body
}

#bottom a {
  border-bottom: inherit;
}



a.pdf, a.pdf:hover {
  border: none;
  background: none;
}

a.pdf img:hover {
  background: #f6f6f6;
}



