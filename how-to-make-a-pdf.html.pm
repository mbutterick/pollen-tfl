#lang pollen

◊(define-meta title "how to make a PDF")
◊(section-from-metas metas)


There’s a right way and a wrong way to make a PDF. Based on an unscientific survey of the PDFs I get from other lawyers, just about all of you are doing it the wrong way.

The wrong way: print the document on paper and scan it to PDF.

The right way: “print” the document directly to PDF.

◊subhead{How to print directly to PDF}

◊os{Windows} Install a printer driver that outputs PDFs instead of sending a file to a physical printer. If you have a commercial version of Adobe Acrobat (not just the free Acrobat Reader), the ‹Adobe PDF› driver should already be installed. If you don’t have Adobe Acrobat, numerous third-party PDF printer drivers are available. When you issue the print command, you’ll see the ‹Print› dialog box. At the top of this box is a popup menu listing the installed printers. Select your PDF printer. Set other options as needed and click ‹OK›.

◊os{OS X} You don’t need a special print driver — printing directly to PDF is built into OS X. Issue the ‹Print› command. The dialog box that appears has a button at the lower left labeled ‹PDF›. Click this button. From the menu that appears, select ‹Save as PDF›. In the next dialog box, enter a filename and click ‹Save›.

“What’s the difference? Either way, you end up with a PDF.” True. But one PDF is much better than the other.

When you print a document and then scan it to PDF, you’re defeating most of the benefits of using a PDF at all. Essentially, you’re making a series of photos of your document and packaging them inside a PDF. These photos occupy a lot of disk space, they’re slow to view or print, they have to go through optical character recognition (OCR) to be searchable, and any care you’ve put into typography will be diluted by the reduced quality of the scan.

But printing directly to PDF stores your document in a compact, high-resolution format. Instead of a series of photos, the document pages are stored as highly compressed digital data. These pages take up very little space on disk, are fast to view or print, are searchable without OCR, and preserve your typography with perfect fidelity.

“But my document has exhibits. How am I supposed to get those into the word-processing document?” You don’t. Print the word-processing document to PDF as described above. Then add the exhibits to the PDF using Acrobat or another PDF-editing tool.

Got it? Good.

◊btw{
Many lawyers rely on the built-in PDF generators in Word and Word- Perfect. For bad and arbitrary reasons, they only work with True- Type-format fonts, not OpenType, which is the established industry standard. I recommend that lawyers rely on the Adobe PDF maker because it doesn’t suffer from these deficiencies and makes the most reliable PDFs. If you insist on using your word processor’s PDF gen- erator, make sure you have the TrueType versions of the fonts.
}