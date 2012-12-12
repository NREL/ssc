Instructions for Generating SSC Documentation
=============================================

This document explains how to use Doxygen, LaTeX, and HeVeA to create the SSC API documentation.

Overview
--------

The SSC documentation consists of the 8 chapters in the ssc_guide.tex file.

The content for Chapters 1-7 is in ssc_guide.tex.

Doxygen generates content for Chapter 8 (the function reference for sscapi.h) in sscapi_8h.tex from comments in sscapi.h.

An _include_ command in the ssc\_guide.tex file inserts the conntent of sscapi\_8h.tex into ssc\_guide.tex.

Doxygen must be properly configured to generate only a single page with the function reference, and to generate a LaTeX document rather than HTML.

TeXworks generates the final PDF document from LaTeX.

HeVeA converts the PDF document to HTML.

Modify the CSS in the HTML document to format for the web.

Overall Steps for Maintaining Documentation
-------------------------------------------

1. Revise sections in ssc\_guide.tex and Doxygen-formatted comments in sscapi.h.

2. Run Doxygen to generate LaTeX document from Doxygen-formatted comments in sscapi.h file.

3. Run TeXworks to convert LaTex document to PDF.

4. Run Hevea to convert PDF to HTML.

Notes
-----

Run Doxygen on ssc\_api\_doxyconfig. Doxygen creates the ssc\_api\_latex folder.

Copy sscapi\_8h.tex and doxygen.sty from ssc\_api\_latex folder to the doc folder.

Open doxygen.sty in a text editor, and comment out the commands under "Setup fancy headings" so that the "Generated on date for SSC API by Doxygen" footer does not appear on pages.

Run TexWorks on ssc\_guide.tex to generate PDF.

Run HeVeA on ssc\_guide.pdf to generate HTML: hevea ssc\_guide.tex

Software Installation Instructions
----------------------------------

###LaTeX tools

###Doxygen

###HeVeA