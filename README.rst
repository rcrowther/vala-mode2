============
 Vala Mode2
============
:Authors:
  Robert Crowther
: Version:
  Alpha


About
=====
This is not the comprehensive, standards-savvy, up-to-date mode which Vala, at this point, probably deserves.

However, the author only uses EMACS in  a trivial way, to format and hunt down typographical errors, and has never coded in a LISP before. So he feels only a little sadness about this, and no regrets.

Knowing very little about EMACs mode coding, and up against Vala's c-family syntax, the author made eccentric decisions. One of these, for example, was not to extend from CC mode (on which `c-mode', `java-mode` and several others are based). Without that code, the indentation is crude. But (the author shrugs his shoulders), some of this may turn out to be good for hunting down errors and formatting code. That's what matters.


To Consider
===========
Do not rely on the code, please assess for yourself. Comments are welcome.

Please note the mode *only* works with spaces. Conceptually it can work with tabs, but will this not be implemented unless the author uncovers more information on how to configure EMACS (current code and implementation information, not blogs on keystrokes).


Installation
============
The mode is not in repositories. This mode must be run in EMACS v24 (or above). If you have been using other vala-modes, it may be a good idea to remove them.


1. Download the files, or, ::

    git clone git://github.com/rcrowther/vala-mode2.git

  They are in an EMACS package (from github, uncompressed).

2. Put the files somewhere

   Sensible places are in a local directory. If you have the directory, ::

    .emac.d/

   is a good place (but not in any subdirectory of that folder).

3. Initialize in EMACS

   Manually,
     Include the following in your Emacs config file. ::

             (add-to-list 'load-path "/path/to/vala-mode2/")
             (require 'vala-mode2)

   Via EMACS interfaces
     load-file (type 【Alt+x】) 



list-packages. Then i, then x
  M-x package-install
package-install-file

Besides the features listed below, all of which run via standard EMACS commands, there is one command of note, to change indenting style, ::

  M-x  vala-set-style <stylename>



Features
========
Several of the eccentricities came about because the author chose simple detection heuristics. They're not even accurate.


Parsing of highlighting
-----------------------
Operators are not highlighted
  This does not seem important in Vala. Operators are few in number, easy to spot, not a source of error. And, like most c-family languages, Vala ambiguously reuses operator characters for many purposes. Not highlighting operators helps.

Variables are not highlighted
  Like GNU c styles, variables in Vala are declared mostly at the top of methods. The author finds highlighting them confuses identification of parameters and the body of methods. Also, spotting them all is rather expensive (how do you tell a field from a method?).

Most constants are not highlighted
  Does this really help? Though string and builtin literals are highlighted.

No effort is made to break down strings
  No detection of escape chars, etc. The author rarely finds them a source of error (then again, regex...).

A distinction is made between 'Object Orientated', and other, keywords
  OO modifying keywords, such as ``static`` and ``protected`` are highlighted using a different face.


Fontlocking (colour highlighting)
---------------------------------
Namespacing is highlighted
  Yes, it is. Try it.

Types are highlighted inconsistently
  Types are only highlighted when they create new types, or appear in method declarations.

Keywords are split into OO modifiers and minor
  To the author there's a difference

The highlighting rarely spills
  A byproduct of some of the coding. But the author don't regard this EMACS feature highly.

Three new fontfaces
  These are needed for the classifications of tokens.

Type highlighting can handle several forms
  arrays, namespacing, and generic declarations.

Handles `@` escaped keywords.

Detects code attributes.
 

Comments
--------
Comments are in comment-face 
  (default 'red') Despite the EMACS Lisp background. 

Block comments must match Valadoc form
  i.e (no text after opening with "/**"). If not, they highlight as strings (default green).


Verbatim and literal strings
----------------------------
Full detection of verbatim and literal strings, cross-line. Imbalanced brackets usually spill (occasionally abbreviate) highlighting.


Indenting
---------
Currently, the indenting code is simplistic. The code can differentiate between outer/method indents and braces, then indent accordingly. What it can not do is identify inner code structures such as if..then run-ons (though it does indent throw, and switch statement bodies as if they were bracketed).

Still, it can do some sort of simulation of various styles e.g. ITBS, and Allman. ::

  M-x vala-set-style <stylename>

to change.

Valid indenting styles can be viewed,


You should be able to set the variables directly. But the manual is hapless, help is thin on the ground, so what we have at the moment is the best it can be for now.

 
Fill functions
--------------
The fill functions protect against fill commands (M-q etc.) altering anything but comments and strings.

Within block comments, the fill functions recognise some valadoc markup formation, seeing headings, lists, code blocks, and annotations as paragraphs. So they will refuse to concatenate these lines with previous lines.

List item markups which do not match within a comment paragraph will concatenate.

Fill functions also work within simple and verbatim stings. In string fills markup is not recognised, and the fill works as a simple, no-prefix, fill against the left side.


Whitespace cleanup
------------------
The mode cleans up some whitespace as the buffer is modified. Every time a line is indented, the mode strips whitespace from the end of the line. This idea generally works unobtrusively and to some useful effect.

The cleanup code is always on, can not be switched off.


Notes for Emacs hackers and fans
================================
Not knowing how to use EMACS syntax data and parser, the author took a different route. The result is low on syntax detection. It can also be expensive on CPU time. If anyone wanted it faster, likely it could be made faster.

Somewhat unusually, the mode will generally (excerpt in strings and block comments) stop highlighting whenever it doesn't understand something. And, in general, it reacts to Just-In-Time re-highlighting. The mode should not often cause "EMACS went wrong".


Beat the mode
=============
A diverting and EMACS-instructive pastime is to try confusing modes with code that legitimately passes a language parser, or passes the mode but fails a parser. For vala-mode2, try, ::

  "He's just—nae better than he should be"""

...ok, the mode does no balancing of string delimiters. ::

  int64 oh;
  protected interface
  int dear;
 
...not much syntax parsing here. Or, ::

  class this.Gtk.Nowhere {
    }

...Humm. The code for highlighting symbols is likely one unconstrained lump (it is).



TODO
====
There's a TODO (with rough CHANGELOG and MAYBEPATH) in source but, publicly,

The mode would be far more interesting if it handled tabs
Many will want some sophistication added to the indenting
The options and customization need help
Colorschemes for > 8 bit terminals would be nice.


Acknowlegements
===============
This code started as a hack of scala-mode2 (umm, yes it was),


Though this should not be taken as any guide to the quality of this code. A couple of the ideas are still in there, such as the concatenating list markup. Interesting mode, scala-mode2. 
