============
 Vala Mode2
============
:Authors:
  Robert Crowther
:Version:
  Alpha

Or, the weird one. This is not the comprehensive, standards-savvy, up-to-date mode which Vala, at this point, deserves.

However, the author only uses EMACS in  a trivial way, to format and hunt down typographical errors, and has never coded in a LISP before. So he has little sadness (no regrets).

Knowing very little about EMACs mode coding, and up against Vala's c-family syntax, the author made eccentric decisions. One of these, for example, was not to extend from CC mode (on which 'c-mode', 'java-mode' and several others are based). Without that code, the indentation is crude. But (the author shrugs his shoulders), some of this may turn out to be good for hunting down errors and formatting code. That's what matters.


To Consider
===========
Do not rely on the code, please assess for yourself. Comments are welcome.

*Please note:* the mode was initially designed using space-indenting.  Though tabbed files should display ok, tabbing is in an alpha state.

(author note: need more information on how to configure EMACS...)


Installation
============
The mode is not in repositories. It must be run in EMACS v24 (or above). If you have been using other vala-modes, it may be a good idea to remove them.


1. Download the files, or, ::

    git clone https://github.com/rcrowther/vala-mode2.git

   They are in an EMACS package (from github, an uncompressed folder).

2. Put the files somewhere

   Sensible places are in a local directory. If you have the directory, ::

    .emac.d/

   is a good place (but not in any subdirectory of that folder).

3. Initialize in EMACS

   Include the following in your Emacs config file. ::

    (add-to-list 'load-path "/path/to/vala-mode2/")
    (require 'vala-mode2)

   Note that the load-path string element must point to the directory - the search will not probe recursively.
   
There is no way known to the author of loading local directory contents into EMACS interfaces and/or the packaging system. Even when manually loaded, the results will not appear in ``M-x package-list-packages``.

Besides the features listed below, all of which run via standard EMACS commands, there is one command of note, to change indenting style, ::

  M-x  vala-set-style <stylename>



Features
========
Several of the eccentricities came about because the author chose simple detection heuristics. They're not accurate.


Parsing of highlighting
-----------------------
Operators are not highlighted
  This does not seem important in Vala. Operators are few in number, easy to spot, not a source of error. And, like most c-family languages, Vala ambiguously reuses operator characters for many purposes. Not highlighting operators helps.

Variables are not highlighted, nor are the types in variable declarations
  Like GNU c styles, variables in Vala are declared mostly at the top of methods. The author finds highlighting them confuses identification of parameters and the body of methods. Also, finding them is rather expensive (how do you tell a field from a method?). As for the c-family syntax round types, well...

Most constants are not highlighted
  Does this really help? Though string and builtin literals are highlighted.

No effort is made to break down strings
  No detection of escape chars, etc. The author rarely finds them a source of error (last words of author as he vanishes in smoke).

A distinction is made between 'Object Orientated', and other, keywords
  OO modifying keywords, such as ``static`` and ``protected`` are highlighted using a different face.


Fontlocking (colour highlighting)
---------------------------------
Namespacing is highlighted
  Yes, it is. Try it.

Types are highlighted inconsistently
  Types are not highlighted in method bodies. Except when they create ``new`` types.

Keywords are split into OO modifiers and minor
  To the author, there's a difference

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
  (default 'red') ...despite the EMACS Lisp background. 

Block comments must match Valadoc form
  i.e (no text after opening with "/**"). If not, they highlight as strings (default green).


Verbatim and literal strings
----------------------------
Full detection of verbatim and literal strings, cross-line. Imbalanced brackets usually spill (occasionally abbreviate) highlighting.


Spaces vs. tabs
---------------
On the argument of the ages, Vala is a generous language. Whitespace is whitespace, code parses, and there are no guidelines, though source is tabbed.

vala-mode2,
- detects if a file has been written using tabs or spaces (it looks at the first likely line).
- can change to tabs or spaces on the fly (but see below for how)
 
A first install of EMACS is likely using tabs. In general, for spaces only, put,::

  ;; Use only spaces (no tabs at all).
  (setq-default indent-tabs-mode nil)

in an EMACS startup file. Or change using the interface, ::

  M-x customize-mode > vala > indent-tabs-mode

But remember, vala-mode2 will try to detect the tab mode of a file, so this default will only apply to new, or an occasionally undetectable, file.


To change the tab mode immediately, use,

  M-x set-variable > indent-tabs-mode

The customizable interface is probably not what you want. It changes the default tab mode but, despite appearances, not the current buffer,::

  C-h v > indent-tabs-mode > customize > indent-tabs-mode

or,::

  M-x customize-mode > vala > indent-tabs-mode

If changed for the session, this will change default for any file subsequently *opened* in the same session.




Tab width is set with the variable, ::

  tab-width

If altered on the fly, vala-mode2 simply starts tabbing with the new width.




.. _Indenting:

Indenting
---------
Currently, the indenting code is simplistic. The code can differentiate between outer/method indents and braces. What it can not do is identify inner code structures such as if..then run-ons (though it does indent 'throw', and 'switch' statement bodies).

Still, it can do some sort of simulation of various styles. To see the current setting ::

  C-h > v > vala-indentation-style <stylename>

vala-mode2 can change styles on the fly (which is either fun or frustrating, dependant on your expectations). Change indentation style, ::

  M-x customize-mode > vala

From there, a preset can be selected or, if 'custom' is selected, indents can be set directly. ::
 
  M-x vala-set-style <stylename>

``stylename`` is a short descriptive string. To see valid entries for this function, and how they indent, use the customize interface, or look in the file 'vala-style.el' for the function 'vala-style:presets'. The current list is,::

  gnu, 1tbs, k&r, allman, stroudstrup, whitesmith, linux, ais

All the common styles are rough approximations, not guaranteed formatting. 'ais' is an invented style - 'as if syntax' - brackets are treated as syntax and the indent is 2 spaces everywhere.

The customization interface is recommended. Other methods (e.g. progamatic) have undefined or untested behaviour.


Fill functions
--------------
The fill functions protect against fill commands (M-q etc.) altering anything but comments and strings.

Within block comments, the fill functions recognise some valadoc markup formation, seeing headings, lists, code blocks, and annotations as paragraphs. So they will refuse to concatenate these lines with previous lines.

List item markups which do not match within a comment paragraph will concatenate.

Fill functions also work within simple and verbatim stings. In string fills markup is not recognised, and the fill works as a simple, no-prefix fill against the left side.


Whitespace cleanup
------------------
The mode cleans up some whitespace as the buffer is modified. Every time a line is indented, the mode strips whitespace from the end of the line. This idea generally works unobtrusively and to some useful effect.

The cleanup code is always on, can not be switched off.


Compiling
=========
The mode works a little faster if compiled. Use the Makefile, ::

make

and, ::

make clean

On the author's computer, this cut a test computation from 6 secs to 4 secs.


Notes for Emacs hackers and fans
================================
This mode is low on syntax detection. It can be expensive on CPU time. If anyone wants it faster, likely it can be made faster.

The disinterest in method bodies makes the highlighting look restrained on heavy code Vala files. On .vapi files, the highlighting looks like a seaside bucket.

Somewhat unusually, the mode will (except in strings and block comments) stop highlighting whenever it fails to understand syntax. And, in general, it reacts to Just-In-Time re-highlighting. The mode should not often cause "EMACS went wrong".


Beat the mode
=============
A diverting and EMACS-instructive pastime is to try confusing modes with code which legitimately passes a language parser, or passes the mode but fails a parser. For vala-mode2, try, ::

  "He's just—nae better than he should be"""

...ok, the mode does no balancing of string delimiters. ::

  int64 oh;
  protected interface
  int dear;
 
...not much syntax parsing here. Or, ::

  class this.Gtk.Nowhere {
    }

...Humm. The code for highlighting symbols is likely one unconstrained lump (it is).


Help
====
There is still plenty of development code in the source, and some non-working/semi-developed, semi-obscured features. So do not rely on the results of the following common EMACS commands.

For information, try 'describe-mode', ::

  C-h m

...currently uninteresting. 'help apros' is more useful, ::

  C-h a vala

User customizations are operative (see Indenting_) and can be seen in, ::

  M-x customize > Programming > Languages > Vala

or, ::

  M-x customize-mode > Vala


TODO
====
There's a TODO (with rough CHANGELOG and MAYBEPATH) in source but, publicly,

- The mode would be far more interesting if it handled tabs
- Many will want some sophistication added to the indenting
- The options and customization need help
- Colour schemes for > 8 bit terminals would be nice.


Acknowlegements
===============
This code started as a hack of scala-mode2 (umm, yes it was). These origins should not be taken as a guide to the quality of this code.

A couple of scala-mode2 ideas are still in there, such as the concatenating of comment list markup. Interesting mode, scala-mode2. 

