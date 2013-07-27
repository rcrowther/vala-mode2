;;; vala-mode2-syntax.el - syntax table and functions for Vala Mode2             

;; Copyright (c) 2013 Robert Crowther                         

;; Authors: 2013 Robert Crowther                        
;; Keywords: c languages  
;; Package: vala-mode2                        

;;; For information on the License, see the LICENSE file      

;;; Commentary:                    

;;; Code:

;; currently required for propertize funtions
(require 'vala-mode2-faces)

;;;;
;;;; Regular expressions
;;;;

;;; Based on no specification whatsoever
;;; ...guesswork.

;;; A note on naming. Word lists ending with,
;;; '-raw'
;;;   Simple lists of keywords
;;; '-opt-re'
;;;   expanded through ELISP regex-opt, thus have grouping and symbol
;;;   delimiters. Note that these regexes may not be good for all
;;;   situations e.g. Vala has '@' escape on keywords, so try, 
;;; '-re'
;;;   regular expressions for general usage. Any prefix to '-re'
;;;   indicates usage e.g.
;;; '-grouped-re'
;;;   a regular expression with a group round it. Bear in mind that
;;;   subgroups may exist, C-x C-e (evaluate) to see full details.




;;
;; non-Vala re
;;

;; Vala delimiters, but no quotes
;; deprecated?
(defconst vala-syntax:delimiter-group ".,;")
 


;; line re

;;(defconst vala-syntax:empty-line-re
;;  "^[\t ]*$")

;;(defconst vala-syntax:empty-line-end-re
;;  "[\t ]*$")

(defconst vala-syntax:non-empty-line-grouped-re
  "\\(?:[;\n][ ]*[^;\n]\\)")




;; Vala symbol and non-code re

;; Symbol re
;; though conventions exist,
;; symbols are suspected to be this simple definition.
(defconst vala-syntax:vala-symbol-raw-re
  "[A-Za-z_][A-Za-z_0-9]*")


;; Vala symbol
(defconst vala-syntax:vala-symbol-re
 (concat "\\(" vala-syntax:vala-symbol-raw-re "\\)"))


;; Vala directives
;; TODO: This also covers script shebangs, which could be annoying?
;; that could be done in fontlock, which may be faster?
(defconst vala-syntax:preprocessor-directive-re
  "[\t ]*#[^\n]+$"
  "Match preprocessor directives.
Matches a whitespace intro, to eol")

;;(defconst vala-syntax:preamble-start-re
;;  "\#\! \\!#[\t ]*$")



;; Code attributes
(defconst vala-syntax:vala-code-attribute-start-re
  (concat "\\[" vala-syntax:vala-symbol-raw-re))



;; Strings
;; handled by the propertize function.



;; Comment re

(defconst vala-syntax:line-comment-start-re
  "//")


;; Valadoc block comments are distinctive, they start "/**" and nust
;; have no following text.
(defconst vala-syntax:block-comment-start-re
  "/\\*+[ \t]*$")


(defconst vala-syntax:comment-start-re
  "/[/*]"
  "Any comment start. Referenced in mode.el")


;; match block comments, with the special magic that
;; if the line is the first line of a block, group 1 will
;; be matched.
;; TODO: check if failed match is reliable?
(defconst vala-syntax:opening-grouped-comment-start-re
  "\\*+\\|\\(?:/\\(\\*+\\|/+\\)\\)"
  "Match the start of any comment.
Has a nested match which returns nil if this was a first line in
a block comment, else t")




;;
;; OO re
;;

;; Class (or classlike) definitions
;; followed by a definition of one symbol
(defconst vala-syntax:class-definition-keywords-raw
  '("class" "interface"))

(defconst vala-syntax:class-definition-keywords-opt-re
   (regexp-opt vala-syntax:class-definition-keywords-raw 'symbols))
;;(looking-at vala-syntax:classlike-definition-keywords-and-bracketing-re)


(defconst vala-syntax:classlike-definition-keywords-raw
  '("struct" "enum" "namespace"))

(defconst vala-syntax:classlike-definition-keywords-opt-re
   (regexp-opt vala-syntax:classlike-definition-keywords-raw 'symbols))
;;(looking-at vala-syntax:classlike-definition-keywords-and-bracketing-re)


(defconst vala-syntax:level-indenting-keywords-raw
  '("class" "interface" "struct" "enum" "namespace")
  "This re includes namespace too")

(defconst vala-syntax:level-indenting-keywords-opt-re
   (regexp-opt vala-syntax:level-indenting-keywords-raw 'symbols))

(defconst vala-syntax:level-indenting-keywords-and-bracketing-re
  (concat "[{(/*]\\|\\(?:"
          vala-syntax:level-indenting-keywords-opt-re
          "\\)"))

;; Modifiers used for class, method and field descriptions.
; What is partial/params?
; TODO: throw and new are things to do with objects, not modifiers?
; throws is also some sort of intent about an object
; Note that new is also here due to the ambiguity of
; it being an subclass-method hide declaration.
; TODO: var works here, but should it be?
; signals and delegates have returns, like a method.
(defconst  vala-syntax:oo-modifier-keywords-raw
  '("internal" "private" "protected" "public" "partial"
                "const" "static" "async"
                "abstract" "virtual" "override"
                "params"
                "weak" "owned" "unowned"))


(defconst  vala-syntax:oo-modifier-keywords-opt-re
  (regexp-opt vala-syntax:oo-modifier-keywords-raw)
  "keywords for modifying OO structures/nodes.
note: not 'symbols. Bounded elsewhere.")

(defconst  vala-syntax:oo-modifier-keywords-seq-re
  (concat "\\(?:" vala-syntax:oo-modifier-keywords-opt-re "[ ]+\\)+")
  "Match a sequence of modifiers. Overruns following space, but ok.")


;; Keywords for OO access
;;(defconst  vala-syntax:access-modifier-keywords-raw
;;  '("internal" "private" "protected" "public"))

;;(defconst  vala-syntax:access-modifier-keywords-opt-re
;;  (regexp-opt vala-syntax:access-modifier-keywords-raw 'words))


;; Keywords which reference class heirarchies
(defconst  vala-syntax:class-tree-keywords-raw
  '("base" "this"))

(defconst  vala-syntax:class-tree-keywords-opt-re
  (regexp-opt vala-syntax:class-tree-keywords-raw 'symbols))




;; Param directions. They prefix params, modifiy behavior
;; TODO: rename to parameter modifiers.
;; May need others - weak, unowned?
(defconst  vala-syntax:param-directive-opt-re
  (regexp-opt '("ref" "out" "owned")))

;;TODO: keywords missing here?
(defconst  vala-syntax:param-directive-keywords-seq-re
  (concat "\\(?:" vala-syntax:param-directive-opt-re "[ ]+\\)+")
  "Match a sequence of modifiers. Overruns following space, but ok.")



;;
;; Keyword re
;;
;; Some of these are put in seperate groups so they can be parsed for
;; indenting and fontlocking.


;; Primitive types
;; unused, but could be used for a simple fontlock.
(defconst vala-syntax:primitive-type-keywords-raw
 '("void" "bool" "char" "uchar" "short" "ushort" "int" "uint" "long" "ulong"
   "size_t" "ssize_t" "int8" "uint8" "int16" "uint16" "int32" "uint32"
   "int64" "uint64"
   "unichar" "float" "double" "string"))


;; Keywords to reference classes externally
(defconst  vala-syntax:class-ref-keywords-raw
  (regexp-opt '("using" "extern")))


;; Value keywords
;;TODO: should void be in here too?
(defconst  vala-syntax:value-keywords-raw
  '("true" "false" "null"))

(defconst vala-syntax:value-keywords-opt-re
  (regexp-opt vala-syntax:value-keywords-raw))


;; Gee material
(defconst  vala-syntax:gee-method-keywords-raw
  '("add" "remove" "addAll"))

(defconst  vala-syntax:gee-method-keywords-opt-re
  (regexp-opt vala-syntax:gee-method-keywords-raw 'words))



;; catch group for unclassified keywords.
(defconst  vala-syntax:unclassified-keywords-raw
  '("sizeof" "typeof"
    "for" "foreach" "in"
    "switch" "lock"
    "return" "continue" "break" "yield"
    "throw" "var" 
    ))

(defconst  vala-syntax:unclassified-keywords-opt-re
  (regexp-opt vala-syntax:unclassified-keywords-raw 'words))



;;
;; Parsing significant keyword re
;;

;; keywords for ops followed by a declaration of the class to be used
;; for the operation.
;; TODO: doesn't cover static type casting?
;; Unused, but could be
;;(defconst vala-syntax:op-keyword-followed-by-class-declaration-raw
;;  '( "is" "as" "throws" "new"))

;;(defconst vala-syntax:op-keywords-followed-by-class-declaration-re
;;  (regexp-opt vala-syntax:op-keyword-followed-by-class-declaration-raw))


;; Some builtins maybe don't always require a brace
;; these are identified...
(defconst  vala-syntax:maybe-brace-keywords-raw
  '("set" "get" 
    "if" "then" "else" 
    "while" "do"
    "try" "catch" "finally"
    "construct"
    ))

(defconst vala-syntax:maybe-brace-keywords-opt-re
  (regexp-opt vala-syntax:maybe-brace-keywords-raw))


;; Keywords introducing conditional expressions with
;; irregular syntax.
(defconst  vala-syntax:irregular-conditional-keywords-raw
  '("case" "default"))

(defconst vala-syntax:irregular-conditional-keywords-opt-re
  (regexp-opt vala-syntax:irregular-conditional-keywords-raw))



;;
;; Keyword collections
;;


;; Non-propertized keywords
;; group for keywords with no propertize action.
(defconst vala-syntax:non-propertized-keywords-opt-re
;;  Used for syntactical rejection
  ;; the keyword groups missing are
  ;; - anything to do with OO definitions
  ;; - parameter definitions
  ;; - class referencing keywords (using/this ...)
  (regexp-opt
   (append 
    vala-syntax:maybe-brace-keywords-raw
    vala-syntax:irregular-conditional-keywords-raw
    vala-syntax:value-keywords-raw
    vala-syntax:gee-method-keywords-raw
    vala-syntax:unclassified-keywords-raw
    )
   'symbols))


(defconst vala-syntax:builtin-function-and-operator-keywords-opt-re
  ;; Used for fontlocking
  ;; the keyword groups missing are
  ;; - value keywords
  ;; - anything to do with OO definitions
  ;; - parameter definitions
  ;; - class referencing keywords (using/this ...)
  (regexp-opt
   (append 
    vala-syntax:maybe-brace-keywords-raw
    vala-syntax:irregular-conditional-keywords-raw
    vala-syntax:gee-method-keywords-raw
    vala-syntax:unclassified-keywords-raw
    )
   'symbols))

;;vala-syntax:non-propertized-keywords-opt-re


;; Several of these are defined here but unused in this file.
;; See vala-mode2-fontlock.



;;
;; Syntactical newline re
;;

(defconst vala-syntax:syntactical-newline-keyword-opt-re
  (regexp-opt '("new" "throws") 'symbols)
  "Match keywords defined, for propertizing purposes, as
newlines. This regexp is bound by symbols.")
;;(looking-at vala-syntax:syntactical-newline-keyword-opt-re)


(defconst vala-syntax:syntactical-newline-re
  (concat vala-syntax:non-empty-line-grouped-re
          "\\|//\\|\\(?:" 
          vala-syntax:syntactical-newline-keyword-opt-re
          "\\)")
  "Match anything defined, for propertizing purposes, as
newlines. Includes semi-colon, form-feed, dual
forward-slash (i.e. single line comments) and the symbols 'new',
'throws'")
;;(progn (when (looking-at vala-syntax:syntactical-newline-re)
;;(goto-char (match-end 0))))new 


(defconst vala-syntax:syntactical-newline-re2
  (concat vala-syntax:non-empty-line-grouped-re
          "\\|\\(?:" 
          vala-syntax:syntactical-newline-keyword-opt-re
          "\\)")
  "Match anything defined, for propertizing purposes, as
newlines. Includes semi-colon, form-feed, and the symbols 'new',
'throws'")
;;(progn (when (looking-at vala-syntax:syntactical-newline-re)
;;(goto-char (match-end 0))))new 

(defconst vala-syntax:syntactical-newline-re3
  (concat 
          "[;\n]\\|\\(?:" 
          vala-syntax:syntactical-newline-keyword-opt-re
          "\\)")
  "Match anything defined, for propertizing purposes, as
newlines. Includes semi-colon, form-feed, and the symbols 'new',
'throws'")
;;(progn (when (looking-at vala-syntax:syntactical-newline-re)
;;(goto-char (match-end 0))))new 




;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun vala-syntax:backward-sexp ()
  "Move backward one vala expression. It can be: parameter
  list (value or type), id, reserved symbol, keyword, block, or
  literal. Delimiters (.,;) and comments are skipped
  silently. Position is placed at the beginning of the skipped
  expression."
  (interactive)
  ;; for implementation comments, see vala-syntax:forward-sexp
  (forward-comment (- (buffer-size)))
  (while (> 0 (+ (skip-syntax-backward " ")
                 (skip-chars-backward vala-syntax:delimiter-group))))

  (when (= (skip-syntax-backward ".") 0)
    (goto-char (or (scan-sexps (point) -1) (buffer-end -1)))
    (backward-prefix-chars)))





;;;;
;;;; Character syntax table
;;;;

;;; The syntax table relies heavily on the syntax-propertize-functions being
;;; run. Hence this syntax requires at least emacs 24, which introduced
;;; this new facility.

(defvar vala-syntax:syntax-table nil
  "Syntax table used in `vala-mode2' buffers.")
;(when (not vala-syntax:syntax-table)
  (let ((syntab (make-syntax-table)))
    (message "*table-rebuild*")
    ;; 1. Reset all parent table syntax marked as punctuation or
    ;; parentheses in the parent to symbol constituents.
    ;; Clean slate. Clears the new table, if not the parent? 
    (map-char-table
     #'(lambda (key value)
         (when (or (= (syntax-class value) 4) ; open
                   (= (syntax-class value) 5) ; close
                   (= (syntax-class value) 6) ; prefix
                   (= (syntax-class value) 1) ; punctuation
                   )
           (modify-syntax-entry key "." syntab)))
     (char-table-parent syntab))

     ;; Codes below 'space' are either illegal or whitespace.
     ;; Includes newline.
     (modify-syntax-entry '(0 . 32) " " syntab)

     ;; Vala parentheses
     ;; Angle brackets cant be here as they clash with ops. See below.
     ;; Square brackets have too many uses; array declaration, array
     ;; access (slice and index), and annotations (Vala - 'Attributes')
     ;; such as extern and property metadata. And the notify 
     ;; property-by-name compiler call.
     ;; However, these are all parenthetical scans, thankfully.
     ;; Curly brackets are always used for code blocks. 
     (modify-syntax-entry ?\( "()" syntab)
     (modify-syntax-entry ?\{ "(}" syntab)
     (modify-syntax-entry ?\[ "(]" syntab)
     (modify-syntax-entry ?\) ")(" syntab)
     (modify-syntax-entry ?\} "){" syntab)
     (modify-syntax-entry ?\] ")[" syntab)


     ;; Angle brackets are used for generic typing in Vala, as with c++.
     ;; Sadly, they can't be set as parentheses, or the scan will be
     ;; thrown by a 'greater than' operator.
     ;; Make them punctuation.
     (modify-syntax-entry ?\< "." syntab)
     (modify-syntax-entry ?\> "." syntab)

     ;; Operators
     ;; these are marked as puntuation, not because
     ;; they are used in this mode (they are not), but because
     ;; this invalidates them in symbol usage.
     ;; (other Vala operators;
     ;; forward slash, greater than, less than and asterix,
     ;; have non-symbolic syntax codes for special usages)
     (modify-syntax-entry ?\+ "." syntab)
     (modify-syntax-entry ?\- "." syntab)
     ;; Equals should be punctuation
     ;; (by default it is a symbol)
     (modify-syntax-entry ?\= "." syntab)



     ;; Vala accepts _ in most symbols I tried.
     ;; set as symbol constituent. 
     (modify-syntax-entry ?\_ "_" syntab)

     ;; Ampersand is the Vala symbol-escape character.
     ;; Another ambiguity, this time with string templates.
     ;; but it will not parse as a string delimiter, so that's ok.
     (modify-syntax-entry ?\@ "'" syntab)

     ;; & and * are Vala for pointer addressing and referencing.
     ;; * also used for multiply op and block comments.
     ;; set both to prefix
     (modify-syntax-entry ?\& "'" syntab)
     ;; For asterisk, see comments, below

     ;; Question mark is Vala null directive for types
     ;; ...and an operator, another c-family ambiguity.
     ;; By this mode's design, it doesn't need to be identified as an
     ;; operater.
     ;; ELISP/EMACS has no obvious way of doing postfix syntax
     ;; but the so-called prefix syntax becomes a symbol constituent
     ;; whenever it is next to a symbol, including after,
     ;; so that will do.
     (modify-syntax-entry ?\? "'" syntab)


     ;; Mark backslash as punctuation.
     ;; Syntax parsing handles comments
     (modify-syntax-entry ?\\ "." syntab)

     ;; '"' is Vala for delimiting strings.
     ;; Strings are propertized via the syntax-propertize-function,
     ;; so can be anything neutral.
     ;; Mark as punctuation.
     (modify-syntax-entry ?\" "." syntab)

     ;; ' is Vala for delimiting a char literal.
     ;; Vala has no "'"-as-symbol declarations, so this is easier.
     ;; Also, Vala's char literals seem fairly straightforward,
     ;; so no fuss with seperate propertizing.
     ;; Set as string quotes (default is punctuation).
     (modify-syntax-entry ?\' "." syntab)

     ;; . is Vala for namespacing
     ;; its not used for anything else (phew)
     ;; If punctuation, makes bad skipping for backwards tests
     ;; if set as symbol, makes forward skips easy, but means regex
     ;; all the way for propertizing.
     ;; set as punctuation
     (modify-syntax-entry ?\. "." syntab)

     ;; Vala punctuation
     (modify-syntax-entry ?\, "." syntab)

     ;; This is a decision.
     ;; This mode uses line detection as a significant feature of Vala
     ;; (c, c#...) syntax. Line detection includes semi-colons and
     ;; newlines, and a few other keywords too.
     ;; But no syntax class exists for line ends (sadly).
     ;; The only way to make these items significant to the parse,
     ;; without triggering fontlocks and whatever other auto-magic is
     ;; glowing in EMACS code, is to make them a symbol constituent, and
     ;; '_' itself into a word. Which bends EMACS conventions a little
     ;; too far for stability.
     ;; line ends can be found by EMACS underlying c-code, but that
     ;; leaves the problem of semi-colons, only detectable by 
     ;; space-scanning  (if the scan and sexp hopping is used).
     ;; So we stick to EMACS conventions:
     ;; semi-colon is punctuation. 
     ;; Line-ends are marked as whitespace, so syntax parsing with sexp
     ;; can walk straight over them, making multiline construct
     ;; detection easy. 
     ;; However classified, it seems regex are needed for line detection.
     (modify-syntax-entry ?\; "." syntab)

     ;; Mark LF and CR as whitespace
     (modify-syntax-entry ?\n  " " syntab)
     (modify-syntax-entry ?\r  " " syntab)

     ;; Mark LF and CR as whitespace
     ;; This allows sexp over lineends, which is helpful for 
     ;; method/function/field detection.
     ;; Since this mode uses targeted sexp and regex
     ;; the loss of some autoformatting is not a great loss?
 


     ;; comments
     ;; Not using auto-comment syntax for single-line comments as they
     ;; usually finish with a line-end character (this mode puts the
     ;; comment end on the character before the line-end, to keep the
     ;; line-end alive if a sexp skips the comment). Propertize
     ;; functions handle that.
     ;; 1: 1st char of 2 char comment start
     ;; 4: 2nd char of two char comment end
     ;; n: nestable
     ;; Block comments are set as nestabe, not because they are or not
     ;; according to Vala spec, but because this makes them easy to
     ;; spot using integerp from a parse state.
     (modify-syntax-entry ?\/  ". 14n" syntab)
     ;; 2: 2nd char of 2 char comment start
     ;; 3: 1st char, two char comment end
     ;; b: only significant when infixed in 2 char comment sequences
     ;; asterix is otherwise set as a prefix
     (modify-syntax-entry ?\*  "' 23b"   syntab)
     

     (setq vala-syntax:syntax-table syntab)
  )
; (string (char-syntax ?/))
; (skip-syntax-forward " ") 


;;
;; looking-at
;;

;; Code Attributes

;; There's problems with code attributes.  From c#, and other
;; languages, they ambiguate the index operator with annotation
;; bracketing. Searching for text won't help, as all kinds of
;; variables may be in an index operator. Filtering names,
;; e.g. Deprecated, Compact, CCode... is foiled by vapi flexibility in
;; naming just about any c attribute.

;; We could go for an initial capital letter, which seems standard.
;; currently we look for the form,
;; '[' someSymbol optional(sexp) ']'
;; located at the start of a line.
(defun vala-syntax:looking-at-code-attribute ()
  "matches a code attribute"
  (save-excursion
    (when (looking-at vala-syntax:vala-code-attribute-start-re)
      (let ((start-mark (match-beginning 0)))
        (goto-char (match-end 0))
        (when (looking-at "[ ]*(")
          ;; this can error?
          (forward-sexp))
        (when (looking-at "[  ]*]")
          (set-match-data (list start-mark (match-end 0)))
          t )))))
;(vala-syntax:looking-at-code-attribute)[dio (dip = "diffy")]



;;
;; utility
;;

;; Not provided in my version
(defun vala-syntax:buffer-narrowed-p ()
  "Return non-nil if the current buffer is narrowed."
  (/= (buffer-size)
      (- (point-max)
         (point-min))))


(defun vala-syntax:look-backward-p (re)
  (save-excursion
    (while (not (or (bobp)
                    (looking-at re)
                    (progn
                      (while (and (not (bobp)) (= (char-before)?\s))
                        (backward-char))
                      (cond
                       ((= (char-after) ?\;) t)
                       ((looking-at "^\\s *$"))
                       (t  (progn (backward-char) nil)))
                      )))
      )
    (looking-at re)))
; (vala-syntax:look-backward-p "class")


;; being used someplace?
;; TODO: not in this form. see indent.
(defun vala-syntax:beginning-of-code-line ()
  (interactive)
  "Move to the beginning of code on the line, or to the end of
the line, if the line is empty. Return the new point.  Not to be
called on a line whose start is inside a comment, i.e. a comment
begins on the previous line and continues past the start of this
line."
  ;; TODO: make it work even if the start IS inside a comment
  (beginning-of-line)
  (let ((eol (line-end-position))
        (pos (point)))

    (while (and (forward-comment 1)
                (< (point) eol))
      (setq pos (point)))
    ;; Now we are either on a different line or at eol.
    ;; Pos is the last point one the starting line.
    (if (> (point) eol)
        (goto-char pos)
      (skip-syntax-forward " " eol)
      (point))))


;;
;; Bracket depth cache
;;

(defvar vala-syntax:bracket-depth-cache-region-end-anchor 0)

;; arbitary length to start
(defvar vala-syntax:bracket-depth-cache
  (make-vector 32 nil))


(defvar vala-syntax:bracket-depth-cache-depth 0
  "dpeth is the deepest valid value")

;; be quiet, compiler!
(declare-function fun-p "vala-mode2-syntax.el" nil)


(defun vala-syntax:get-bracket-depth-cache (fun-p depth)
  "Get a cached value,

This function returns cached values if DEPTH is below previous
requested depths. Otherwise it calculates, and caches, a new
value from FUN-P. The cache is intended for bracketed, nested,
data.

return: the value from a FUN-P, either cached or newly
calculated.
"
  ;; simple heuristic - the value will be recalculated if above the
  ;; previous request.
  (if (> depth vala-syntax:bracket-depth-cache-depth)
      ;; depth not in valid region. Use function to calculate.
      (progn
        ;; only increase the depth if it is directly above the current
        ;; valid depth (or old and invalid nested values may be
        ;; revalidated)
        (when (= depth (+ vala-syntax:bracket-depth-cache-depth 1))
          (setq vala-syntax:bracket-depth-cache-depth depth))
        (aset vala-syntax:bracket-depth-cache depth (funcall fun-p)))
    ;; depth is in the valid region, if working sequentially, bracket
    ;; results will not have changed, return the cached value.
    (progn 
      ;; clamp the depth. anything above this is now suspicious
      (setq vala-syntax:bracket-depth-cache-depth depth)
      (aref vala-syntax:bracket-depth-cache depth)
      )))
;; (class-or-class-level-definition-p) 
;; (defun t-p () t)
;; (progn (setq vala-syntax:bracket-depth-cache  (make-vector 32 nil))
;; (setq vala-syntax:bracket-depth-cache-depth 0))
;; (progn (vala-syntax:get-bracket-depth-cache 't-p 0)
;; (message "-%s--%s-" vala-syntax:bracket-depth-cache-depth 
;; vala-syntax:bracket-depth-cache))


(defun vala-syntax:reset-bracket-depth-cache (start)
  "Reset the cache.
Will not reset if start is similar to last call (i.e. this region
starts in the same place, so has the same initial data)"
  (when (not (= start vala-syntax:bracket-depth-cache-region-end-anchor))
    (message "  uncontiguous cache reset -%s-"
             vala-syntax:bracket-depth-cache-region-end-anchor)
    (setq vala-syntax:bracket-depth-cache-depth 0))
  (setq vala-syntax:bracket-depth-cache-region-end-anchor  start))




;;
;; token syntax
;;

(defun vala-syntax:skip-char-space ()
  "skip spaces
"
;;TODO: regex faster?
    (while (= (char-after) ?\s)
      (forward-char)))
;(vala-syntax:skip-char-space)

(defun vala-syntax:skip-char-space-tab ()
  "skip tabs and space."
  ;;TODO: is regex faster, more secure, more general?
  (when (looking-at "[\t ]*")
    (goto-char (match-end 0)) t))
;(vala-syntax:skip-char-space-tab)  

(defun vala-syntax:skip-char-tab-space-and-line-ends ()
  "skip tabs, space, and line-ends
return: t if space was skipped, else f"
  (when (looking-at "[\t\r\n ]+")
    (goto-char (match-end 0)) t))
;(vala-syntax:skip-char-tab-space-and-line-ends)

(defun vala-syntax:skip-forward-comment-or-string (parse-state)
  ;; 3 = in a string 4 = in a comment
  (when (or (nth 3 parse-state)  (nth 4 parse-state))
    (goto-char (nth 8 parse-state))
    (if (nth 4 parse-state)
        ;; use forward comment. Sexps leap bracketing and
        ;; punctuation also.
        (forward-comment 1)
      ;; sexp finishes tight on strings
      (forward-sexp 1)) t ))

 
;; much used, expensive?
;; inline has little effect
(defun vala-syntax:goto-syntactical-line-start (limit)
  "Goto items regarded as syntactical line starts.
These include semi-colons, LF, 'new', 'throws' or line-comment
starts.  The function ignores matches in strings and comments,
and skips newlines.
 
The function positions point to read the new text, i.e. after
semi-colon and LF but before a 'new' symbol or line-comment
start.

Because the function will not skip keywords, calling
code must make a move over keyword points returned supplied
by this function, otherwise parsing will step into an infinite
loop.
return: t if found one and less than limit, else nil"
  ;; This function needs to,
  ;; - ignore bracketing
  ;; - catch punctuation character ';'
  ;; - catch whitespace character '\n'
  ;; - match keywords
  ;; Since the scan has been done, it would seem a good idea to use
  ;; sexp skipping, but it has disadvantages, missing punctuation
  ;; (linends) and skating bracketing in a LISPy way.
  ;; However, when it can be trusted, it is fast, so this function is
  ;; a hybrid of regex and sexp parsing. It is is also heavy duty
  ;; on testing, but profiling shows testing to be very
  ;; advantageous.
   (let ((parse-state nil) (anchor nil))
     (setq anchor (point))

     (while (and
             ;; always stop at limit
             (< (point) limit)
             (progn
               ;; find us a newline
               (re-search-forward vala-syntax:syntactical-newline-re3 limit t)
               ;; ways to continue loop.
               ;; had a match (i.e. no match fails)
               (when (> (point) anchor)
                 ;; keyword, move back
                 (when (match-beginning 1) (goto-char (match-beginning 1)))
                 (setq parse-state (syntax-ppss (point)))
                 (when (or
                        ;; looking at line comment starts, or empty lines
                        (looking-at "[ \t]*\\(?://\\|$\\)")
                        ;; ...or in string, comment
                        (vala-syntax:skip-forward-comment-or-string parse-state))
                   (setq anchor (point)) t )))))
     (and (> (point) anchor) (< (point) limit))))

;; (defun vala-syntax:goto-syntactical-line-start-interactive ()
;;   (interactive)
;;   (vala-syntax:goto-syntactical-line-start (point-max)))


 (defun vala-syntax:goto-syntactical-line-start-interactive ()
   (interactive)
   (vala-syntax:goto-syntactical-line-start (point-max)))


(defun vala-syntax:skip-syntax-symbol-path ()
  "skip a symbol path using parse data.
This function uses syntax as defined in the table, so is safe on
an initial parse.
return: t if a path skipped and has no trailing point, else nil"
  ;; store the start to see if the tests match anything.
  (let ((anchor (point)))
    (while (and (> (skip-syntax-forward "w_'" (line-end-position)) 0)
                (when (= (char-after) ?\.) (forward-char) t)
                ))
    (and (< anchor (point)) (not (= (char-after) ?\.)))
    ))
;(vala-syntax:skip-syntax-symbol-path) ju.fo.

;; used by indent?
(defun vala-syntax:skip-syntax-backward-symbol-path ()
  "skip a symbol path backwards using parse data.
return: t if a path skipped and has no trailing point, else nil"
  ;; store the start to see if the tests match anything.
  (let ((anchor (point)))
    (while (and (< (skip-syntax-backward "w_'" (line-beginning-position)) 0)
                (when (= (char-before) ?\.) (backward-char) t)
                ))
    (and (> anchor (point)) (not (= (char-before) ?\.)))
    ))
;ju.fo.     
;(progn (goto-char 63179)
;(vala-syntax:skip-syntax-backward-symbol-path))


;; SEEALSO: vala-syntax:pskip-partial-generic ()
(defun vala-syntax:skip-generic ()
  "Skip a generic description, even partial.
If the generic fully closes, matching stops, even if followed by
generic-like code. The function allows crossing line ends after
commas.  It uses syntax as defined in the table, so is safe on an
initial parse, and the return is designed to indicate if parsing
could progress (not a test of parse validity).
It is intended for use in detection.
return: t if matched and the generic closed, else f"
  (let ((depth 0) (start (point)))
    (while (progn
             ;;    (message "fuck -%s-" (point))
             ;; three ways of continuing, comma, close angle brace,
             ;; or unrecognised parse
             (cond
              ((eobp) nil)
              ((= (char-after) ?\<)
               (setq depth (+ depth 1))
               (message " prop1 -%s--%s-" (point) (+ (point) 1))
               (forward-char)
               (skip-syntax-forward " " (point-max))
               (vala-syntax:skip-syntax-symbol-path)
               )
              ((= (char-after) ?\,)
               (forward-char)
               (skip-syntax-forward " " (point-max))
               (vala-syntax:skip-syntax-symbol-path)
               )
              ((= (char-after) ?\>)
               (setq depth (- depth 1))
               ;;(message " prop2 -%s--%s-" (char-before) (char-after))
               (forward-char)
               (> depth 0)
               )
              (t nil))))
    ;;(message " depth -%s-" depth)
    depth
    ))
;;(vala-syntax:skip-generic)
;; <qdig.hoh<hot>>



;;
;; Compound syntax
;;
(defun vala-syntax:skip-syntax-forward-type ()
  "Skip a type declaration forward.
This function uses syntax to skip. It relies on generic baracketing
having bracketing syntax.
It ignores (char-based) line ends in it's searching to test for a type
declaration.
For detection after propertizing.
return: t if a type was matched, else nil"
    ;; The obvious algorithm is to use the parser to look for a following
    ;; symbol. However, this will not work, especially in a propertize
    ;; function on the initial parse. Initial parse
    ;; information can not be trusted as angle brackets
    ;; (and maybe other characters) may carry c language parsing 
    ;; ambiguities. So regex. 
;;(message "skip type")
    ;; store the start to see if the tests match anything.
    (let ((anchor (point)))
      ;; The inital parse values can be used for symbols skipping, they
      ;; are a positive assertion of chars.
      ;; if followed by a period, this text must be a namespaced chain.
      ;; An OO chain could be any oo declaration, class type, field or
      ;; method, so skip them.
      ;; This code allows a trailing dot, but namespace operators are
      ;; not highlighted, so pass the test.
      ;;todo: use the function
      (when (vala-syntax:skip-syntax-symbol-path)
        ;; skip any space. Note the function can skip over line ends
        (vala-syntax:skip-char-tab-space-and-line-ends)
        (cond
           ((or (= (char-after) ?\<) (= (char-after) ?\[))
            ;; if followed by open angle or open block brackets,
            ;; must be a type.
            ;; skip brackets.
            ;; this can error?
            ;;(goto-char (scan-lists (point) 1 0)) t)
            ;; skip the parentheses
            ;;TODO: this can error?
            (forward-sexp) t)

           ((looking-at "\\(?:[a-zA-Z]\\)")
            ;;(nth 1 (syntax-ppss (point)))
            ;; if followed by a symbol char then likely the code is the type
            ;; declaration of a type symbol pair (this algorithm may fail for a
            ;; number followed by <, say (a number being a valid symbol in the
            ;; simplictic parsing here). We'll see? 
           t)
            ;; testing for a match on a following symbol catches
            ;; method and implicit var declarations.
            ;; there's one last possibility that this may be a type
            ;; declaration, if it is part of a class inheritance list.
            ;; it would only be a sole type, with a following comma...
            ((vala-syntax:class-definition-p) t)
            (t nil) 
            ))
      ))
;(vala-syntax:skip-syntax-forward-type)
;indiscrete.monochrome[] dim

;; used by indent?
(defun vala-syntax:skip-syntax-backward-type ()
  "Skip a type declaration backwards.
This function uses syntax to skip. It relies on generic baracketing
having bracketing syntax.
It ignores (char-based) line ends in it's searching to test for a type
declaration.
For detection after propertizing.
return: t if a type was matched, else nil"
    ;; store the start to see if the tests match anything.
    (let ((anchor (point)))
      (when (or (= (char-before) ?\>) (= (char-before) ?\]))
        ;; if skipped a generic or array modifier, back again
        (backward-sexp)
        )
      (backward-char)
      (vala-syntax:skip-syntax-backward-symbol-path)
      ))
;class indiscrete.monochrome[]
;(progn (forward-line -2) (goto-char (line-end-position))
;(vala-syntax:skip-syntax-backward-type))

;; Unused
(defun vala-syntax:skip-symbol-optional-generic ()
  "Skip a symbol followed by an optional generic.
Not the same as a type, doesn't allow square brackets.
It uses syntax as defined in the table, so is safe on an
initial parse,
Used for method detection.
return: t if symbol, and any following generic closed, else nill"
  (and
   (vala-syntax:skip-syntax-symbol-path)
   (= (vala-syntax:skip-generic) 0)))
;;(vala-syntax:skip-symbol-optional-generic)
;;qaw<d>


;;
;; Predicate functions
;;
;; xxx-p functions test if something is something else. They usually
;; return boolean, though some here return state (maybe not
;; predicates, but this is LISP being hoity-toity).
;;
;; Due to the many ambiguities in c-family syntax, the predicate
;; functions can not usually return definitive results from any point
;; in a buffer. They should be used in likely areas, to select between
;; alternatives.

(defconst vala-syntax:type-guess-re
  (concat
   ;; classpath, generic, or constructor, yes  
   "[.<[(]\\|" 
   ;; space then alphanumeric, yes
   "\\(?:\\s +[A-Za-z@]\\)" ;
   ))

(defun vala-syntax:type-or-constructor-rough-p ()
  "Test an item is like a type or constructor.

The function searches for a few characters giving positive id
\\(.<(). If that fails, it looks for space than the start of
a (so a type/symbol pair).

This function only works in generally likely regions."
  (save-excursion
    (when  (> (skip-syntax-forward "w_'" (line-end-position)) 0)
      (looking-at vala-syntax:type-guess-re))))

;; Used in indent
(defun vala-syntax:class-definition-p ()
  "Check an item is part of a list opened with the word 'class'.
Fails on empty lines, semi-colon, and beginning of buffer.
param: a regex
return: t if matched, else nil"
  (vala-syntax:look-backward-p 
   vala-syntax:level-indenting-keywords-opt-re))

;; Much used,
;; profiling suggests about 1/8 of total scantime.
(defun vala-syntax:preceeded-by-class-definition-p ()
  "Check point is somewhere within the syntax of a class definition.
The algorithm is to sexp back, looking for the word 'class'.
The function fails on all bracketing, or bobp appears.

Used for detecting inheritance lists.
return: t if matched, else nil"
  (save-excursion
    ;; This error block catches any move up in bracketing level
    (ignore-errors
      ;; Can't fail on semi-colon, as marked as punctuation so skipped
      ;; by sexp. However, can stop on bracketing.

      ;; This do...until loop steps backwards before trying any tests.
      (while (progn
               (backward-sexp)
               (not (or
                     (bobp)
                     ;; fail on curly/round brackets too. Must have sexped
                     ;; over a code block.
   ;;                  (looking-at "(\\|\\|{\\|class")
                     (looking-at 
                      vala-syntax:level-indenting-keywords-and-bracketing-re)
                      ))))
;;  (message " classwords found at -%s-" (point))
      (looking-at vala-syntax:level-indenting-keywords-opt-re)
     ;; (looking-at "class")
      )))
; protect class boop.frog<> : 
;dark
;(progn (forward-line -2)
; (vala-syntax:preceeded-by-class-definition-p))
;;  (message "-%s-" (vala-syntax:preceeded-by-class-definition-p)))

(defun vala-syntax:preceeded-by-class-definition-p-int ()
   (interactive)
   (message "-%s-" (vala-syntax:preceeded-by-class-definition-p)))

;; used by indent
(defun vala-syntax:syntax-class-inheritance-item-p ()
  "Check an item is part of a list of inheritance items.
The algorithm is to walk back, looking for commas, the colon, 
the symbol then the word 'class'.
This function uses syntax to skip. It relies on generic bracketing
having bracketing syntax.
return: t if matched, else nil"
  (save-excursion
    (while (and
            (progn (skip-syntax-backward " ") (= (char-before) ?\,))
            (progn (backward-char) (vala-syntax:skip-syntax-backward-type)))
      (backward-char))
    (when (and (= (char-before) ?\:)
               ;; not enough, there's an ambiguity with named parameters
               (progn (backward-char)
                      (skip-syntax-backward " ")
                      (vala-syntax:skip-syntax-backward-type))
               (progn (backward-sexp)
                      (looking-at "class"))) t )))
;protected class oko.Entry : gtk, conconation.crum,   io[],
;(progn (forward-line -2) (goto-char (line-end-position))
;(vala-syntax:syntax-class-inheritance-item-p))


(defun vala-syntax:class-or-class-level-definition-p ()
  "test if in class definition code, or a definition within a class code block.
It ignores (char-based) line ends in a search to test for a class
declaration.
The function will return true for positions after a class declaration
e.g. within an inheritance list, for positions within the brackets of
a class, and for positions in curved brackets within either area? 
This function uses syntax to skip. It relies on generic baracketing
having bracketing syntax.
return:
0 if it passed without moving point,
1 if it passed by moving out of curved parameters,
2 if it passed by moving out of curly brackets,
3 if it passed by moving out of both kinds of brackets,
else nil"
  (save-excursion
    (let (
          (test-point (point))
          (return-code 0)
          (open-parenthesis nil)
          )
      ;; (message " class definition test: -%s-" (vala-syntax:buffer-narrowed-p))
      ;; (message " class definition test: -%s-" (point))
      (cond 
       ;; if at top level, return true
       ;; 0 = depth of parentheses
       ;;  ((< (nth 0 (syntax-ppss (point))) 1) t)
       ;; if in a comment, return false
       ;; 4 = in comment
       ((nth 4 (syntax-ppss (point))) nil)
       (t 
        ;; test for an open curved parenthesis. If so, jump back and
        ;; out. This takes us out of parameter lists. It means
        ;; little if we back out of a condition, for example,
        ;; but if we back out of a method parameter list, the next
        ;; test will make this return true, i.e. this function returns
        ;; true for method parameter lists.
        ;; 1 = start of parentheses
        (setq open-parenthesis (nth 1 (syntax-ppss test-point)))
        (when open-parenthesis
          (goto-char open-parenthesis)
          (when (= (char-after) ?\()
            (setq return-code 1)
            (setq test-point (point))))
        ;;(message " class definition test-point1: -%s-" test-point)
        ;; test for an open curly parenthesis. If so, jump back and
        ;; out. This takes us out of a code block.
        (setq open-parenthesis (nth 1 (syntax-ppss test-point)))
        (when open-parenthesis
          (goto-char open-parenthesis)
          (when (= (char-after) ?\{)
            (setq return-code (+ return-code 2))
            (setq test-point (point))))

        (goto-char test-point)
        ;;(message " class definition test-point2: -%s-" test-point)

        (when 
            ;; direct calculation
            (vala-syntax:preceeded-by-class-definition-p)
          ;; cached results
          ;; marginal 1/10 sec, not worth it.
          ;; (vala-syntax:get-bracket-depth-cache
          ;;     'vala-syntax:preceeded-by-class-definition-p
          ;     (nth 0 (syntax-ppss test-point)))
          return-code)
        )))))
; (vala-syntax:class-or-class-level-definition-p)indiscrete.revelation +a[


(defun vala-syntax:class-or-class-level-definition-p2 ()
  "test if in class definition code, or a definition within a class code block.
It ignores (char-based) line ends in a search to test for a class
declaration.
The function will return true for positions after a class declaration
e.g. within an inheritance list, for positions within the brackets of
a class, and for positions in curved brackets within either area? 
This function uses syntax to skip. It relies on generic baracketing
having bracketing syntax.
return:
0 if it passed without moving point,
1 if it passed by moving out of curved parameters,
2 if it passed by moving out of curly brackets,
3 if it passed by moving out of both kinds of brackets,
else nil"
  (save-excursion
    (let (
          (return-code 0)
          (open-parenthesis nil)
          )
      ;;(message " class definition test: -%s-" (point))
      (cond 
       ;; if in a comment, return false
       ;; 4 = in comment
       ((nth 4 (syntax-ppss (point))) nil)
       (t 
        ;; back out of, sequentially, a curved, then curly brackets.
        ;; 1 = start of parentheses
        (setq open-parenthesis (nth 1 (syntax-ppss (point))))
        (when open-parenthesis
          ;; first parenthesis
          (goto-char open-parenthesis)
          ;; if curved bracket, jump back again, incrementing the
          ;; return value. This takes us out of parameter lists. It
          ;; means little if we back out of a condition, for example,
          ;; as point is still in the code block of a method, but if
          ;; we back out of a method parameter list, the next test
          ;; will make this return true, i.e. the function returns
          ;; true for method parameter lists.
          (when (= (char-after) ?\()
            (setq return-code 1)
            (setq open-parenthesis (nth 1 (syntax-ppss (point))))
            (when open-parenthesis
              (goto-char open-parenthesis)
              ))
          ;; test for an open curly parenthesis, appending to the
          ;; return value. This takes us out of a code block.
          (when (= (char-after) ?\{)
            (setq return-code (+ return-code 2)))
           ;; (message "  class definition test-point2: -%s-" (point))
          )
        ;; now test
        (when (vala-syntax:preceeded-by-class-definition-p)
          ;; cached results
          ;; marginal 1/10 sec, not worth it.
          ;; (vala-syntax:get-bracket-depth-cache
          ;;     'vala-syntax:preceeded-by-class-definition-p
          ;     (nth 0 (syntax-ppss (point))))
          return-code)
        )))))
; (vala-syntax:class-or-class-level-definition-p)indiscrete.revelation +a[

(defun vala-syntax:class-or-class-level-definition-p-interactive ()
  (interactive)
  (message " is c-%s-" (vala-syntax:class-or-class-level-definition-p2)))

;; used in indent
;TODO: This should allow line ends on spacing
; and make positive detection of all parts
; should also include a modifier check
; and also following brackets to confirm
; and maybe be replaced by the definition test?
(defun vala-syntax:syntax-function-or-class-declaration-p ()
  "Test for a fuction or class definition.
Includes simple, array and class types. This
function is reasonably defended, but should be used in areas which are
likely to be a definition, as it will sucessfully detect anything that
matches symbol, symbol, opening curved bracket.
return: t if match, else nil"
  ;; easy start - any modifiers means it is
  (if (looking-at vala-syntax:oo-modifier-keywords-seq-re) t
  (save-excursion
    ;; look for type, symbol, open bracket 
    (when (vala-syntax:skip-syntax-forward-type)
      ;; skip any space. Note the function can skip over line ends
      (vala-syntax:skip-char-tab-space-and-line-ends)
      (vala-syntax:path-symbol-with-following-bracket-p)
      ))))
;(vala-syntax:syntax-function-or-class-declaration-p)
; doing[] pounding.head(
;doing<freeb<heck>> pally


;; used in indent?
(defun vala-syntax:path-symbol-with-following-bracket-p ()
  ""
  (save-excursion
    (vala-syntax:skip-syntax-symbol-path)
    (vala-syntax:skip-char-tab-space-and-line-ends)
    (=(char-after) ?\()))
;(vala-syntax:path-symbol-with-following-bracket-p)
;gh (



;;
;; Propertize utilities
;;
(defun vala-syntax:propertize-extend-region (start end)
  "See syntax-propertize-extend-region-functions"
  ;; nothing yet
  nil)


(defmacro vala-syntax:put-syntax-table-property (match-group value)
  "Add 'syntax-table entry 'value' to the region marked by the
match-group 'match-group'"
  `(put-text-property (match-beginning ,match-group)
                      (match-end ,match-group)
                      'syntax-table
                      ,value))


;;;;
;;;; Syntax table propertize functions
;;;;
;;
;; Propertizing functions
;;                                                                             
;; pskip: adds properties and skips
;; These functions lunge ahead, adding properties and skipping.
;;
;; They may modify any properties necessary for this mode. Currently, they
;; - add a discrete amount of fontlocking
;;   when used in all levels of highlighting,
;; - change the syntax of certain characters
;;   when the parsing by the pskip has positivly identified the usaage of
;;   a character. Thus trying to avoid the many ambiguities of syntax in
;;   c-family languages.  
;;
;; All pskip functions should return if it's worth carrying on propertizing.
;; At least one function (generic detection) is an optional match, 
;; but it's return still informs about continuation.
;; Note that this is not the same as saying that the propertizing itself
;; suceeded or not.
;;
;; None of the funtions are expected to do any propertizing beyond a line
;; end (generics?).
;; They should do appropiate end tests and return nil (don't carry on parsing)
;; if necessary.
;;
;; limiting is not fully implemented. It is not seen as a failure to overrun,
;; but the functions should stop someplace.
;; they may test for syntax before or beyond this, though.                                                    
;; Error highlighting really consists of not highlighting and, if a 
;; match can't be found, and not continuing the paarse/propertizing.
;; Ocassionally, a propertize parse will make a bad match, as the functions
;; do not look forward much (and c-family grammer is very difficult for a
;; fast match). Bad matches are not always regarded as an algorithmic failure,
;; as the skip function will leave the parsing in a place where the next match
;; will find unparsable items.




;; The following propertize-X functions are called through the lowermost
;; function "propertize", from vala-mode2 call on "define-derived-mode".
;; They resolve ambiguous-to-a-regex parsing (such as '<' and "\"\"\"")
;;


;; notes on the pskip functions:
;; - They tend to regex, so they can make fine-grained face-locking,
;; and so they are not fooled by sexping over local punctuation.
;; - They regex over spaces too. So many naturally fail on line-ends.
;; This is ok, see the notes to the propertize function. 
;; - They don't care about following syntax. Once they've propertized
;; what we've specified, they quit (some don't have a useful return)
;; - When they return, they tend to return t/nil for "worth continuing?"



;; TOCONSIDER: seems messy for what it does?
;; TODO: This seems to escape '\""'?
(defun vala-syntax:escaped-char-p (&optional pos)
  "check if a string is an escaped character.
checks if the previous character is a backslash escape, which is not itself backslashed."
  (save-excursion
    (goto-char (if (null pos) (point) pos))
    (let ((back-char-1 (char-before)) (back-char-2 nil))
      (when back-char-1
        (goto-char (- (point) 1))
        (setq back-char-2  (char-before)))
      (and 
       (not (null back-char-1))
       (= back-char-1 ?\\)
       (or (null back-char-2)
           (not (= back-char-2 ?\\)))
      ))))
;\\c
;(vala-syntax:escaped-char-p 48931)


(defun vala-syntax:propertize-line-comments-and-strings (start end)
  "Add syntax properties to strings and line comment characters.
"
  (goto-char start)
  ;; stash the matches so syntax-ppss will not interfere with them
  (let ((found-end nil) (found-start nil) (syntax-parse nil))
    (while (re-search-forward "\"+\\|\\(//\\)" end t)
      (if (match-beginning 1)
          (progn
            ;; ...and here is an issue. What if the line-comment
            ;; characters are inside a block comment? EMACS handles the
            ;; line-comment, usually, by setting the end char to be the
            ;; newline. This could never clash with a block comment end
            ;; - either one comment is inside the other, or the comments
            ;; overlap, and EMACS can handle overlapping comments.

            ;; However, if we back off from the line-end, then it is
            ;; possible we back onto a block comment end. Some solutions
            ;; would be to,

            ;; - set syntax using flags as EMACS would usually do.
            ;; in the stock setup for c flags every line-end is maybe a
            ;; line-comment end, Unfortunately, when flagging a block
            ;; comment end, it seems EMACS will not recognise the
            ;; character acts as a dual comment end. Sexp, if it reaches
            ;; a nested comment, misses the intended (dual) comment
            ;; end and looks for the next one, probably a line end
            ;; halfway down the file. Seems like an accident in the
            ;; making.

            ;; - reject the characters
            ;; on the basis they are inside a comment. Rejection may
            ;; seem easy, but would render the stock variable
            ;; parse-sexp-ignore-comments redundant. Whatever that does.

            ;; - move back
            ;; which is pretty ugly, but since this mode doesn't do
            ;; anything with the second slash it identifies, is
            ;; solid. Unlike flag setting, this seems to sexp as
            ;; expected.

            ;; Comment start on the initial slash. Easy.
            (put-text-property (match-beginning 1) (+ (match-beginning 1) 1) 
                               'syntax-table
                               (list 11 nil))
            
            ;; put the comment end before the end of the line
            ;; this is, I understand, a little unconventional, but means
            ;; comment skipping will never skip the line end after one
            ;; of these comments. Which we want, as we detect the line
            ;; ends to maybe propertize. If it is the final character,
            ;; and not the linend, which delimits a comment, this makes
            ;; no difference for, say, fontlocking the line.
            (let ((target-char (- (line-end-position) 1)))
              (goto-char target-char)
              ;; if on a block comment boundary, move back in.
              (when (and (= (char-after) ?\/) (= (char-before) ?\*))
                (setq target-char (- target-char 2)))
              ;; propertize where we decided
              (put-text-property target-char (+ target-char 1)
                                 'syntax-table
                                 (list 12 nil)))
            ;; couldn't be anything interesting here now?
            ;; rough test suggests no problems after the first comment
            ;; is established
            (goto-char (line-end-position))
            )

        ;; was a string delimiter
        (setq found-end (match-end 0))
        (setq found-start (match-beginning 0))
        (setq syntax-parse (syntax-ppss found-start))
        (if (or
             ;; if escaped, ignore and move on.
             (vala-syntax:escaped-char-p found-start)
             ;; 4 = inside comment, ignore and move on.
             (nth 4 syntax-parse))
            (goto-char (+ found-end 1))
          (progn
            ;; propertizing, in some way.
            ;; Strings may have been left unbalanced by a JIT parse (or
            ;; something) that stops in the middle of a string. So test to
            ;; see if the point that triggered this is unbalanced. If so,
            ;; the string needs finishing, not starting.
            ;; 3 = inside string
            (if (nth 3 syntax-parse)
                (progn
                  (put-text-property (- found-end 1) found-end
                                     'syntax-table
                                     (list 7 nil))
                  (goto-char (+ found-end 1))
                  )
              (progn
                ;; string start
                (when (or (= (- found-end found-start) 2)
                          (= (- found-end found-start) 6))
                  ;; two or six delimiters are empty strings. Mark their
                  ;; ends too (then skip the whole match).
                  (put-text-property (- found-end 1) found-end
                                     'syntax-table
                                     (list 7 nil)))
                ;; mark the first delimiter as a string quote
                (put-text-property found-start (+ found-start 1)
                                   'syntax-table
                                   (list 7 nil))
                (goto-char (+ found-end 1)))
              )))))))


;;
;; propertize - basic tokens and token parts
;;

(defun vala-syntax:pskip-single-line-comment ()
  "Add properties and skip single line comments.
Adds comment properties to the start and end position characters.
but not fontlocking."
  (put-text-property (point) (+ (point) 1)
                     'syntax-table
                     (list 11 nil))
  ;; put the comment end before the end of the line
  ;; this is, I understand, a little unconventional, but
  ;; means comment skipping will never skip the line end
  ;; after one of these comments. Which we want, as we detect the line
  ;; ends to maybe propertize. And if it is the final character, and not the
  ;; linend, which delimits a comment, this makes no difference for,
  ;; say, fontlocking the line.
  (put-text-property (- (line-end-position) 1) (line-end-position)
                     'syntax-table
                     (list 12 nil))
  (goto-char (line-end-position))
  )



(defun vala-syntax:pskip-symbol-path (face)
  "Highlight and skip a dotted symbol path.
If matching fails, highlighting is halted.
The matching is limited by the line end
face: face to be used for the final item (usually quoted) 
return: nil if no match, else t."
  (let ((anchor (point)) (tail (point)))
    (when (> (skip-syntax-forward "w_'" (line-end-position)) 0)
      (while
          (when (= (char-after) ?\.)
            ;; First elements of classpath, namespace face
           ; (message "-%s--%s-" tail (point))
            (put-text-property tail (point)
                               'font-lock-face
                               'font-lock-namespace-face)
            (forward-char)
            (setq tail (point))
            (> (skip-syntax-forward "w_'" (buffer-size)) 0)
            ))
      ;; Last element of classspath, given face
      (put-text-property tail (point)
                         'font-lock-face
                         face)
      ) (not (= anchor (point))) ))
; (vala-syntax:pskip-symbol-path 'font-lock-class-definition-face)
; (vala-font-lock:loose-lskip-symbol-path 'font-lock-type-face)
; alva.scott.fuku
; halva.ft.gy



;; TODO: allow multiline/no?
(defun vala-syntax:pskip-partial-generic ()
  "Add properties and skip to generic descriptions, even partial.
If the generic fully closes, matching stops, even if followed by
generic-like code.
Internal path symbols are assumed and marked as font-lock-type-face.
The function is limited by line end, which is not a valid symbol or
delimiter code.
The return is designed to indicate if parsing should progress (not a test
of parse validity)
return: t if matched and the generic closed, else f"
  (let ((depth 0) (start (point)))
    (while (progn
             ;;    (message "fuck -%s-" (point))
             ;;skipping a little space here is about right?
             ;; (skip-char-space)
             ;;(skip-syntax-forward " " (line-end-position))
             ;; three ways of continuing, comma, close angle brace,
             ;; or unrecognised parse
             (cond
              ((eobp) nil)
              ((= (char-after) ?\<)
               (setq depth (+ depth 1))
               ;;(message " prop1 -%s--%s-" (point) (+ (point) 1))
               (put-text-property (point) (+ (point) 1)
                                  'syntax-table
                                  ;;'vala-syntax:syntax-table-disambiguate)
                                  (list 4 nil))

               (forward-char)
               (vala-syntax:skip-char-space)
               (vala-syntax:pskip-symbol-path 'font-lock-type-face)
               )
              ((= (char-after) ?\,)
               (forward-char)
               (vala-syntax:skip-char-space)
               (vala-syntax:pskip-symbol-path 'font-lock-type-face)
               )
              ((= (char-after) ?\>)
               (setq depth (- depth 1))
               ;;(message " prop2 -%s--%s-" (char-before) (char-after))
               (put-text-property (point) (+ (point) 1) 
                                  'syntax-table
                                 ;; 'vala-syntax:syntax-table-disambiguate)
                                  (list 5 nil))
               (forward-char)
               (> depth 0)
               )
              (t nil))))
    ;;(message " depth -%s-" depth)
    (and (> (point) start) (= depth 0))
    ))
;(vala-syntax:pskip-partial-generic)
; <qdig.hoh<hot>>

;vala-syntax:pskip-partial-generic ()


;;TOCONSIDER: This mode only ever parses OO definitions and 'new'
;;declarations so all that's needded for arrays is the empty
;;bracket. If implicit definitions were added, or attempts made to
;;highlight function calls, then the mode would need to understand
;;multiDimArray[78:9], and so forth.
(defun vala-syntax:pskip-partial-anytype ()
  "Add properties and skip anytype definition, even partial.
Includes simple, array and class types. This
function should be used in areas which are likely to be a type, as it will
sucessfully highlight anything that constitutes a valid Vala symbol,
including method names and numeric values.
The return is designed to indicate if parsing should progress (not a test
of parse validity)
return: t if matched and any generic closed, else nil"
  (when (vala-syntax:pskip-symbol-path 'font-lock-type-face)
    (vala-syntax:skip-char-space)
    ;; test for array and generic qualifiers
    (cond
     ((looking-at "\\[ *\\]") (goto-char (match-end 0)) t)
     ((= (char-after) ?\<) (vala-syntax:pskip-partial-generic))
     (t 
      ;; no qualifiers at all, that's fine, return true
      t)
     )))
; (vala-syntax:pskip-partial-anytype)
; roger.the<reputed.engineer<engineer>[]



;;
;; propertize - token compounds and sequences
;;

(defun vala-syntax:pskip-method-or-field-definition ()
  "Add properties to an method or field definition.
This function must be called on code which was preceeded by another
symbol. Otherwise it will make wild and innacurate matches. 
return: of no interest.
"
  (let ((anchor (point)) (forward-point nil))
    ;; There's no easy way here. The code must know what
    ;; follows the symbol to propertize, but can not save the match
    ;; along the way, as the locking is maybe a several-match path.
    ;; ho hum, let's go look...
    ;;(message "  is method definition -%s-" (point))
     (when (vala-syntax:skip-syntax-symbol-path)
    
      ;; skip any space.
      (vala-syntax:skip-char-space)
      (setq forward-point (point))
      (cond
       ;; following open curly bracket means property.
       ;; is a property a function? I say no. Do nothing.
       ;; following semi-colon is a short implicit var
       ;; declaration for sure. Do nothing.

       ;; following open-bracket means a method declaration
       ((= (char-after) ?\()
        (goto-char anchor)
        (vala-syntax:pskip-symbol-path 'font-lock-method-definition-face)
        ;; TODO: params?
        (goto-char forward-point)
        (forward-char)
        ;;(message "  is method definition -%s-" (point))
        (vala-syntax:pskip-anytyped-definition-parameter-list 
         (line-end-position))
        )

       ;; following comma means an implicit var declaration symbol or
       ;; typed parameter.
       ;; The only interest here is in parameters so look back for
       ;; open bracket. 
       ;; ((and (= (char-after) ?\,) (vala-font-lock:is-bracketed-item))
       ;;  ;; the symbol doesn't need propertizing. Charge on...
       ;;  (forward-char)
       ;;  (skip-char-space)
       ;;  ;;TODO: start skipping the parameter list, modifiers and pairs
       ;;  ;; If parameter, followed by list of modifiers and two symbols
       ;;  ;;(vala-font-lock:lskip-any-type-declaration-list-tail limit)       
       ;;  (vala-syntax:pskip-anytyped-definition-parameter-list 
       ;;   (line-end-position))
       ;;  (message "  is  declarative parameters -%s-" (point)))
       (t 
        ;;(message "  *failed* pskipping method or field" (point)))
       )))))
;(vala-syntax:pskip-method-or-field-definition) 
;drok 

(defun vala-syntax:pskip-class-definition (face)
  "Highlight and skip a class definition with optional generic declaration.
The declaration may have optional path and generic qualifiers.
Path symbols are marked as font-lock-namespace-face, final symbol as
font-lock-class-definition-face.
Limited by line end (not a valid match)
return: t if exists with balanced generic, else nil"
  (when (vala-syntax:pskip-symbol-path face)
    (vala-syntax:skip-char-space)
    ;; propertize generic qualifiers (no arrays on a class?)
    (if (= (char-after) ?\<)
        (vala-syntax:pskip-partial-generic)
      ;; no qualifiers at all, that's fine, return true
      t)
    ))
;(vala-syntax:pskip-class-definition 'font-lock-class-definition-face)
; roger.the<reputed.engineer<engineer>


(defun vala-syntax:pskip-anytype-list ()
  "Highlight and skip any-type list tails.
i.e. comma type comma type comma type...
Used for class inheritance lists. 
limit:limit length of work
return: nothing interesting"
;; This will accept array types,
;; which is not valid Vala syntax, but syntactically consistent,
;; and, for now, the anytype function is reused. Until someone complains)
  (while 
      (and 
       (vala-syntax:pskip-partial-anytype)
       (when (= (char-after)?\,)
         (forward-char)
         (vala-syntax:skip-char-space) t)
       )))
;(vala-syntax:pskip-anytype-list)
;bloop, blip, blinker


(defun vala-syntax:pskip-anytype-symbol-pair ()
  "Add properties to a type then a symbol.
Though not fundamental, a key token in c languages, used in
declarations.
All path symbols are assumed and marked as font-lock-type-face.
return:t if match, else nil"
  (and
   (vala-syntax:pskip-partial-anytype)
   (progn
     (vala-syntax:skip-char-space)
     (vala-syntax:pskip-symbol-path 'font-lock-method-definition-face)
    )))
; (vala-syntax:pskip-anytype-symbol-pair)
; curious<dream> thoughts 


(defun vala-syntax:pskip-anytyped-symbol ()
  "Add properties to a type, then skip a symbol.
Though not fundamental, a key token in c languages, used in
declaration parameters.
The type symbol is marked as font-lock-type-face.
return:t if match, else nil"
  (and
   (vala-syntax:pskip-partial-anytype)
   (progn
     (vala-syntax:skip-char-space)
     (vala-syntax:skip-syntax-symbol-path)
    )))
; (vala-syntax:pskip-anytyped-symbol)
; curious<dream> thoughts 



(defun vala-syntax:pskip-anytyped-definition-parameter-list (limit)
  "Add properties to an any-type symbol pair list.
return: undefined."
  (while (and
          (progn
            ;; test for and skip modifiers
            (when (looking-at vala-syntax:param-directive-keywords-seq-re)
              (put-text-property (match-beginning 0) (match-end 0)
                                 'font-lock-face
                                 'font-lock-minor-keywords-face)
              (goto-char (match-end 0)))
            ;; pskip a pair
            (vala-syntax:skip-char-space)
            (vala-syntax:pskip-anytyped-symbol))

          (progn
            ;; skip space
            (vala-syntax:skip-char-space)
            ;; test for and skip defaults
            ;; TODO: can error?
            (when (and (= (char-after) ?\=) (<= (point) limit))
            (forward-sexp))

            ;; test for the comma
            (when (and (= (char-after) ?\,) (<= (point) limit))
              (forward-char)
              (vala-syntax:skip-char-space)
              t
            )))))
; (vala-syntax:pskip-anytyped-definition-parameter-list (line-end-position))
;  ref CGI.defo<int> foll, out CGI.defo<int> foll,)

(defun vala-syntax:pskip-namespace-list ()
  "Highlight and skip a Vala symbol as namespace-face.
Used on using/namespace.
Text must be on point to start.
Limited by line end (not a valid match)
return: nothing interesting"
  (let ((anchor (point)))
   (while (and 
           (when (> (skip-syntax-forward "w_'" (line-end-position)) 0)
             (put-text-property anchor (point)
                               'font-lock-face
                               'font-lock-namespace-face)
             t)
           ;; test for the comma
           (when (and (= (char-after) ?\,))
             (forward-char)
             (message "hi")
             (vala-syntax:skip-char-space)
             (setq anchor (point))
             t)))
            ))
;(vala-syntax:pskip-namespace-list)gig, fig, sag; 



;;
;; propertize - other
;;

(defmacro vala-syntax:propertize-line-beginning-keywords (body)
  "Add properties to keywords which start lines.
For keywords which start lines and need more than a simple lock.
Our dpec anyhow - doesnt include if, while etc.
body: if all keyword matching fails, code in this parameter is executed.
"
  `(cond
    ((looking-at vala-syntax:class-definition-keywords-opt-re)

     (put-text-property (match-beginning 0) (match-end 0)
                        'font-lock-face
                        'font-lock-keyword-face)
     (goto-char (match-end 0))
     (vala-syntax:skip-char-space)
     ;;(message "  is named as class -%s-" (point)) 
     ;; check for an inheritance list, and apply properties
     (when (and (vala-syntax:pskip-class-definition
                 'font-lock-class-definition-face)
                (looking-at "[ ]*:[ ]*"))
       (goto-char (match-end 0))
       (vala-syntax:pskip-anytype-list)
       ))

    ;; delegate and signal
    ;; both followed by function-like syntax. Though this function could
    ;; handle that by recurse, propertize directly.
    ((looking-at "\\(?:delegate\\|signal\\)[ ]+")
     ;;(message "  is delegate/signal") 
     (put-text-property (match-beginning 0) (match-end 0)
                        'font-lock-face
                        'font-lock-keyword-face)
     (goto-char (match-end 0))
     (vala-syntax:pskip-anytype-symbol-pair))

    ;; enum...
    ((looking-at vala-syntax:classlike-definition-keywords-opt-re)
     ;;(message "  is enum")
     (put-text-property (match-beginning 0) (match-end 0)
                        'font-lock-face
                        'font-lock-keyword-face)
     (goto-char (match-end 0))
     (vala-syntax:skip-char-space)
     (vala-syntax:pskip-symbol-path
      'font-lock-class-definition-face))

    ;; new/throws
    ;; Propertywise, surprisingly similar. Both are secondary
    ;; parts of multipart constructs, and both are oprators for
    ;; class typing.
    ((looking-at "\\(new\\|throws\\)[ ]+") 
     ;;(message "  is new or throws")
     (put-text-property (match-beginning 1) (match-end 1)
                        'font-lock-face
                        'font-lock-minor-keywords-face)
     (goto-char (match-end 0))
     (vala-syntax:pskip-class-definition 'font-lock-type-face)
     )

    ;; var
    ((looking-at "var[ ]+")
     ;; Do nothing, but bail out
     )

    ;; case/default
    ;; ((looking-at vala-syntax:irregular-conditional-keywords-opt-re)
    ;;  (put-text-property (match-beginning 0) (match-end 0)
    ;;                     'font-lock-face
    ;;                     'font-lock-minor-keywords-face)
    ;;  (goto-char (match-end 0))
    ;;  (vala-syntax:pskip-switch-condition)
    ;;  )

    ;; using/namespace?
    ((looking-at "\\(?:using\\)[ ]+")
     (put-text-property (match-beginning 0) (match-end 0)
                        'font-lock-face
                        'font-lock-keyword-face)
     (goto-char (match-end 0))
     (vala-syntax:pskip-namespace-list))

    ;;TODO: whats the @ for?
    (t ,body)
    ))
;(macroexpand '(vala-syntax:propertize-line-beginning-keywords (message "failed!")))
;(vala-syntax:propertize-line-beginning-keywords
;(cond (message "failed!"))
;)signal 

;; Note about the propertize.
;; The syntactical line jumper does not move on it's keywords. So all
;; keywords (currently "throws"/"new")must make a move, otherwise the
;; parsing steps into an infinite loop.
(defun vala-syntax:propertize-from-left (limit)
  ;; skip intro space
  (vala-syntax:skip-char-space-tab)
 ;;(message "left propertize %s" (point))
  (cond
   ;; checking for some different formats of line start
   ;; single line comments
   ;;((looking-at vala-syntax:line-comment-start-re)
    ;;(vala-syntax:pskip-single-line-comment))

   ;; preprocessor directives
   ((looking-at vala-syntax:preprocessor-directive-re)
    (put-text-property (match-beginning 0) (match-end 0)
                       'font-lock-face
                       'font-lock-preprocessor-face)
     (goto-char (line-end-position)))

   ;; We don't try hard with attributes, so can use conventional
   ;; 'looking-at' detection
   ((vala-syntax:looking-at-code-attribute)
    (put-text-property (match-beginning 0) (match-end 0)
                       'font-lock-face
                       'font-lock-preprocessor-face))

   ;; breaking on uninteresting keywords may spare some performance,
   ;; but more importantly, some detection edge cases later.
   ((looking-at vala-syntax:non-propertized-keywords-opt-re) )

   (t
    ;;(message "left propertize %s" (point))
    ;; lock skip everything defined as OO keywords 
    (when (looking-at vala-syntax:oo-modifier-keywords-seq-re)
      (put-text-property (match-beginning 0) (match-end 0)
                         'font-lock-face
                         'font-lock-keyword-face)
      (goto-char (match-end 0)))

    ;; at this point we know little more than we have cleared modifiers
    (vala-syntax:skip-char-space)
    (vala-syntax:propertize-line-beginning-keywords
     (progn 
       ;; If no opening keywords, into the realms of substituting
       ;; detection heuristics for c-family long multiple-state
       ;; parsing.
       (let ((type-class (vala-syntax:class-or-class-level-definition-p2)))
         ;;(message " type return %s -%s-" type-class (point))
         (cond
          ;;failed. bail.
          ((null type-class))
          ((= type-class 0)
           ;; no movement
           ;; must be class inheritance item
           (vala-syntax:pskip-partial-anytype) 
           )
          ((= type-class 1)
           ;; moved out of curved brackets.
           ;; In vala syntax there are no curved brackets as part of
           ;; class definitions. bail.
           )
          ((= type-class 2)
           ;; moved out of curly brackets
           ;; could be a method, field item, constructor symbol, enum
           ;; element. Most of these will be highlighted as type, but
           ;; not enum elements.
           (when (vala-syntax:type-or-constructor-rough-p)
            (vala-syntax:pskip-partial-anytype)
            (vala-syntax:skip-char-space)
           ;; (message "  try pskipping method or field -%s-" (point))
            (vala-syntax:pskip-method-or-field-definition))
          )
          ((= type-class 3)
           ;; moved out of curved and curly brackets.
           ;; must be method definition parameters.
           (vala-syntax:pskip-anytyped-definition-parameter-list
            (line-end-position))
           )
          ;; There's no use in fail detection. Any of the above might
          ;; match, and if they make an unintended match, control
          ;; never reaches here.
          (t (message " unknown left item %s" (point))
         )
     ))))
    )))
;(vala-syntax:propertize-from-left (line-end-position))
;public

;; for testing
;; ensures point movement, and is interactive
(defun vala-syntax:propertize-from-left-interactive ()
  (interactive)
  (vala-syntax:propertize-from-left (point-max))
  ;;(vala-syntax:propertize-from-left end)
)

(defun vala-syntax:propertize-from-left-dummy (end)
   (forward-char))




(defun vala-syntax:propertize-syntactical-newline (start end)
  (goto-char start)
  (while
      (progn
        (vala-syntax:propertize-from-left end)
        ;; (vala-syntax:propertize-from-left-dummy end)
        (progn
          ;;(message "left propertize %s" (point))
          (vala-syntax:goto-syntactical-line-start end)
          )
        )))

(defun vala-syntax:propertize (start end)
  "See: syntax-propertize-function"
  (message "propertize area %s-%s" start end)
  ;; reset cache
  ;;(vala-syntax:reset-bracket-depth-cache))

  ;; propertize strings.
  ;; Unfortunately, added to the semantic parse, this means three
  ;; phase parsing for all text. But the string propertizing is a poor
  ;; fit to the newline skipping, and newline skipping is central to
  ;; our approach. A separate string propertize phase seems more
  ;; straightforward.

  ;; TODO: propertising of strings after the newline function means
  ;; strings in comments comments never get marked. Isn't there a
  ;; variable someplace making comments parathesis/string reactive?
  ;; Should this ordering be reacting to that?

  ;; Comments and strings are propertized before newlines. This means
  ;; the more detailed and complex newline propertise can do some
  ;; (comment skipping), if not general (not generics), sexp skipping.
  (vala-syntax:propertize-line-comments-and-strings start end)
  ;; propertise from newlines
  (vala-syntax:propertize-syntactical-newline start end)
  )


(provide 'vala-mode2-syntax)
