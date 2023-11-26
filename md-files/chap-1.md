



 

**1. Introduction**   





**1.1 Scope, Purpose, and History** 

**1.1.1 Scope and Purpose** 

The specification set forth in this document is designed to promote the portability of Common Lisp programs among a variety of data processing systems. It is a language specification aimed at an audience of implementors and knowledgeable programmers. It is neither a tutorial nor an implementation guide. 

**1.1.2 History** 

Lisp is a family of languages with a long history. Early key ideas in Lisp were developed by John McCarthy during the 1956 Dartmouth Summer Research Project on Artificial Intelligence. McCarthy’s motivation was to develop an algebraic list processing language for artificial intelligence work. Implementation efforts for early dialects of Lisp were undertaken on the IBM 704, the IBM 7090, the Digital Equipment Corporation (DEC) PDP-1, the DEC PDP-6, and the PDP-10. The primary dialect of Lisp between 1960 and 1965 was Lisp 1.5. By the early 1970’s there were two predominant dialects of Lisp, both arising from these early efforts: MacLisp and Interlisp. For further information about very early Lisp dialects, see *The Anatomy of Lisp* or *Lisp 1.5 Programmer’s Manual*. 

MacLisp improved on the Lisp 1.5 notion of special variables and error handling. MacLisp also introduced the concept of functions that could take a variable number of arguments, macros, arrays, non-local dynamic exits, fast arithmetic, the first good Lisp compiler, and an emphasis on execution speed. By the end of the 1970’s, MacLisp was in use at over 50 sites. For further information about Maclisp, see *Maclisp Reference Manual, Revision 0* or *The Revised Maclisp Manual*. 

Interlisp introduced many ideas into Lisp programming environments and methodology. One of the Interlisp ideas that influenced Common Lisp was an iteration construct implemented by Warren Teitelman that inspired the **loop** macro used both on the Lisp Machines and in MacLisp, and now in Common Lisp. For further information about Interlisp, see *Interlisp Reference Manual*. 

Although the first implementations of Lisp were on the IBM 704 and the IBM 7090, later work focussed on the DEC PDP-6 and, later, PDP-10 computers, the latter being the mainstay of Lisp and artificial intelligence work at such places as Massachusetts Institute of Technology (MIT), Stanford University, and Carnegie Mellon University (CMU) from the mid-1960’s through much of the 1970’s. The PDP-10 computer and its predecessor the PDP-6 computer were, by design, especially well-suited to Lisp because they had 36-bit words and 18-bit addresses. This architecture allowed a *cons* cell to be stored in one word; single instructions could extract the *car* and *cdr* parts. The PDP-6 and PDP-10 had fast, powerful stack instructions that enabled fast function calling. But the limitations of the PDP-10 were evident by 1973: it supported a small number of researchers using Lisp, and the small, 18-bit address space (2&#60;sup&#62;18&#60;/sup&#62; = 262,144 words) limited the size of a single program. One response to the address space problem was the Lisp Machine, a special-purpose computer designed to run Lisp programs. The other response was to use general-purpose computers  



with address spaces larger than 18 bits, such as the DEC VAX and the S-1 Mark IIA. For further information about S-1 Common Lisp, see “S-1 Common Lisp Implementation.” 

The Lisp machine concept was developed in the late 1960’s. In the early 1970’s, Peter Deutsch, working with Daniel Bobrow, implemented a Lisp on the Alto, a single-user minicomputer, using microcode to interpret a byte-code implementation language. Shortly thereafter, Richard Greenblatt began work on a different hardware and instruction set design at MIT. Although the Alto was not a total success as a Lisp machine, a dialect of Interlisp known as Interlisp-D became available on the D-series machines manufactured by Xerox—the Dorado, Dandelion, Dandetiger, and Dove (or Daybreak). An upward-compatible extension of MacLisp called Lisp Machine Lisp became available on the early MIT Lisp Machines. Commercial Lisp machines from Xerox, Lisp Machines (LMI), and Symbolics were on the market by 1981. For further information about Lisp Machine Lisp, see *Lisp Machine Manual*. 

During the late 1970’s, Lisp Machine Lisp began to expand towards a much fuller language. Sophisticated lambda lists, setf, multiple values, and structures like those in Common Lisp are the results of early experimentation with programming styles by the Lisp Machine group. Jonl White and others migrated these features to MacLisp. Around 1980, Scott Fahlman and others at CMU began work on a Lisp to run on the Scientific Personal Integrated Computing Environment (SPICE) workstation. One of the goals of the project was to design a simpler dialect than Lisp Machine Lisp. 

The Macsyma group at MIT began a project during the late 1970’s called the New Implementation of Lisp (NIL) for the VAX, which was headed by White. One of the stated goals of the NIL project was to fix many of the historic, but annoying, problems with Lisp while retaining significant compatibility with MacLisp. At about the same time, a research group at Stanford University and Lawrence Livermore National Laboratory headed by Richard P. Gabriel began the design of a Lisp to run on the S-1 Mark IIA supercomputer. S-1 Lisp, never completely functional, was the test bed for adapting advanced compiler techniques to Lisp implementation. Eventually the S-1 and NIL groups collaborated. For further information about the NIL project, see “NIL—A Perspective.” 

The first effort towards Lisp standardization was made in 1969, when Anthony Hearn and Martin Griss at the University of Utah defined Standard Lisp—a subset of Lisp 1.5 and other dialects—to transport REDUCE, a symbolic algebra system. During the 1970’s, the Utah group implemented first a retargetable optimizing compiler for Standard Lisp, and then an extended implementation 

known as Portable Standard Lisp (PSL). By the mid 1980’s, PSL ran on about a dozen kinds of computers. For further information about Standard Lisp, see “Standard LISP Report.” 

PSL and Franz Lisp—a MacLisp-like dialect for Unix machines—were the first examples of widely available Lisp dialects on multiple hardware platforms. 

One of the most important developments in Lisp occurred during the second half of the 1970’s: Scheme. Scheme, designed by Gerald J. Sussman and Guy L. Steele Jr., is a simple dialect of Lisp whose design brought to Lisp some of the ideas from programming language semantics developed in the 1960’s. Sussman was one of the prime innovators behind many other advances in Lisp technology from the late 1960’s through the 1970’s. The major contributions of Scheme were lexical scoping, lexical closures, first-class continuations, and simplified syntax (no separation of value cells 





and function cells). Some of these contributions made a large impact on the design of Common Lisp. For further information about Scheme, see *IEEE Standard for the Scheme Programming Language* or “Revised&#60;sup&#62;3&#60;/sup&#62; Report on the Algorithmic Language Scheme.” 

In the late 1970’s object-oriented programming concepts started to make a strong impact on Lisp. At MIT, certain ideas from Smalltalk made their way into several widely used programming systems. Flavors, an object-oriented programming system with multiple inheritance, was developed at MIT for the Lisp machine community by Howard Cannon and others. At Xerox, the experience with 

Smalltalk and Knowledge Representation Language (KRL) led to the development of Lisp Object Oriented Programming System (LOOPS) and later Common LOOPS. For further information on Smalltalk, see *Smalltalk-80: The Language and its Implementation*. For further information on Flavors, see *Flavors: A Non-Hierarchical Approach to Object-Oriented Programming*. 

These systems influenced the design of the Common Lisp Object System (CLOS). CLOS was developed specifically for this standardization effort, and was separately written up in “Common Lisp Object System Specification.” However, minor details of its design have changed slightly since that publication, and that paper should not be taken as an authoritative reference to the semantics of the object system as described in this document. 

In 1980 Symbolics and LMI were developing Lisp Machine Lisp; stock-hardware implementation groups were developing NIL, Franz Lisp, and PSL; Xerox was developing Interlisp; and the SPICE project at CMU was developing a MacLisp-like dialect of Lisp called SpiceLisp. 

In April 1981, after a DARPA-sponsored meeting concerning the splintered Lisp community, Symbolics, the SPICE project, the NIL project, and the S-1 Lisp project joined together to define Common Lisp. Initially spearheaded by White and Gabriel, the driving force behind this grassroots effort was provided by Fahlman, Daniel Weinreb, David Moon, Steele, and Gabriel. Common Lisp was designed as a description of a family of languages. The primary influences on Common Lisp were Lisp Machine Lisp, MacLisp, NIL, S-1 Lisp, Spice Lisp, and Scheme. *Common Lisp: The Language* is a description of that design. Its semantics were intentionally underspecified in places where it was felt that a tight specification would overly constrain Common Lisp research and use. 

In 1986 X3J13 was formed as a technical working group to produce a draft for an ANSI Common Lisp standard. Because of the acceptance of Common Lisp, the goals of this group differed from those of the original designers. These new goals included stricter standardization for portability, an object-oriented programming system, a condition system, iteration facilities, and a way to handle large character sets. To accommodate those goals, a new language specification, this document, was developed.  



**1.2 Organization of the Document** 

This is a reference document, not a tutorial document. Where possible and convenient, the order of presentation has been chosen so that the more primitive topics precede those that build upon them; however, linear readability has not been a priority. 

This document is divided into chapters by topic. Any given chapter might contain conceptual material, dictionary entries, or both. 

*Defined names* within the dictionary portion of a chapter are grouped in a way that brings related topics into physical proximity. Many such groupings were possible, and no deep significance should be inferred from the particular grouping that was chosen. To see *defined names* grouped alphabetically, consult the index. For a complete list of *defined names*, see Section 1.9 (Symbols in the COMMON-LISP Package). 

In order to compensate for the sometimes-unordered portions of this document, a glossary has been provided; see Chapter 26 (Glossary). The glossary provides connectivity by providing easy access to definitions of terms, and in some cases by providing examples or cross references to additional conceptual material. 

For information about notational conventions used in this document, see Section 1.4 (Definitions). For information about conformance, see Section 1.5 (Conformance). 

For information about extensions and subsets, see Section 1.6 (Language Extensions) and Section 1.7 (Language Subsets). 

For information about how *programs* in the language are parsed by the *Lisp reader* , see Chapter 2 (Syntax). 

For information about how *programs* in the language are *compiled* and *executed*, see Chapter 3 (Evaluation and Compilation). 

For information about data types, see Chapter 4 (Types and Classes). Not all *types* and *classes* are defined in this chapter; many are defined in chapter corresponding to their topic–for example, the numeric types are defined in Chapter 12 (Numbers). For a complete list of *standardized types*, see Figure 4–2. 

For information about general purpose control and data flow, see Chapter 5 (Data and Control Flow) or Chapter 6 (Iteration). 





**1.3 Referenced Publications** 

*• The Anatomy of Lisp*, John Allen, McGraw-Hill, Inc., 1978. 

*• The Art of Computer Programming, Volume 3* , Donald E. Knuth, Addison-Wesley Company (Reading, MA), 1973. 

*• The Art of the Metaobject Protocol*, Kiczales et al., MIT Press (Cambridge, MA), 1991. 

*•* “Common Lisp Object System Specification,” D. Bobrow, L. DiMichiel, R.P. Gabriel, S. Keene, G. Kiczales, D. Moon, *SIGPLAN Notices* V23, September, 1988. 

*• Common Lisp: The Language*, Guy L. Steele Jr., Digital Press (Burlington, MA), 1984. 

*• Common Lisp: The Language, Second Edition*, Guy L. Steele Jr., Digital Press (Bedford, MA), 1990. 

*• Exceptional Situations in Lisp*, Kent M. Pitman, *Proceedings of the First European Conference on the Practical Application of LISP* (EUROPAL ’90), Churchill College, Cambridge, England, March 27-29, 1990. 

*• Flavors: A Non-Hierarchical Approach to Object-Oriented Programming*, Howard I. Cannon, 1982. 

*• IEEE Standard for Binary Floating-Point Arithmetic*, ANSI/IEEE Std 754-1985, Institute of Electrical and Electronics Engineers, Inc. (New York), 1985. 

*• IEEE Standard for the Scheme Programming Language*, IEEE Std 1178-1990, Institute of Electrical and Electronic Engineers, Inc. (New York), 1991. 

*• Interlisp Reference Manual*, Third Revision, Teitelman, Warren, et al, Xerox Palo Alto Research Center (Palo Alto, CA), 1978. 

*•* ISO 6937/2, *Information processing—Coded character sets for text communication—Part 2: Latin alphabetic and non-alphabetic graphic characters*, ISO, 1983. 

*• Lisp 1.5 Programmer’s Manual*, John McCarthy, MIT Press (Cambridge, MA), August, 1962. 

*• Lisp Machine Manual*, D.L. Weinreb and D.A. Moon, Artificial Intelligence Laboratory, MIT (Cambridge, MA), July, 1981. 

*• Maclisp Reference Manual, Revision 0* , David A. Moon, Project MAC (Laboratory for Computer Science), MIT (Cambridge, MA), March, 1974.  



*•* “NIL—A Perspective,” JonL White, *Macsyma User’s Conference*, 1979. 

*• Performance and Evaluation of Lisp Programs*, Richard P. Gabriel, MIT Press (Cambridge, MA), 1985. 

*•* “Principal Values and Branch Cuts in Complex APL,” Paul Penfield Jr., *APL 81 Conference Proceedings*, ACM SIGAPL (San Francisco, September 1981), 248-256. Proceedings published as *APL Quote Quad 12*, 1 (September 1981). 

*• The Revised Maclisp Manual*, Kent M. Pitman, Technical Report 295, Laboratory for Computer Science, MIT (Cambridge, MA), May 1983. 

*•* “Revised&#60;sup&#62;3&#60;/sup&#62; Report on the Algorithmic Language Scheme,” Jonathan Rees and William Clinger (editors), *SIGPLAN Notices* V21, #12, December, 1986. 

*•* “S-1 Common Lisp Implementation,” R.A. Brooks, R.P. Gabriel, and G.L. Steele, *Conference Record of the 1982 ACM Symposium on Lisp and Functional Programming*, 108-113, 1982. 

*• Smalltalk-80: The Language and its Implementation*, A. Goldberg and D. Robson, Addison Wesley, 1983. 

*•* “Standard LISP Report,” J.B. Marti, A.C. Hearn, M.L. Griss, and C. Griss, *SIGPLAN Notices* V14, #10, October, 1979. 

*• Webster’s Third New International Dictionary the English Language, Unabridged*, Merriam Webster (Springfield, MA), 1986. 

*• XP: A Common Lisp Pretty Printing System*, R.C. Waters, Memo 1102a, Artificial Intelligence Laboratory, MIT (Cambridge, MA), September 1989. 





**1.4 Definitions** 

This section contains notational conventions and definitions of terms used in this manual.

 **1.4.1 Notational Conventions**

 

The following notational conventions are used throughout this document. 

**1.4.1.1 Font Key** 

Fonts are used in this document to convey information. 

*name* 

Denotes a formal term whose meaning is defined in the Glossary. When this font is used, the Glossary definition takes precedence over normal English usage. 

Sometimes a glossary term appears subscripted, as in “*whitespace*&#60;sub&#62;2&#60;/sub&#62;.” Such a notation selects one particular Glossary definition out of several, in this case the second. The subscript notation for Glossary terms is generally used where the context might be insufficient to disambiguate among the available definitions. 

*name* 

Denotes the introduction of a formal term locally to the current text. There is still a corresponding glossary entry, and is formally equivalent to a use of “*name*,” but the hope is that making such uses conspicuous will save the reader a trip to the glossary in some cases. 

**name** 

Denotes a symbol in the COMMON-LISP *package*. For information about *case* conventions, see Section 1.4.1.4.1 (Case in Symbols). 

name 

Denotes a sample *name* or piece of *code* that a programmer might write in Common Lisp. 

This font is also used for certain *standardized* names that are not names of *external symbols* of the COMMON-LISP *package*, such as *keywords*&#60;sub&#62;1&#60;/sub&#62;, *package names*, and *loop keywords*. 

*name* 

Denotes the name of a *parameter* or *value*. 

In some situations the notation “&#10216;name&#10217;” (*i.e.*, the same font, but with surrounding “angle brackets”) is used instead in order to provide better visual separation from surrounding characters. These “angle brackets” are metasyntactic, and never actually appear in program input or output.  



**1.4.1.2 Modified BNF Syntax** 

This specification uses an extended Backus Normal Form (BNF) to describe the syntax of Common Lisp *macro forms* and *special forms*. This section discusses the syntax of BNF expressions. 

**1.4.1.2.1 Splicing in Modified BNF Syntax** 

The primary extension used is the following: 

[[ *O* ]] 

An expression of this form appears whenever a list of elements is to be spliced into a larger structure and the elements can appear in any order. The symbol *O* represents a description of the syntax of some number of syntactic elements to be spliced; that description must be of the form 

&#60;i&#62;O&#60;/i&#62;&#60;sub&#62;1&#60;/sub&#62; &#60;i&#62;| . . . | O&#60;sub&#62;l&#60;/sub&#62;&#60;/i&#62; 

where each &#60;i&#62;O&#60;sub&#62;i&#60;/sub&#62;&#60;/i&#62; can be of the form &#60;i&#62;S&#60;/i&#62; or of the form &#60;i&#62;S&#60;/i&#62;* or of the form &#60;i&#62;S&#60;/i&#62;&#60;sup&#62;1&#60;/sup&#62;. The expression [[ &#60;i&#62;O&#60;/i&#62; ]] means that a list of the form 

(&#60;i&#62;O&#60;sub&#62;i&#60;/sub&#62;&#60;/i&#62;1&#60;/sub&#62;&#60;i&#62;. . . O&#60;sub&#62;ij&#60;/sub&#62;&#60;/i&#62;) 1 &#60;i&#62;≤ j&#60;/i&#62; 

is spliced into the enclosing expression, such that if &#60;i&#62;n 6&#60;/i&#62;= &#60;i&#62;m&#60;/i&#62; and 1 &#60;i&#62;≤ n, m ≤ j&#60;/i&#62;, then either &#60;i&#62;O&#60;sub&#62;in&#60;/sub&#62;6&#60;/i&#62;= &#60;i&#62;O&#60;sub&#62;im&#60;/sub&#62;&#60;/i&#62; or &#60;i&#62;O&#60;sub&#62;in&#60;/sub&#62;&#60;/i&#62; = &#60;i&#62;O&#60;sub&#62;im&#60;/sub&#62;&#60;/i&#62; = &#60;i&#62;Q&#60;sub&#62;k&#60;/sub&#62;&#60;/i&#62;, where for some 1 &#60;i&#62;≤ k ≤ n&#60;/i&#62;, &#60;i&#62;O&#60;sub&#62;k&#60;/sub&#62;&#60;/i&#62; is of the form &#60;i&#62;Q&#60;sub&#62;k&#60;/sub&#62;&#60;/i&#62;*. Furthermore, for each &#60;i&#62;O&#60;sub&#62;in&#60;/sub&#62;&#60;/i&#62; that is of the form &#60;i&#62;Q&#60;sub&#62;k&#60;/sub&#62;&#60;/i&#62;&#60;sup&#62;1&#60;/sup&#62;, that element is required to appear somewhere in the list to be spliced. 

For example, the expression 

(x [[ A | B\* | C ]] y) 

means that at most one A, any number of B’s, and at most one C can occur in any order. It is a description of any of these: 

(x y) 

(x B A C y) 

(x A B B B B B C y) 

(x C B A B B B y) 

but not any of these: 

(x B B A A C C y) 

(x C B C y) 

In the first case, both A and C appear too often, and in the second case C appears too often. 

The notation [[ *O*&#60;sub&#62;1&#60;/sub&#62; *| O*&#60;sub&#62;2&#60;/sub&#62; *| . . .* ]]&#60;sup&#62;+&#60;/sup&#62;adds the additional restriction that at least one item from among the possible choices must be used. For example: 





(x [[ A | B\* | C ]]&#60;sup&#62;+&#60;/sup&#62;y) 

means that at most one A, any number of B’s, and at most one C can occur in any order, but that in any case at least one of these options must be selected. It is a description of any of these: 

(x B y) 

(x B A C y) 

(x A B B B B B C y) 

(x C B A B B B y) 

but not any of these: 

(x y) 

(x B B A A C C y) 

(x C B C y) 

In the first case, no item was used; in the second case, both A and C appear too often; and in the third case C appears too often. 

Also, the expression: 

(x [[ A&#60;sup&#62;1&#60;/sup&#62;| B&#60;sup&#62;1&#60;/sup&#62;| C ]] y) 

can generate exactly these and no others: 

(x A B C y) 

(x A C B y) 

(x A B y) 

(x B A C y) 

(x B C A y) 

(x B A y) 

(x C A B y) 

(x C B A y) 

**1.4.1.2.2 Indirection in Modified BNF Syntax** 

An indirection extension is introduced in order to make this new syntax more readable: *↓O* 

If *O* is a non-terminal symbol, the right-hand side of its definition is substituted for the entire expression *↓O*. For example, the following BNF is equivalent to the BNF in the previous example: 

(x [[ *↓O* ]] y) 

*O::*=A *|* B\* *|* C  



**1.4.1.2.3 Additional Uses for Indirect Definitions in Modified BNF Syntax** 

In some cases, an auxiliary definition in the BNF might appear to be unused within the BNF, but might still be useful elsewhere. For example, consider the following definitions: 

**case** *keyform &#123;↓normal-clause&#125;*\* [*↓otherwise-clause*] *→ &#123;result&#125;*\* 

**ccase** *keyplace &#123;↓normal-clause&#125;*\* *→ &#123;result&#125;*\* 

**ecase** *keyform &#123;↓normal-clause&#125;*\* *→ &#123;result&#125;*\* 

*normal-clause::*=(*keys &#123;form&#125;*\*) 

*otherwise-clause::*=(*&#123;otherwise | t&#125; &#123;form&#125;*\*) 

*clause::*=*normal-clause | otherwise-clause* 

Here the term “*clause*” might appear to be “dead” in that it is not used in the BNF. However, the purpose of the BNF is not just to guide parsing, but also to define useful terms for reference in the descriptive text which follows. As such, the term “*clause*” might appear in text that follows, as shorthand for “*normal-clause* or *otherwise-clause*.” 

**1.4.1.3 Special Symbols** 

The special symbols described here are used as a notational convenience within this document, and are part of neither the Common Lisp language nor its environment. 

*→* 

This indicates evaluation. For example: 

(+ 4 5) *→* 9 

This means that the result of evaluating the *form* (+ 4 5) is 9. 

If a *form* returns *multiple values*, those values might be shown separated by spaces, line breaks, or commas. For example: 

(truncate 7 5) 

*→* 1 2 

(truncate 7 5) 

*→* 1 

2 

(truncate 7 5) 

*→* 1, 2 

Each of the above three examples is equivalent, and specifies that (truncate 7 5) returns two values, which are 1 and 2. 





Some *conforming implementations* actually type an arrow (or some other indicator) before showing return values, while others do not. 

&#60;i&#62;&#60;sup&#62;or&#60;/sup&#62;→&#60;/i&#62; 

The notation “ &#60;i&#62;&#60;sup&#62;or&#60;/sup&#62;→&#60;/i&#62;” is used to denote one of several possible alternate results. The example 

(char-name #\a) 

*→* NIL 

&#60;i&#62;&#60;sup&#62;or&#60;/sup&#62;→&#60;/i&#62; "LOWERCASE-a" 

&#60;i&#62;&#60;sup&#62;or&#60;/sup&#62;→&#60;/i&#62; "Small-A" 

&#60;i&#62;&#60;sup&#62;or&#60;/sup&#62;→&#60;/i&#62; "LA01" 

indicates that **nil**, "LOWERCASE-a", "Small-A", "LA01" are among the possible results of (char-name #\a)—each with equal preference. Unless explicitly specified otherwise, it should not be assumed that the set of possible results shown is exhaustive. Formally, the above example is equivalent to 

(char-name #\a) *→ implementation-dependent* 

but it is intended to provide additional information to illustrate some of the ways in which it is permitted for implementations to diverge. 

&#60;i&#62;&#60;sup&#62;not&#60;/sup&#62; →&#60;/i&#62; 

The notation “&#60;i&#62;&#60;sup&#62;not&#60;/sup&#62; →&#60;/i&#62;” is used to denote a result which is not possible. This might be used, for example, in order to emphasize a situation where some anticipated misconception might lead the reader to falsely believe that the result might be possible. For example, 

(function-lambda-expression 

(funcall #’(lambda (x) #’(lambda () x)) nil)) 

*→* NIL, *true*, NIL 

&#60;i&#62;&#60;sup&#62;or&#60;/sup&#62;→&#60;/i&#62; (LAMBDA () X), &#60;i&#62;true&#60;/i&#62;, NIL 

&#60;i&#62;&#60;sup&#62;not&#60;/sup&#62; →&#60;/i&#62; NIL, &#60;i&#62;false&#60;/i&#62;, NIL 

&#60;i&#62;&#60;sup&#62;not&#60;/sup&#62; →&#60;/i&#62; (LAMBDA () X), &#60;i&#62;false&#60;/i&#62;, NIL 

*≡* 

This indicates code equivalence. For example: 

(gcd x (gcd y z)) *≡* (gcd (gcd x y) z) 

This means that the results and observable side-effects of evaluating the *form* 

(gcd x (gcd y z)) are always the same as the results and observable side-effects of (gcd (gcd x y) z) for any x, y, and z. 

&#9655;  



Common Lisp specifies input and output with respect to a non-interactive stream model. The specific details of how interactive input and output are mapped onto that non-interactive model are *implementation-defined*. 

For example, *conforming implementations* are permitted to differ in issues of how interactive input is terminated. For example, the *function* **read** terminates when the final delimiter is typed on a non-interactive stream. In some *implementations*, an interactive call to **read** returns as soon as the final delimiter is typed, even if that delimiter is not a *newline*. In other *implementations*, a final *newline* is always required. In still other *implementations*, there might be a command which “activates” a buffer full of input without the command itself being visible on the program’s input stream. 

In the examples in this document, the notation “&#9655;” precedes lines where interactive input and output occurs. Within such a scenario, “this notation” notates user input. 

For example, the notation 

(+ 1 (print (+ (sqrt (read)) (sqrt (read))))) 

&#9655; 9 16 

&#9655; 7 

*→* 8 

shows an interaction in which “(+ 1 (print (+ (sqrt (read)) (sqrt (read)))))” is a *form* to be *evaluated*, “9 16 ” is interactive input, “7” is interactive output, and “8” is the *value yielded* from the *evaluation*. 

The use of this notation is intended to disguise small differences in interactive input and output behavior between *implementations*. 

Sometimes, the non-interactive stream model calls for a *newline*. How that *newline* character is interactively entered is an *implementation-defined* detail of the user interface, but in that case, either the notation “*hNewlinei*” or “*←-*” might be used. 

(progn (format t "~&Who? ") (read-line)) 

&#9655; Who? Fred, Mary, and Sally*←-* 

*→* "Fred, Mary, and Sally", *~~fal~~se* 

**1.4.1.4 Objects with Multiple Notations** 

Some *objects* in Common Lisp can be notated in more than one way. In such situations, the choice of which notation to use is technically arbitrary, but conventions may exist which convey a “point of view” or “sense of intent.” 

**1.4.1.4.1 Case in Symbols** 

While *case* is significant in the process of *interning* a *symbol*, the *Lisp reader* , by default, attempts to canonicalize the case of a *symbol* prior to interning; see Section 23.1.2 (Effect of Readtable Case on the Lisp Reader). As such, case in *symbols* is not, by default, significant. Throughout 





this document, except as explicitly noted otherwise, the case in which a *symbol* appears is not significant; that is, HELLO, Hello, hElLo, and hello are all equivalent ways to denote a symbol whose name is "HELLO". 

The characters *backslash* and *vertical-bar* are used to explicitly quote the *case* and other parsing related aspects of characters. As such, the notations |hello| and \h\e\l\l\o are equivalent ways to refer to a symbol whose name is "hello", and which is *distinct* from any symbol whose name is "HELLO". 

The *symbols* that correspond to Common Lisp *defined names* have *uppercase* names even though their names generally appear in *lowercase* in this document. 

**1.4.1.4.2 Numbers** 

Although Common Lisp provides a variety of ways for programs to manipulate the input and output radix for rational numbers, all numbers in this document are in decimal notation unless explicitly noted otherwise. 

**1.4.1.4.3 Use of the Dot Character** 

The dot appearing by itself in an *expression* such as 

(*item1 item2* . *tail*) 

means that *tail* represents a *list* of *objects* at the end of a list. For example, 

(A B C . (D E F)) 

is notationally equivalent to: 

(A B C D E F) 

Although *dot* is a valid constituent character in a symbol, no *standardized symbols* contain the character *dot*, so a period that follows a reference to a *symbol* at the end of a sentence in this document should always be interpreted as a period and never as part of the *symbol*’s *name*. For example, within this document, a sentence such as “This sample sentence refers to the symbol **car**.” refers to a symbol whose name is "CAR" (with three letters), and never to a four-letter symbol "CAR." 

**1.4.1.4.4 NIL** 

**nil** has a variety of meanings. It is a *symbol* in the COMMON-LISP *package* with the *name* "NIL", it is *boolean* (and *generalized boolean*) *false*, it is the *empty list*, and it is the *name* of the *empty type* (a *subtype* of all *types*). 

Within Common Lisp, **nil** can be notated interchangeably as either NIL or (). By convention, the choice of notation offers a hint as to which of its many roles it is playing.  



|**For Evaluation? Notation Typically Implied Role**|
| :- |
|&#60;p&#62;Yes nil use as a *boolean*. &#60;/p&#62;&#60;p&#62;Yes ’nil use as a *symbol*. &#60;/p&#62;&#60;p&#62;Yes ’() use as an *empty list* &#60;/p&#62;&#60;p&#62;No nil use as a *symbol* or *boolean*. No () use as an *empty list*.&#60;/p&#62;|


**Figure 1–1. Notations for NIL** 

Within this document only, **nil** is also sometimes notated as *false* to emphasize its role as a *boolean*. For example: 

(print ()) ;avoided 

(defun three nil 3) ;avoided 

’(nil nil) ;list of two symbols 

’(() ()) ;list of empty lists 

(defun three () 3) ;Emphasize empty parameter list. 

(append ’() ’()) *→* () ;Emphasize use of empty lists 

(not nil) *→ true* ;Emphasize use as Boolean false 

(get ’nil ’color) ;Emphasize use as a symbol 

A *function* is sometimes said to “be *false*” or “be *true*” in some circumstance. Since no *function* object can be the same as **nil** and all *function objects* represent *true* when viewed as *booleans*, it would be meaningless to say that the *function* was literally *false* and uninteresting to say that it was literally *true*. Instead, these phrases are just traditional alternative ways of saying that the 

*function* “returns *false*” or “returns *true*,” respectively. 

**1.4.1.5 Designators** 

A *designator* is an *object* that denotes another *object*. 

Where a *parameter* of an *operator* is described as a *designator* , the description of the *operator* is written in a way that assumes that the value of the *parameter* is the denoted *object*; that is, that the *parameter* is already of the denoted *type*. (The specific nature of the *object* denoted by a “&#10216;typeii designator*” or a “*designator* for a &#10216;type&#10217;” can be found in the Glossary entry for “&#10216;typeii designator* .”) 

For example, “**nil**” and “the *value* of **\*standard-output\***” are operationally indistinguishable as *stream designators*. Similarly, the *symbol* foo and the *string* "FOO" are operationally indistinguishable as *string designators*. 

Except as otherwise noted, in a situation where the denoted *object* might be used multiple times, it is *implementation-dependent* whether the *object* is coerced only once or whether the coercion occurs each time the *object* must be used. 





For example, **mapcar** receives a *function designator* as an argument, and its description is written as if this were simply a function. In fact, it is *implementation-dependent* whether the *function designator* is coerced right away or whether it is carried around internally in the form that it was given as an *argument* and re-coerced each time it is needed. In most cases, *conforming programs* cannot detect the distinction, but there are some pathological situations (particularly those involving self-redefining or mutually-redefining functions) which do conform and which can detect this difference. The following program is a *conforming program*, but might or might not have portably correct results, depending on whether its correctness depends on one or the other of the results: 

(defun add-some (x) 

(defun add-some (x) (+ x 2)) 

(+ x 1)) *→* ADD-SOME 

(mapcar ’add-some ’(1 2 3 4)) 

*→* (2 3 4 5) 

&#60;i&#62;&#60;sup&#62;or&#60;/sup&#62;→&#60;/i&#62; (2 4 5 6) 

In a few rare situations, there may be a need in a dictionary entry to refer to the *object* that was the original *designator* for a *parameter* . Since naming the *parameter* would refer to the denoted *object*, the phrase “the &#10216;parameter-nameii designator*” can be used to refer to the *designator* which was the *argument* from which the *value* of &#10216;parameter-name&#10217; was computed. 

**1.4.1.6 Nonsense Words** 

When a word having no pre-attached semantics is required (*e.g.*, in an example), it is common in the Lisp community to use one of the words “foo,” “bar,” “baz,” and “quux.” For example, in 

(defun foo (x) (+ x 1)) 

the use of the name foo is just a shorthand way of saying “please substitute your favorite name here.” 

These nonsense words have gained such prevalance of usage, that it is commonplace for newcomers to the community to begin to wonder if there is an attached semantics which they are overlooking—there is not. 

**1.4.2 Error Terminology** 

Situations in which errors might, should, or must be signaled are described in the standard. The wording used to describe such situations is intended to have precise meaning. The following list is a glossary of those meanings. 

**Safe code** 

This is *code* processed with the **safety** optimization at its highest setting (3). **safety** is a lexical property of code. The phrase “the function F should signal an error” means that if F is invoked from code processed with the highest **safety** optimization, an error is signaled. It is *implementation-dependent* whether F or the calling code signals the error.  



**Unsafe code** 

This is code processed with lower safety levels. 

Unsafe code might do error checking. Implementations are permitted to treat all code as safe code all the time. 

**An error is signaled** 

This means that an error is signaled in both safe and unsafe code. *Conforming code* may rely on the fact that the error is signaled in both safe and unsafe code. Every implementation is required to detect the error in both safe and unsafe code. For example, “an error is signaled if **unexport** is given a *symbol* not *accessible* in the *current package*.” 

If an explicit error type is not specified, the default is **error**. 

**An error should be signaled** 

This means that an error is signaled in safe code, and an error might be signaled in unsafe code. *Conforming code* may rely on the fact that the error is signaled in safe code. Every implementation is required to detect the error at least in safe code. When the error is not signaled, the “consequences are undefined” (see below). For example, “**+** should signal an error of *type* **type-error** if any argument is not of *type* **number**.” 

**Should be prepared to signal an error** 

This is similar to “should be signaled” except that it does not imply that ‘extra effort’ has to be taken on the part of an *operator* to discover an erroneous situation if the normal action of that *operator* can be performed successfully with only ‘lazy’ checking. An *implementation* is always permitted to signal an error, but even in *safe code*, it is only required to signal the error when failing to signal it might lead to incorrect results. In *unsafe code*, the consequences are undefined. 

For example, defining that “**find** should be prepared to signal an error of *type* **type-error** if its second *argument* is not a *proper list*” does not imply that an error is always signaled. The *form* 

(find ’a ’(a b . c)) 

must either signal an error of *type* **type-error** in *safe code*, else return A. In *unsafe code*, the consequences are undefined. By contrast, 

(find ’d ’(a b . c)) 

must signal an error of *type* **type-error** in *safe code*. In *unsafe code*, the consequences are undefined. Also, 

(find ’d ’#1=(a b . #1#)) 





in *safe code* might return **nil** (as an *implementation-defined* extension), might never return, or might signal an error of *type* **type-error**. In *unsafe code*, the consequences are undefined. 

Typically, the “should be prepared to signal” terminology is used in type checking situations where there are efficiency considerations that make it impractical to detect errors that are not relevant to the correct operation of the *operator* . 

**The consequences are unspecified** 

This means that the consequences are unpredictable but harmless. Implementations are permitted to specify the consequences of this situation. No *conforming code* may depend on the results or effects of this situation, and all *conforming code* is required to treat the 

results and effects of this situation as unpredictable but harmless. For example, “if the second argument to **shared-initialize** specifies a name that does not correspond to any *slots accessible* in the *object*, the results are unspecified.” 

**The consequences are undefined** 

This means that the consequences are unpredictable. The consequences may range from harmless to fatal. No *conforming code* may depend on the results or effects. *Conforming code* must treat the consequences as unpredictable. In places where the words “must,” “must not,” or “may not” are used, then “the consequences are undefined” if the stated requirement is not met and no specific consequence is explicitly stated. An implementation is permitted to signal an error in this case. 

For example: “Once a name has been declared by **defconstant** to be constant, any further assignment or binding of that variable has undefined consequences.” 

**An error might be signaled** 

This means that the situation has undefined consequences; however, if an error is signaled, it is of the specified *type*. For example, “**open** might signal an error of *type* **file-error**.” 

**The return values are unspecified** 

This means that only the number and nature of the return values of a *form* are not specified. However, the issue of whether or not any side-effects or transfer of control occurs is still well-specified. 

A program can be well-specified even if it uses a function whose returns values are unspecified. For example, even if the return values of some function F are unspecified, an expression such as (length (list (F))) is still well-specified because it does not rely on any particular aspect of the value or values returned by F. 

**Implementations may be extended to cover this situation** 

This means that the *situation* has undefined consequences; however, a *conforming* Introduction **1–17**





*implementation* is free to treat the situation in a more specific way. For example, an *implementation* might define that an error is signaled, or that an error should be signaled, or even that a certain well-defined non-error behavior occurs. 

No *conforming code* may depend on the consequences of such a *situation*; all *conforming code* must treat the consequences of the situation as undefined. *Implementations* are required to document how the situation is treated. 

For example, “implementations may be extended to define other type specifiers to have a corresponding *class*.” 

**Implementations are free to extend the syntax** 

This means that in this situation implementations are permitted to define unambiguous extensions to the syntax of the *form* being described. No *conforming code* may depend on this extension. Implementations are required to document each such extension. All 

*conforming code* is required to treat the syntax as meaningless. The standard might disallow certain extensions while allowing others. For example, “no implementation is free to extend the syntax of **defclass**.” 

**A warning might be issued** 

This means that *implementations* are encouraged to issue a warning if the context is appropriate (*e.g.*, when compiling). However, a *conforming implementation* is not required to issue a warning. 

**1.4.3 Sections Not Formally Part Of This Standard** 

Front matter and back matter, such as the “Table of Contents,” “Index,” “Figures,” “Credits,” and “Appendix” are not considered formally part of this standard, so that we retain the flexibility needed to update these sections even at the last minute without fear of needing a formal vote to change those parts of the document. These items are quite short and very useful, however, and it is not recommended that they be removed even in an abridged version of this document. 

Within the concept sections, subsections whose names begin with the words “Note” or “Notes” or “Example” or “Examples” are provided for illustration purposes only, and are not considered part of the standard. 

An attempt has been made to place these sections last in their parent section, so that they could be removed without disturbing the contiguous numbering of the surrounding sections in order to produce a document of smaller size. 

Likewise, the “Examples” and “Notes” sections in a dictionary entry are not considered part of the standard and could be removed if necessary. 

Nevertheless, the examples provide important clarifications and consistency checks for the rest of the material, and such abridging is not recommended unless absolutely unavoidable. 





**1.4.4 Interpreting Dictionary Entries** 

The dictionary entry for each *defined name* is partitioned into sections. Except as explicitly indicated otherwise below, each section is introduced by a label identifying that section. The omission of a section implies that the section is either not applicable, or would provide no interesting information. 

This section defines the significance of each potential section in a dictionary entry. 

**1.4.4.1 The “Affected By” Section of a Dictionary Entry** 

For an *operator* , anything that can affect the side effects of or *values* returned by the *operator* . 

For a *variable*, anything that can affect the *value* of the *variable* including *functions* that bind or assign it. 

**1.4.4.2 The “Arguments” Section of a Dictionary Entry** 

This information describes the syntax information of entries such as those for *declarations* and special *expressions* which are never *evaluated* as *forms*, and so do not return *values*. 

**1.4.4.3 The “Arguments and Values” Section of a Dictionary Entry** 

An English language description of what *arguments* the *operator* accepts and what *values* it returns, including information about defaults for *parameters* corresponding to omittable *arguments* (such as *optional parameters* and *keyword parameters*). For *special operators* and *macros*, their *arguments* are not *evaluated* unless it is explicitly stated in their descriptions that they are *evaluated*. 

Except as explicitly specified otherwise, the consequences are undefined if these type restrictions are violated. 

**1.4.4.4 The “Binding Types Affected” Section of a Dictionary Entry** 

This information alerts the reader to the kinds of *bindings* that might potentially be affected by a declaration. Whether in fact any particular such *binding* is actually affected is dependent on additional factors as well. See the “Description” section of the declaration in question for details.  



**1.4.4.5 The “Class Precedence List” Section of a Dictionary Entry** 

This appears in the dictionary entry for a *class*, and contains an ordered list of the *classes* defined by Common Lisp that must be in the *class precedence list* of this *class*. 

It is permissible for other (*implementation-defined*) *classes* to appear in the *implementation*’s *class precedence list* for the *class*. 

It is permissible for either **standard-object** or **structure-object** to appear in the *implementation*’s *class precedence list*; for details, see Section 4.2.2 (Type Relationships). 

Except as explicitly indicated otherwise somewhere in this specification, no additional *standardized classes* may appear in the *implementation*’s *class precedence list*. 

By definition of the relationship between *classes* and *types*, the *classes* listed in this section are also *supertypes* of the *type* denoted by the *class*. 

**1.4.4.6 Dictionary Entries for Type Specifiers** 

The *atomic type specifiers* are those *defined names* listed in Figure 4–2. Such dictionary entries are of kind “Class,” “Condition Type,” “System Class,” or “Type.” A description of how to interpret a *symbol* naming one of these *types* or *classes* as an *atomic type specifier* is found in the “Description” section of such dictionary entries. 

The *compound type specifiers* are those *defined names* listed in Figure 4–3. Such dictionary entries are of kind “Class,” “System Class,” “Type,” or “Type Specifier.” A description of how to interpret as a *compound type specifier* a *list* whose *car* is such a *symbol* is found in the “Compound Type Specifier Kind,” “Compound Type Specifier Syntax,” “Compound Type Specifier Arguments,” and “Compound Type Specifier Description” sections of such dictionary entries. 

**1.4.4.6.1 The “Compound Type Specifier Kind” Section of a Dictionary Entry** 

An “abbreviating” *type specifier* is one that describes a *subtype* for which it is in principle possible to enumerate the *elements*, but for which in practice it is impractical to do so. 

A “specializing” *type specifier* is one that describes a *subtype* by restricting the *type* of one or more components of the *type*, such as *element type* or *complex part type*. 

A “predicating” *type specifier* is one that describes a *subtype* containing only those *objects* that satisfy a given *predicate*. 

A “combining” *type specifier* is one that describes a *subtype* in a compositional way, using combining operations (such as “and,” “or,” and “not”) on other *types*. 





**1.4.4.6.2 The “Compound Type Specifier Syntax” Section of a Dictionary Entry** This information about a *type* describes the syntax of a *compound type specifier* for that *type*. 

Whether or not the *type* is acceptable as an *atomic type specifier* is not represented here; see Section 1.4.4.6 (Dictionary Entries for Type Specifiers). 

**1.4.4.6.3 The “Compound Type Specifier Arguments” Section of a Dictionary Entry** 

This information describes *type* information for the structures defined in the “Compound Type Specifier Syntax” section. 

**1.4.4.6.4 The “Compound Type Specifier Description” Section of a Dictionary Entry** 

This information describes the meaning of the structures defined in the “Compound Type Specifier Syntax” section. 

**1.4.4.7 The “Constant Value” Section of a Dictionary Entry** This information describes the unchanging *type* and *value* of a *constant variable*. 

**1.4.4.8 The “Description” Section of a Dictionary Entry** 

A summary of the *operator* and all intended aspects of the *operator* , but does not necessarily include all the fields referenced below it (“Side Effects,” “Exceptional Situations,” *etc.*) 

**1.4.4.9 The “Examples” Section of a Dictionary Entry** 

Examples of use of the *operator* . These examples are not considered part of the standard; see Section 1.4.3 (Sections Not Formally Part Of This Standard). 

**1.4.4.10 The “Exceptional Situations” Section of a Dictionary Entry** Three kinds of information may appear here: 

*•* Situations that are detected by the *function* and formally signaled. 

*•* Situations that are handled by the *function*. 

*•* Situations that may be detected by the *function*. 

This field does not include conditions that could be signaled by *functions* passed to and called by this *operator* as arguments or through dynamic variables, nor by executing subforms of this operator if it is a *macro* or *special operator* .  



**1.4.4.11 The “Initial Value” Section of a Dictionary Entry** 

This information describes the initial *value* of a *dynamic variable*. Since this variable might change, see *type* restrictions in the “Value Type” section. 

**1.4.4.12 The “Argument Precedence Order” Section of a Dictionary Entry** 

This information describes the *argument precedence order* . If it is omitted, the *argument precedence order* is the default (left to right). 

**1.4.4.13 The “Method Signature” Section of a Dictionary Entry** 

The description of a *generic function* includes descriptions of the *methods* that are defined on that *generic function* by the standard. A method signature is used to describe the *parameters* and *parameter specializers* for each *method*. *Methods* defined for the *generic function* must be of the form described by the *method signature*. 

**F** (*x class*) (*y t*) &optional *z* &key *k* 

This *signature* indicates that this method on the *generic function* **F** has two *required parameters*: *x*, which must be a *generalized instance* of the *class class*; and *y*, which can be any *object* (*i.e.*, a *generalized instance* of the *class* **t**). In addition, there is an *optional parameter z* and a *keyword parameter k*. This *signature* also indicates that this method on F is a *primary method* and has no *qualifiers*. 

For each *parameter* , the *argument* supplied must be in the intersection of the *type* specified in the description of the corresponding *generic function* and the *type* given in the *signature* of some *method* (including not only those *methods* defined in this specification, but also *implementation-defined* or user-defined *methods* in situations where the definition of such *methods* is permitted). 

**1.4.4.14 The “Name” Section of a Dictionary Entry** 

This section introduces the dictionary entry. It is not explicitly labeled. It appears preceded and followed by a horizontal bar. 

In large print at left, the *defined name* appears; if more than one *defined name* is to be described by the entry, all such *names* are shown separated by commas. 

In somewhat smaller italic print at right is an indication of what kind of dictionary entry this is. Possible values are: 

*Accessor* 

This is an *accessor function*. 

*Class* 

This is a *class*. 





*Condition Type* 

This is a *subtype* of *type* **condition**. 

*Constant Variable* 

This is a *constant variable*. 

*Declaration* 

This is a *declaration identifier* . 

*Function* 

This is a *function*. 

*Local Function* 

This is a *function* that is defined only lexically within the scope of some other *macro form*. 

*Local Macro* 

This is a *macro* that is defined only lexically within the scope of some other *macro form*. 

*Macro* 

This is a *macro*. 

*Restart* 

This is a *restart*. 

*Special Operator* 

This is a *special operator* . 

*Standard Generic Function* 

This is a *standard generic function*. 

*Symbol* 

This is a *symbol* that is specially recognized in some particular situation, such as the syntax of a *macro*. 

*System Class* 

This is like *class*, but it identifies a *class* that is potentially a *built-in class*. (No *class* is actually required to be a *built-in class*.)  



*Type* 

This is an *atomic type specifier* , and depending on information for each particular entry, may subject to form other *type specifiers*. 

*Type Specifier* 

This is a *defined name* that is not an *atomic type specifier* , but that can be used in constructing valid *type specifiers*. 

*Variable* 

This is a *dynamic variable*. 

**1.4.4.15 The “Notes” Section of a Dictionary Entry** 

Information not found elsewhere in this description which pertains to this *operator* . Among other things, this might include cross reference information, code equivalences, stylistic hints, implementation hints, typical uses. This information is not considered part of the standard; any *conforming implementation* or *conforming program* is permitted to ignore the presence of this information. 

**1.4.4.16 The “Pronunciation” Section of a Dictionary Entry** 

This offers a suggested pronunciation for *defined names* so that people not in verbal communication with the original designers can figure out how to pronounce words that are not in normal English usage. This information is advisory only, and is not considered part of the standard. For brevity, it is only provided for entries with names that are specific to Common Lisp and would not be found in *Webster’s Third New International Dictionary the English Language, Unabridged*. 

**1.4.4.17 The “See Also” Section of a Dictionary Entry** 

List of references to other parts of this standard that offer information relevant to this *operator* . This list is not part of the standard. 

**1.4.4.18 The “Side Effects” Section of a Dictionary Entry** 

Anything that is changed as a result of the evaluation of the *form* containing this *operator* . 

**1.4.4.19 The “Supertypes” Section of a Dictionary Entry** 

This appears in the dictionary entry for a *type*, and contains a list of the *standardized types* that must be *supertypes* of this *type*. 

In *implementations* where there is a corresponding *class*, the order of the *classes* in the *class precedence list* is consistent with the order presented in this section. 





**1.4.4.20 The “Syntax” Section of a Dictionary Entry** 

This section describes how to use the *defined name* in code. The “Syntax” description for a *generic function* describes the *lambda list* of the *generic function* itself, while the “Method Signatures” describe the *lambda lists* of the defined *methods*. The “Syntax” description for an *ordinary function*, a *macro*, or a *special operator* describes its *parameters*. 

For example, an *operator* description might say: 

**F** *x y* &optional *z* &key *k* 

This description indicates that the function **F** has two required parameters, *x* and *y*. In addition, there is an optional parameter *z* and a keyword parameter *k*. 

For *macros* and *special operators*, syntax is given in modified BNF notation; see Section 1.4.1.2 (Modified BNF Syntax). For *functions* a *lambda list* is given. In both cases, however, the outermost parentheses are omitted, and default value information is omitted. 

**1.4.4.20.1 Special “Syntax” Notations for Overloaded Operators** 

If two descriptions exist for the same operation but with different numbers of arguments, then the extra arguments are to be treated as optional. For example, this pair of lines: 

**file-position** *stream → position* 

**file-position** *stream position-spec → success-p* 

is operationally equivalent to this line: 

**file-position** *stream* &optional *position-spec → result* 

and differs only in that it provides on opportunity to introduce different names for *parameter* and *values* for each case. The separated (multi-line) notation is used when an *operator* is overloaded in such a way that the *parameters* are used in different ways depending on how many *arguments* are supplied (*e.g.*, for the *function* **/**) or the return values are different in the two cases (*e.g.*, for the 

*function* **file-position**). 

**1.4.4.20.2 Naming Conventions for Rest Parameters** 

Within this specification, if the name of a *rest parameter* is chosen to be a plural noun, use of that name in *parameter* font refers to the *list* to which the *rest parameter* is bound. Use of the singular form of that name in *parameter* font refers to an *element* of that *list*. 

For example, given a syntax description such as: 

**F** &rest *arguments* 

it is appropriate to refer either to the *rest parameter* named *arguments* by name, or to one of its elements by speaking of “an *argument*,” “some *argument*,” “each *argument*” *etc.*  



**1.4.4.20.3 Requiring Non-Null Rest Parameters in the “Syntax” Section** 

In some cases it is useful to refer to all arguments equally as a single aggregation using a *rest parameter* while at the same time requiring at least one argument. A variety of imperative and declarative means are available in *code* for expressing such a restriction, however they generally do not manifest themselves in a *lambda list*. For descriptive purposes within this specification, 

**F** &rest *arguments*&#60;sup&#62;+&#60;/sup&#62; 

means the same as 

**F** &rest *arguments* 

but introduces the additional requirement that there be at least one *argument*. 

**1.4.4.20.4 Return values in the “Syntax” Section** 

An evaluation arrow “*→*” precedes a list of *values* to be returned. For example: 

**F** *a b c → x* 

indicates that F is an operator that has three *required parameters* (*i.e.*, *a*, *b*, and *c*) and that returns one *value* (*i.e.*, *x*). If more than one *value* is returned by an operator, the *names* of the *values* are separated by commas, as in: 

**F** *a b c → x, y, z* 

**1.4.4.20.4.1 No Arguments or Values in the “Syntax” Section** 

If no *arguments* are permitted, or no *values* are returned, a special notation is used to make this more visually apparent. For example, 

**F** *hno argumentsi → hno valuesi* 

indicates that F is an operator that accepts no *arguments* and returns no *values*. 

**1.4.4.20.4.2 Unconditional Transfer of Control in the “Syntax” Section** 

Some *operators* perform an unconditional transfer of control, and so never have any return values. Such *operators* are notated using a notation such as the following: 

**F** *a b c →* 

**1.4.4.21 The “Valid Context” Section of a Dictionary Entry** 

This information is used by dictionary entries such as “Declarations” in order to restrict the context in which the declaration may appear. 

A given “Declaration” might appear in a *declaration* (*i.e.*, a **declare** *expression*), a *proclamation* (*i.e.*, a **declaim** or **proclaim** *form*), or both. 





**1.4.4.22 The “Value Type” Section of a Dictionary Entry** 

This information describes any *type* restrictions on a *dynamic variable*. 

Except as explicitly specified otherwise, the consequences are undefined if this type restriction is violated.  



**1.5 Conformance** 

This standard presents the syntax and semantics to be implemented by a *conforming implementation* (and its accompanying documentation). In addition, it imposes requirements on *conforming programs*. 

**1.5.1 Conforming Implementations** 

A *conforming implementation* shall adhere to the requirements outlined in this section. 

**1.5.1.1 Required Language Features** 

A *conforming implementation* shall accept all features (including deprecated features) of the language specified in this standard, with the meanings defined in this standard. 

A *conforming implementation* shall not require the inclusion of substitute or additional language elements in code in order to accomplish a feature of the language that is specified in this standard. 

**1.5.1.2 Documentation of Implementation-Dependent Features** 

A *conforming implementation* shall be accompanied by a document that provides a definition of all *implementation-defined* aspects of the language defined by this specification. 

In addition, a *conforming implementation* is encouraged (but not required) to document items in this standard that are identified as *implementation-dependent*, although in some cases such documentation might simply identify the item as “undefined.” 

**1.5.1.3 Documentation of Extensions** 

A *conforming implementation* shall be accompanied by a document that separately describes any features accepted by the *implementation* that are not specified in this standard, but that do not cause any ambiguity or contradiction when added to the language standard. Such extensions shall be described as being “extensions to Common Lisp as specified by ANSI &#10216;standard number &#10217;.” 

**1.5.1.4 Treatment of Exceptional Situations** 

A *conforming implementation* shall treat exceptional situations in a manner consistent with this specification. 

**1.5.1.4.1 Resolution of Apparent Conflicts in Exceptional Situations** 

If more than one passage in this specification appears to apply to the same situation but in conflicting ways, the passage that appears to describe the situation in the most specific way (not necessarily the passage that provides the most constrained kind of error detection) takes precedence. 





**1.5.1.4.1.1 Examples of Resolution of Apparent Conflicts in Exceptional Situations** 

Suppose that function foo is a member of a set *S* of *functions* that operate on numbers. Suppose that one passage states that an error must be signaled if any *function* in *S* is ever given an argument of 17. Suppose that an apparently conflicting passage states that the consequences are undefined if foo receives an argument of 17. Then the second passage (the one specifically about foo) would dominate because the description of the situational context is the most specific, and it would not be required that foo signal an error on an argument of 17 even though other functions in the set *S* would be required to do so. 

**1.5.1.5 Conformance Statement** 

A *conforming implementation* shall produce a conformance statement as a consequence of using the implementation, or that statement shall be included in the accompanying documentation. If the implementation conforms in all respects with this standard, the conformance statement shall be 

“&#10216;Implementation&#10217; conforms with the requirements of ANSI &#10216;standard number &#10217;” 

If the *implementation* conforms with some but not all of the requirements of this standard, then the conformance statement shall be 

“&#10216;Implementation&#10217; conforms with the requirements of ANSI &#10216;standard number &#10217; with the following exceptions: &#10216;reference to or complete list of the requirements of the standard with which the implementation does not conform&#10217;.” 

**1.5.2 Conforming Programs** 

Code conforming with the requirements of this standard shall adhere to the following: 

1\. *Conforming code* shall use only those features of the language syntax and semantics that are either specified in this standard or defined using the extension mechanisms specified in the standard. 

2\. *Conforming code* may use *implementation-dependent* features and values, but shall not rely upon any particular interpretation of these features and values other than those that are discovered by the execution of *code*. 

3\. *Conforming code* shall not depend on the consequences of undefined or unspecified situations. 

4\. *Conforming code* does not use any constructions that are prohibited by the standard. 5. *Conforming code* does not depend on extensions included in an implementation.  



**1.5.2.1 Use of Implementation-Defined Language Features** 

Note that *conforming code* may rely on particular *implementation-defined* values or features. Also note that the requirements for *conforming code* and *conforming implementations* do not require that the results produced by conforming code always be the same when processed by a *conforming implementation*. The results may be the same, or they may differ. 

Conforming code may run in all conforming implementations, but might have allowable *implementation-defined* behavior that makes it non-portable code. For example, the following are examples of *forms* that are conforming, but that might return different *values* in different implementations: 

(evenp most-positive-fixnum) *→ implementation-dependent* 

(random) *→ implementation-dependent* 

(&#62; lambda-parameters-limit 93) *→ implementation-dependent* 

(char-name #\A) *→ implementation-dependent* 

**1.5.2.1.1 Use of Read-Time Conditionals** 

Use of #+ and #- does not automatically disqualify a program from being conforming. A program which uses #+ and #- is considered conforming if there is no set of *features* in which the program would not be conforming. Of course, *conforming programs* are not necessarily working programs. The following program is conforming: 

(defun foo () 

#+ACME (acme:initialize-something) 

(print ’hello-there)) 

However, this program might or might not work, depending on whether the presence of the feature ACME really implies that a function named acme:initialize-something is present in the environment. In effect, using #+ or #- in a *conforming program* means that the variable **\*features\*** becomes just one more piece of input data to that program. Like any other data coming into a program, the 

programmer is responsible for assuring that the program does not make unwarranted assumptions on the basis of input data. 

**1.5.2.2 Character Set for Portable Code** 

*Portable code* is written using only *standard characters*. 





**1.6 Language Extensions** 

A language extension is any documented *implementation-defined* behavior of a *defined name* in this standard that varies from the behavior described in this standard, or a documented consequence of a situation that the standard specifies as undefined, unspecified, or extendable by the implementation. For example, if this standard says that “the results are unspecified,” an extension would be to specify the results. 

If the correct behavior of a program depends on the results provided by an extension, only implementations with the same extension will execute the program correctly. Note that such a program might be non-conforming. Also, if this standard says that “an implementation may be extended,” a conforming, but possibly non-portable, program can be written using an extension. 

An implementation can have extensions, provided they do not alter the behavior of conforming code and provided they are not explicitly prohibited by this standard. 

The term “extension” refers only to extensions available upon startup. An implementation is free to allow or prohibit redefinition of an extension. 

The following list contains specific guidance to implementations concerning certain types of extensions. 

**Extra return values** 

An implementation must return exactly the number of return values specified by this standard unless the standard specifically indicates otherwise. 

**Unsolicited messages** 

No output can be produced by a function other than that specified in the standard or due to the signaling of *conditions* detected by the function. 

Unsolicited output, such as garbage collection notifications and autoload heralds, should not go directly to the *stream* that is the value of a *stream* variable defined in this standard, but can go indirectly to *terminal I/O* by using a *synonym stream* to **\*terminal-io\***. 

Progress reports from such functions as **load** and **compile** are considered solicited, and are not covered by this prohibition. 

**Implementation of macros and special forms** 

*Macros* and *special operators* defined in this standard must not be *functions*.  



**1.7 Language Subsets** 

The language described in this standard contains no subsets, though subsets are not forbidden. 

For a language to be considered a subset, it must have the property that any valid *program* in that language has equivalent semantics and will run directly (with no extralingual pre-processing, and no special compatibility packages) in any *conforming implementation* of the full language. 

A language that conforms to this requirement shall be described as being a “subset of Common Lisp as specified by ANSI &#10216;standard number &#10217;.” 





**1.8 Deprecated Language Features** 

Deprecated language features are not expected to appear in future Common Lisp standards, but are required to be implemented for conformance with this standard; see Section 1.5.1.1 (Required Language Features). 

*Conforming programs* can use deprecated features; however, it is considered good programming style to avoid them. It is permissible for the compiler to produce *style warnings* about the use of such features at compile time, but there should be no such warnings at program execution time. 

**1.8.1 Deprecated Functions** 

The *functions* in Figure 1–2 are deprecated. 

|&#60;p&#62;**assoc-if-not nsubst-if-not require** &#60;/p&#62;&#60;p&#62;**count-if-not nsubstitute-if-not set** &#60;/p&#62;&#60;p&#62;**delete-if-not position-if-not subst-if-not** &#60;/p&#62;&#60;p&#62;**find-if-not provide substitute-if-not gentemp rassoc-if-not** &#60;/p&#62;&#60;p&#62;**member-if-not remove-if-not**&#60;/p&#62;|
| :- |


**Figure 1–2. Deprecated Functions** 

**1.8.2 Deprecated Argument Conventions** 

The ability to pass a numeric *argument* to **gensym** has been deprecated. 

The :test-not *argument* to the *functions* in Figure 1–3 are deprecated. 

|&#60;p&#62;**adjoin nset-difference search** &#60;/p&#62;&#60;p&#62;**assoc nset-exclusive-or set-difference count nsublis set-exclusive-or delete nsubst sublis** &#60;/p&#62;&#60;p&#62;**delete-duplicates nsubstitute subsetp** &#60;/p&#62;&#60;p&#62;**find nunion subst** &#60;/p&#62;&#60;p&#62;**intersection position substitute** &#60;/p&#62;&#60;p&#62;**member rassoc tree-equal** &#60;/p&#62;&#60;p&#62;**mismatch remove union** &#60;/p&#62;&#60;p&#62;**nintersection remove-duplicates**&#60;/p&#62;|
| :- |


**Figure 1–3. Functions with Deprecated :TEST-NOT Arguments** 

The use of the situation names **compile**, **load**, and **eval** in **eval-when** is deprecated. Introduction **1–33**





**1.8.3 Deprecated Variables** 

The *variable* **\*modules\*** is deprecated. 

**1.8.4 Deprecated Reader Syntax** 

The #S *reader macro* forces keyword names into the KEYWORD *package*; see Section 2.4.8.13 (Sharpsign S). This feature is deprecated; in the future, keyword names will be taken in the package they are read in, so *symbols* that are actually in the KEYWORD *package* should be used if that is what is desired. 





**1.9 Symbols in the COMMON-LISP Package** 

The figures on the next twelve pages contain a complete enumeration of the 978 *external symbols* in the COMMON-LISP *package*. 

|&#60;p&#62;**&allow-other-keys \*print-miser-width\*** &#60;/p&#62;&#60;p&#62;**&aux \*print-pprint-dispatch\*** &#60;/p&#62;&#60;p&#62;**&body \*print-pretty\*** &#60;/p&#62;&#60;p&#62;**&environment \*print-radix\*** &#60;/p&#62;&#60;p&#62;**&key \*print-readably\*** &#60;/p&#62;&#60;p&#62;**&optional \*print-right-margin\*** &#60;/p&#62;&#60;p&#62;**&rest \*query-io\*** &#60;/p&#62;&#60;p&#62;**&whole \*random-state\*** &#60;/p&#62;&#60;p&#62;**\* \*read-base\*** &#60;/p&#62;&#60;p&#62;**\*\* \*read-default-float-format\* \*\*\* \*read-eval\*** &#60;/p&#62;&#60;p&#62;**\*break-on-signals\* \*read-suppress\*** &#60;/p&#62;&#60;p&#62;**\*compile-file-pathname\* \*readtable\*** &#60;/p&#62;&#60;p&#62;**\*compile-file-truename\* \*standard-input\*** &#60;/p&#62;&#60;p&#62;**\*compile-print\* \*standard-output\*** &#60;/p&#62;&#60;p&#62;**\*compile-verbose\* \*terminal-io\*** &#60;/p&#62;&#60;p&#62;**\*debug-io\* \*trace-output\*** &#60;/p&#62;&#60;p&#62;**\*debugger-hook\* +** &#60;/p&#62;&#60;p&#62;**\*default-pathname-defaults\* ++** &#60;/p&#62;&#60;p&#62;**\*error-output\* +++** &#60;/p&#62;&#60;p&#62;**\*features\* -** &#60;/p&#62;&#60;p&#62;**\*gensym-counter\* /** &#60;/p&#62;&#60;p&#62;**\*load-pathname\* //** &#60;/p&#62;&#60;p&#62;**\*load-print\* ///** &#60;/p&#62;&#60;p&#62;**\*load-truename\* /=** &#60;/p&#62;&#60;p&#62;**\*load-verbose\* 1+** &#60;/p&#62;&#60;p&#62;**\*macroexpand-hook\* 1-** &#60;/p&#62;&#60;p&#62;**\*modules\*** &#60; &#60;/p&#62;&#60;p&#62;**\*package\*** &#60;**=** &#60;/p&#62;&#60;p&#62;**\*print-array\* =** &#60;/p&#62;&#60;p&#62;**\*print-base\*** &#62; &#60;/p&#62;&#60;p&#62;**\*print-case\*** &#62;**=** &#60;/p&#62;&#60;p&#62;**\*print-circle\* abort** &#60;/p&#62;&#60;p&#62;**\*print-escape\* abs** &#60;/p&#62;&#60;p&#62;**\*print-gensym\* acons** &#60;/p&#62;&#60;p&#62;**\*print-length\* acos** &#60;/p&#62;&#60;p&#62;**\*print-level\* acosh** &#60;/p&#62;&#60;p&#62;**\*print-lines\* add-method**&#60;/p&#62;|
| :- |


**Figure 1–4. Symbols in the COMMON-LISP package (part one of twelve).**  



|&#60;p&#62;**adjoin atom boundp** &#60;/p&#62;&#60;p&#62;**adjust-array base-char break** &#60;/p&#62;&#60;p&#62;**adjustable-array-p base-string broadcast-stream allocate-instance bignum broadcast-stream-streams alpha-char-p bit built-in-class** &#60;/p&#62;&#60;p&#62;**alphanumericp bit-and butlast** &#60;/p&#62;&#60;p&#62;**and bit-andc1 byte** &#60;/p&#62;&#60;p&#62;**append bit-andc2 byte-position** &#60;/p&#62;&#60;p&#62;**apply bit-eqv byte-size** &#60;/p&#62;&#60;p&#62;**apropos bit-ior caaaar** &#60;/p&#62;&#60;p&#62;**apropos-list bit-nand caaadr** &#60;/p&#62;&#60;p&#62;**aref bit-nor caaar** &#60;/p&#62;&#60;p&#62;**arithmetic-error bit-not caadar** &#60;/p&#62;&#60;p&#62;**arithmetic-error-operands bit-orc1 caaddr** &#60;/p&#62;&#60;p&#62;**arithmetic-error-operation bit-orc2 caadr** &#60;/p&#62;&#60;p&#62;**array bit-vector caar** &#60;/p&#62;&#60;p&#62;**array-dimension bit-vector-p cadaar** &#60;/p&#62;&#60;p&#62;**array-dimension-limit bit-xor cadadr** &#60;/p&#62;&#60;p&#62;**array-dimensions block cadar** &#60;/p&#62;&#60;p&#62;**array-displacement boole caddar** &#60;/p&#62;&#60;p&#62;**array-element-type boole-1 cadddr** &#60;/p&#62;&#60;p&#62;**array-has-fill-pointer-p boole-2 caddr** &#60;/p&#62;&#60;p&#62;**array-in-bounds-p boole-and cadr** &#60;/p&#62;&#60;p&#62;**array-rank boole-andc1 call-arguments-limit array-rank-limit boole-andc2 call-method** &#60;/p&#62;&#60;p&#62;**array-row-major-index boole-c1 call-next-method array-total-size boole-c2 car** &#60;/p&#62;&#60;p&#62;**array-total-size-limit boole-clr case** &#60;/p&#62;&#60;p&#62;**arrayp boole-eqv catch** &#60;/p&#62;&#60;p&#62;**ash boole-ior ccase** &#60;/p&#62;&#60;p&#62;**asin boole-nand cdaaar** &#60;/p&#62;&#60;p&#62;**asinh boole-nor cdaadr** &#60;/p&#62;&#60;p&#62;**assert boole-orc1 cdaar** &#60;/p&#62;&#60;p&#62;**assoc boole-orc2 cdadar** &#60;/p&#62;&#60;p&#62;**assoc-if boole-set cdaddr** &#60;/p&#62;&#60;p&#62;**assoc-if-not boole-xor cdadr** &#60;/p&#62;&#60;p&#62;**atan boolean cdar** &#60;/p&#62;&#60;p&#62;**atanh both-case-p cddaar**&#60;/p&#62;|
| :- |


**Figure 1–5. Symbols in the COMMON-LISP package (part two of twelve).** 





|&#60;p&#62;**cddadr clear-input copy-tree** &#60;/p&#62;&#60;p&#62;**cddar clear-output cos** &#60;/p&#62;&#60;p&#62;**cdddar close cosh** &#60;/p&#62;&#60;p&#62;**cddddr clrhash count** &#60;/p&#62;&#60;p&#62;**cdddr code-char count-if** &#60;/p&#62;&#60;p&#62;**cddr coerce count-if-not** &#60;/p&#62;&#60;p&#62;**cdr compilation-speed ctypecase** &#60;/p&#62;&#60;p&#62;**ceiling compile debug** &#60;/p&#62;&#60;p&#62;**cell-error compile-file decf** &#60;/p&#62;&#60;p&#62;**cell-error-name compile-file-pathname declaim** &#60;/p&#62;&#60;p&#62;**cerror compiled-function declaration** &#60;/p&#62;&#60;p&#62;**change-class compiled-function-p declare** &#60;/p&#62;&#60;p&#62;**char compiler-macro decode-float** &#60;/p&#62;&#60;p&#62;**char-code compiler-macro-function decode-universal-time char-code-limit complement defclass** &#60;/p&#62;&#60;p&#62;**char-downcase complex defconstant** &#60;/p&#62;&#60;p&#62;**char-equal complexp defgeneric** &#60;/p&#62;&#60;p&#62;**char-greaterp compute-applicable-methods define-compiler-macro char-int compute-restarts define-condition char-lessp concatenate define-method-combination char-name concatenated-stream define-modify-macro char-not-equal concatenated-stream-streams define-setf-expander char-not-greaterp cond define-symbol-macro char-not-lessp condition defmacro** &#60;/p&#62;&#60;p&#62;**char-upcase conjugate defmethod** &#60;/p&#62;&#60;p&#62;**char/= cons defpackage** &#60;/p&#62;&#60;p&#62;**char**&#60; **consp defparameter char**&#60;**= constantly defsetf** &#60;/p&#62;&#60;p&#62;**char= constantp defstruct** &#60;/p&#62;&#60;p&#62;**char**&#62; **continue deftype** &#60;/p&#62;&#60;p&#62;**char**&#62;**= control-error defun** &#60;/p&#62;&#60;p&#62;**character copy-alist defvar** &#60;/p&#62;&#60;p&#62;**characterp copy-list delete** &#60;/p&#62;&#60;p&#62;**check-type copy-pprint-dispatch delete-duplicates cis copy-readtable delete-file** &#60;/p&#62;&#60;p&#62;**class copy-seq delete-if** &#60;/p&#62;&#60;p&#62;**class-name copy-structure delete-if-not** &#60;/p&#62;&#60;p&#62;**class-of copy-symbol delete-package**&#60;/p&#62;|
| :- |


**Figure 1–6. Symbols in the COMMON-LISP package (part three of twelve).**  



|&#60;p&#62;**denominator eq** &#60;/p&#62;&#60;p&#62;**deposit-field eql** &#60;/p&#62;&#60;p&#62;**describe equal** &#60;/p&#62;&#60;p&#62;**describe-object equalp** &#60;/p&#62;&#60;p&#62;**destructuring-bind error** &#60;/p&#62;&#60;p&#62;**digit-char etypecase** &#60;/p&#62;&#60;p&#62;**digit-char-p eval** &#60;/p&#62;&#60;p&#62;**directory eval-when** &#60;/p&#62;&#60;p&#62;**directory-namestring evenp** &#60;/p&#62;&#60;p&#62;**disassemble every** &#60;/p&#62;&#60;p&#62;**division-by-zero exp** &#60;/p&#62;&#60;p&#62;**do export** &#60;/p&#62;&#60;p&#62;**do\* expt** &#60;/p&#62;&#60;p&#62;**do-all-symbols extended-char** &#60;/p&#62;&#60;p&#62;**do-external-symbols fboundp** &#60;/p&#62;&#60;p&#62;**do-symbols fceiling** &#60;/p&#62;&#60;p&#62;**documentation fdefinition** &#60;/p&#62;&#60;p&#62;**dolist ffloor** &#60;/p&#62;&#60;p&#62;**dotimes fifth** &#60;/p&#62;&#60;p&#62;**double-float file-author** &#60;/p&#62;&#60;p&#62;**double-float-epsilon file-error** &#60;/p&#62;&#60;p&#62;**double-float-negative-epsilon file-error-pathname** &#60;/p&#62;&#60;p&#62;**dpb file-length** &#60;/p&#62;&#60;p&#62;**dribble file-namestring** &#60;/p&#62;&#60;p&#62;**dynamic-extent file-position** &#60;/p&#62;&#60;p&#62;**ecase file-stream** &#60;/p&#62;&#60;p&#62;**echo-stream file-string-length** &#60;/p&#62;&#60;p&#62;**echo-stream-input-stream file-write-date** &#60;/p&#62;&#60;p&#62;**echo-stream-output-stream fill** &#60;/p&#62;&#60;p&#62;**ed fill-pointer** &#60;/p&#62;&#60;p&#62;**eighth find** &#60;/p&#62;&#60;p&#62;**elt find-all-symbols** &#60;/p&#62;&#60;p&#62;**encode-universal-time find-class** &#60;/p&#62;&#60;p&#62;**end-of-file find-if** &#60;/p&#62;&#60;p&#62;**endp find-if-not** &#60;/p&#62;&#60;p&#62;**enough-namestring find-method** &#60;/p&#62;&#60;p&#62;**ensure-directories-exist find-package** &#60;/p&#62;&#60;p&#62;**ensure-generic-function find-restart**&#60;/p&#62;|
| :- |


**Figure 1–7. Symbols in the COMMON-LISP package (part four of twelve).** 





|&#60;p&#62;**find-symbol get-internal-run-time** &#60;/p&#62;&#60;p&#62;**finish-output get-macro-character** &#60;/p&#62;&#60;p&#62;**first get-output-stream-string** &#60;/p&#62;&#60;p&#62;**fixnum get-properties** &#60;/p&#62;&#60;p&#62;**flet get-setf-expansion** &#60;/p&#62;&#60;p&#62;**float get-universal-time** &#60;/p&#62;&#60;p&#62;**float-digits getf** &#60;/p&#62;&#60;p&#62;**float-precision gethash** &#60;/p&#62;&#60;p&#62;**float-radix go** &#60;/p&#62;&#60;p&#62;**float-sign graphic-char-p** &#60;/p&#62;&#60;p&#62;**floating-point-inexact handler-bind** &#60;/p&#62;&#60;p&#62;**floating-point-invalid-operation handler-case** &#60;/p&#62;&#60;p&#62;**floating-point-overflow hash-table** &#60;/p&#62;&#60;p&#62;**floating-point-underflow hash-table-count** &#60;/p&#62;&#60;p&#62;**floatp hash-table-p** &#60;/p&#62;&#60;p&#62;**floor hash-table-rehash-size** &#60;/p&#62;&#60;p&#62;**fmakunbound hash-table-rehash-threshold force-output hash-table-size** &#60;/p&#62;&#60;p&#62;**format hash-table-test** &#60;/p&#62;&#60;p&#62;**formatter host-namestring** &#60;/p&#62;&#60;p&#62;**fourth identity** &#60;/p&#62;&#60;p&#62;**fresh-line if** &#60;/p&#62;&#60;p&#62;**fround ignorable** &#60;/p&#62;&#60;p&#62;**ftruncate ignore** &#60;/p&#62;&#60;p&#62;**ftype ignore-errors** &#60;/p&#62;&#60;p&#62;**funcall imagpart** &#60;/p&#62;&#60;p&#62;**function import** &#60;/p&#62;&#60;p&#62;**function-keywords in-package** &#60;/p&#62;&#60;p&#62;**function-lambda-expression incf** &#60;/p&#62;&#60;p&#62;**functionp initialize-instance** &#60;/p&#62;&#60;p&#62;**gcd inline** &#60;/p&#62;&#60;p&#62;**generic-function input-stream-p** &#60;/p&#62;&#60;p&#62;**gensym inspect** &#60;/p&#62;&#60;p&#62;**gentemp integer** &#60;/p&#62;&#60;p&#62;**get integer-decode-float** &#60;/p&#62;&#60;p&#62;**get-decoded-time integer-length** &#60;/p&#62;&#60;p&#62;**get-dispatch-macro-character integerp** &#60;/p&#62;&#60;p&#62;**get-internal-real-time interactive-stream-p**&#60;/p&#62;|
| :- |


**Figure 1–8. Symbols in the COMMON-LISP package (part five of twelve).**  



|&#60;p&#62;**intern lisp-implementation-type internal-time-units-per-second lisp-implementation-version intersection list** &#60;/p&#62;&#60;p&#62;**invalid-method-error list\*** &#60;/p&#62;&#60;p&#62;**invoke-debugger list-all-packages** &#60;/p&#62;&#60;p&#62;**invoke-restart list-length** &#60;/p&#62;&#60;p&#62;**invoke-restart-interactively listen** &#60;/p&#62;&#60;p&#62;**isqrt listp** &#60;/p&#62;&#60;p&#62;**keyword load** &#60;/p&#62;&#60;p&#62;**keywordp load-logical-pathname-translations labels load-time-value** &#60;/p&#62;&#60;p&#62;**lambda locally** &#60;/p&#62;&#60;p&#62;**lambda-list-keywords log** &#60;/p&#62;&#60;p&#62;**lambda-parameters-limit logand** &#60;/p&#62;&#60;p&#62;**last logandc1** &#60;/p&#62;&#60;p&#62;**lcm logandc2** &#60;/p&#62;&#60;p&#62;**ldb logbitp** &#60;/p&#62;&#60;p&#62;**ldb-test logcount** &#60;/p&#62;&#60;p&#62;**ldiff logeqv** &#60;/p&#62;&#60;p&#62;**least-negative-double-float logical-pathname** &#60;/p&#62;&#60;p&#62;**least-negative-long-float logical-pathname-translations least-negative-normalized-double-float logior** &#60;/p&#62;&#60;p&#62;**least-negative-normalized-long-float lognand** &#60;/p&#62;&#60;p&#62;**least-negative-normalized-short-float lognor** &#60;/p&#62;&#60;p&#62;**least-negative-normalized-single-float lognot** &#60;/p&#62;&#60;p&#62;**least-negative-short-float logorc1** &#60;/p&#62;&#60;p&#62;**least-negative-single-float logorc2** &#60;/p&#62;&#60;p&#62;**least-positive-double-float logtest** &#60;/p&#62;&#60;p&#62;**least-positive-long-float logxor** &#60;/p&#62;&#60;p&#62;**least-positive-normalized-double-float long-float** &#60;/p&#62;&#60;p&#62;**least-positive-normalized-long-float long-float-epsilon** &#60;/p&#62;&#60;p&#62;**least-positive-normalized-short-float long-float-negative-epsilon least-positive-normalized-single-float long-site-name** &#60;/p&#62;&#60;p&#62;**least-positive-short-float loop** &#60;/p&#62;&#60;p&#62;**least-positive-single-float loop-finish** &#60;/p&#62;&#60;p&#62;**length lower-case-p** &#60;/p&#62;&#60;p&#62;**let machine-instance** &#60;/p&#62;&#60;p&#62;**let\* machine-type**&#60;/p&#62;|
| :- |


**Figure 1–9. Symbols in the COMMON-LISP package (part six of twelve).** 





|&#60;p&#62;**machine-version mask-field** &#60;/p&#62;&#60;p&#62;**macro-function max** &#60;/p&#62;&#60;p&#62;**macroexpand member** &#60;/p&#62;&#60;p&#62;**macroexpand-1 member-if** &#60;/p&#62;&#60;p&#62;**macrolet member-if-not** &#60;/p&#62;&#60;p&#62;**make-array merge** &#60;/p&#62;&#60;p&#62;**make-broadcast-stream merge-pathnames** &#60;/p&#62;&#60;p&#62;**make-concatenated-stream method** &#60;/p&#62;&#60;p&#62;**make-condition method-combination** &#60;/p&#62;&#60;p&#62;**make-dispatch-macro-character method-combination-error make-echo-stream method-qualifiers** &#60;/p&#62;&#60;p&#62;**make-hash-table min** &#60;/p&#62;&#60;p&#62;**make-instance minusp** &#60;/p&#62;&#60;p&#62;**make-instances-obsolete mismatch** &#60;/p&#62;&#60;p&#62;**make-list mod** &#60;/p&#62;&#60;p&#62;**make-load-form most-negative-double-float make-load-form-saving-slots most-negative-fixnum** &#60;/p&#62;&#60;p&#62;**make-method most-negative-long-float** &#60;/p&#62;&#60;p&#62;**make-package most-negative-short-float make-pathname most-negative-single-float make-random-state most-positive-double-float make-sequence most-positive-fixnum** &#60;/p&#62;&#60;p&#62;**make-string most-positive-long-float** &#60;/p&#62;&#60;p&#62;**make-string-input-stream most-positive-short-float** &#60;/p&#62;&#60;p&#62;**make-string-output-stream most-positive-single-float make-symbol muffle-warning** &#60;/p&#62;&#60;p&#62;**make-synonym-stream multiple-value-bind** &#60;/p&#62;&#60;p&#62;**make-two-way-stream multiple-value-call** &#60;/p&#62;&#60;p&#62;**makunbound multiple-value-list** &#60;/p&#62;&#60;p&#62;**map multiple-value-prog1** &#60;/p&#62;&#60;p&#62;**map-into multiple-value-setq** &#60;/p&#62;&#60;p&#62;**mapc multiple-values-limit** &#60;/p&#62;&#60;p&#62;**mapcan name-char** &#60;/p&#62;&#60;p&#62;**mapcar namestring** &#60;/p&#62;&#60;p&#62;**mapcon nbutlast** &#60;/p&#62;&#60;p&#62;**maphash nconc** &#60;/p&#62;&#60;p&#62;**mapl next-method-p** &#60;/p&#62;&#60;p&#62;**maplist nil**&#60;/p&#62;|
| :- |


**Figure 1–10. Symbols in the COMMON-LISP package (part seven of twelve).** Introduction **1–41**





|&#60;p&#62;**nintersection package-error** &#60;/p&#62;&#60;p&#62;**ninth package-error-package** &#60;/p&#62;&#60;p&#62;**no-applicable-method package-name** &#60;/p&#62;&#60;p&#62;**no-next-method package-nicknames** &#60;/p&#62;&#60;p&#62;**not package-shadowing-symbols** &#60;/p&#62;&#60;p&#62;**notany package-use-list** &#60;/p&#62;&#60;p&#62;**notevery package-used-by-list** &#60;/p&#62;&#60;p&#62;**notinline packagep** &#60;/p&#62;&#60;p&#62;**nreconc pairlis** &#60;/p&#62;&#60;p&#62;**nreverse parse-error** &#60;/p&#62;&#60;p&#62;**nset-difference parse-integer** &#60;/p&#62;&#60;p&#62;**nset-exclusive-or parse-namestring** &#60;/p&#62;&#60;p&#62;**nstring-capitalize pathname** &#60;/p&#62;&#60;p&#62;**nstring-downcase pathname-device** &#60;/p&#62;&#60;p&#62;**nstring-upcase pathname-directory** &#60;/p&#62;&#60;p&#62;**nsublis pathname-host** &#60;/p&#62;&#60;p&#62;**nsubst pathname-match-p** &#60;/p&#62;&#60;p&#62;**nsubst-if pathname-name** &#60;/p&#62;&#60;p&#62;**nsubst-if-not pathname-type** &#60;/p&#62;&#60;p&#62;**nsubstitute pathname-version** &#60;/p&#62;&#60;p&#62;**nsubstitute-if pathnamep** &#60;/p&#62;&#60;p&#62;**nsubstitute-if-not peek-char** &#60;/p&#62;&#60;p&#62;**nth phase** &#60;/p&#62;&#60;p&#62;**nth-value pi** &#60;/p&#62;&#60;p&#62;**nthcdr plusp** &#60;/p&#62;&#60;p&#62;**null pop** &#60;/p&#62;&#60;p&#62;**number position** &#60;/p&#62;&#60;p&#62;**numberp position-if** &#60;/p&#62;&#60;p&#62;**numerator position-if-not** &#60;/p&#62;&#60;p&#62;**nunion pprint** &#60;/p&#62;&#60;p&#62;**oddp pprint-dispatch** &#60;/p&#62;&#60;p&#62;**open pprint-exit-if-list-exhausted** &#60;/p&#62;&#60;p&#62;**open-stream-p pprint-fill** &#60;/p&#62;&#60;p&#62;**optimize pprint-indent** &#60;/p&#62;&#60;p&#62;**or pprint-linear** &#60;/p&#62;&#60;p&#62;**otherwise pprint-logical-block** &#60;/p&#62;&#60;p&#62;**output-stream-p pprint-newline** &#60;/p&#62;&#60;p&#62;**package pprint-pop**&#60;/p&#62;|
| :- |


**Figure 1–11. Symbols in the COMMON-LISP package (part eight of twelve).** 





|&#60;p&#62;**pprint-tab read-char** &#60;/p&#62;&#60;p&#62;**pprint-tabular read-char-no-hang** &#60;/p&#62;&#60;p&#62;**prin1 read-delimited-list** &#60;/p&#62;&#60;p&#62;**prin1-to-string read-from-string** &#60;/p&#62;&#60;p&#62;**princ read-line** &#60;/p&#62;&#60;p&#62;**princ-to-string read-preserving-whitespace** &#60;/p&#62;&#60;p&#62;**print read-sequence** &#60;/p&#62;&#60;p&#62;**print-not-readable reader-error** &#60;/p&#62;&#60;p&#62;**print-not-readable-object readtable** &#60;/p&#62;&#60;p&#62;**print-object readtable-case** &#60;/p&#62;&#60;p&#62;**print-unreadable-object readtablep** &#60;/p&#62;&#60;p&#62;**probe-file real** &#60;/p&#62;&#60;p&#62;**proclaim realp** &#60;/p&#62;&#60;p&#62;**prog realpart** &#60;/p&#62;&#60;p&#62;**prog\* reduce** &#60;/p&#62;&#60;p&#62;**prog1 reinitialize-instance** &#60;/p&#62;&#60;p&#62;**prog2 rem** &#60;/p&#62;&#60;p&#62;**progn remf** &#60;/p&#62;&#60;p&#62;**program-error remhash** &#60;/p&#62;&#60;p&#62;**progv remove** &#60;/p&#62;&#60;p&#62;**provide remove-duplicates** &#60;/p&#62;&#60;p&#62;**psetf remove-if** &#60;/p&#62;&#60;p&#62;**psetq remove-if-not** &#60;/p&#62;&#60;p&#62;**push remove-method** &#60;/p&#62;&#60;p&#62;**pushnew remprop** &#60;/p&#62;&#60;p&#62;**quote rename-file** &#60;/p&#62;&#60;p&#62;**random rename-package** &#60;/p&#62;&#60;p&#62;**random-state replace** &#60;/p&#62;&#60;p&#62;**random-state-p require** &#60;/p&#62;&#60;p&#62;**rassoc rest** &#60;/p&#62;&#60;p&#62;**rassoc-if restart** &#60;/p&#62;&#60;p&#62;**rassoc-if-not restart-bind** &#60;/p&#62;&#60;p&#62;**ratio restart-case** &#60;/p&#62;&#60;p&#62;**rational restart-name** &#60;/p&#62;&#60;p&#62;**rationalize return** &#60;/p&#62;&#60;p&#62;**rationalp return-from** &#60;/p&#62;&#60;p&#62;**read revappend** &#60;/p&#62;&#60;p&#62;**read-byte reverse**&#60;/p&#62;|
| :- |


**Figure 1–12. Symbols in the COMMON-LISP package (part nine of twelve).**  



|&#60;p&#62;**room simple-bit-vector** &#60;/p&#62;&#60;p&#62;**rotatef simple-bit-vector-p** &#60;/p&#62;&#60;p&#62;**round simple-condition** &#60;/p&#62;&#60;p&#62;**row-major-aref simple-condition-format-arguments rplaca simple-condition-format-control rplacd simple-error** &#60;/p&#62;&#60;p&#62;**safety simple-string** &#60;/p&#62;&#60;p&#62;**satisfies simple-string-p** &#60;/p&#62;&#60;p&#62;**sbit simple-type-error** &#60;/p&#62;&#60;p&#62;**scale-float simple-vector** &#60;/p&#62;&#60;p&#62;**schar simple-vector-p** &#60;/p&#62;&#60;p&#62;**search simple-warning** &#60;/p&#62;&#60;p&#62;**second sin** &#60;/p&#62;&#60;p&#62;**sequence single-float** &#60;/p&#62;&#60;p&#62;**serious-condition single-float-epsilon** &#60;/p&#62;&#60;p&#62;**set single-float-negative-epsilon** &#60;/p&#62;&#60;p&#62;**set-difference sinh** &#60;/p&#62;&#60;p&#62;**set-dispatch-macro-character sixth** &#60;/p&#62;&#60;p&#62;**set-exclusive-or sleep** &#60;/p&#62;&#60;p&#62;**set-macro-character slot-boundp** &#60;/p&#62;&#60;p&#62;**set-pprint-dispatch slot-exists-p** &#60;/p&#62;&#60;p&#62;**set-syntax-from-char slot-makunbound** &#60;/p&#62;&#60;p&#62;**setf slot-missing** &#60;/p&#62;&#60;p&#62;**setq slot-unbound** &#60;/p&#62;&#60;p&#62;**seventh slot-value** &#60;/p&#62;&#60;p&#62;**shadow software-type** &#60;/p&#62;&#60;p&#62;**shadowing-import software-version** &#60;/p&#62;&#60;p&#62;**shared-initialize some** &#60;/p&#62;&#60;p&#62;**shiftf sort** &#60;/p&#62;&#60;p&#62;**short-float space** &#60;/p&#62;&#60;p&#62;**short-float-epsilon special** &#60;/p&#62;&#60;p&#62;**short-float-negative-epsilon special-operator-p** &#60;/p&#62;&#60;p&#62;**short-site-name speed** &#60;/p&#62;&#60;p&#62;**signal sqrt** &#60;/p&#62;&#60;p&#62;**signed-byte stable-sort** &#60;/p&#62;&#60;p&#62;**signum standard** &#60;/p&#62;&#60;p&#62;**simple-array standard-char** &#60;/p&#62;&#60;p&#62;**simple-base-string standard-char-p**&#60;/p&#62;|
| :- |


**Figure 1–13. Symbols in the COMMON-LISP package (part ten of twelve).** 





|&#60;p&#62;**standard-class sublis** &#60;/p&#62;&#60;p&#62;**standard-generic-function subseq** &#60;/p&#62;&#60;p&#62;**standard-method subsetp** &#60;/p&#62;&#60;p&#62;**standard-object subst** &#60;/p&#62;&#60;p&#62;**step subst-if** &#60;/p&#62;&#60;p&#62;**storage-condition subst-if-not** &#60;/p&#62;&#60;p&#62;**store-value substitute** &#60;/p&#62;&#60;p&#62;**stream substitute-if** &#60;/p&#62;&#60;p&#62;**stream-element-type substitute-if-not** &#60;/p&#62;&#60;p&#62;**stream-error subtypep** &#60;/p&#62;&#60;p&#62;**stream-error-stream svref** &#60;/p&#62;&#60;p&#62;**stream-external-format sxhash** &#60;/p&#62;&#60;p&#62;**streamp symbol** &#60;/p&#62;&#60;p&#62;**string symbol-function** &#60;/p&#62;&#60;p&#62;**string-capitalize symbol-macrolet** &#60;/p&#62;&#60;p&#62;**string-downcase symbol-name** &#60;/p&#62;&#60;p&#62;**string-equal symbol-package** &#60;/p&#62;&#60;p&#62;**string-greaterp symbol-plist** &#60;/p&#62;&#60;p&#62;**string-left-trim symbol-value** &#60;/p&#62;&#60;p&#62;**string-lessp symbolp** &#60;/p&#62;&#60;p&#62;**string-not-equal synonym-stream** &#60;/p&#62;&#60;p&#62;**string-not-greaterp synonym-stream-symbol** &#60;/p&#62;&#60;p&#62;**string-not-lessp t** &#60;/p&#62;&#60;p&#62;**string-right-trim tagbody** &#60;/p&#62;&#60;p&#62;**string-stream tailp** &#60;/p&#62;&#60;p&#62;**string-trim tan** &#60;/p&#62;&#60;p&#62;**string-upcase tanh** &#60;/p&#62;&#60;p&#62;**string/= tenth** &#60;/p&#62;&#60;p&#62;**string**&#60; **terpri** &#60;/p&#62;&#60;p&#62;**string**&#60;**= the** &#60;/p&#62;&#60;p&#62;**string= third** &#60;/p&#62;&#60;p&#62;**string**&#62; **throw** &#60;/p&#62;&#60;p&#62;**string**&#62;**= time** &#60;/p&#62;&#60;p&#62;**stringp trace** &#60;/p&#62;&#60;p&#62;**structure translate-logical-pathname** &#60;/p&#62;&#60;p&#62;**structure-class translate-pathname** &#60;/p&#62;&#60;p&#62;**structure-object tree-equal** &#60;/p&#62;&#60;p&#62;**style-warning truename**&#60;/p&#62;|
| :- |


**Figure 1–14. Symbols in the COMMON-LISP package (part eleven of twelve).** Introduction **1–45**





|&#60;p&#62;**truncate values-list** &#60;/p&#62;&#60;p&#62;**two-way-stream variable** &#60;/p&#62;&#60;p&#62;**two-way-stream-input-stream vector** &#60;/p&#62;&#60;p&#62;**two-way-stream-output-stream vector-pop** &#60;/p&#62;&#60;p&#62;**type vector-push** &#60;/p&#62;&#60;p&#62;**type-error vector-push-extend** &#60;/p&#62;&#60;p&#62;**type-error-datum vectorp** &#60;/p&#62;&#60;p&#62;**type-error-expected-type warn** &#60;/p&#62;&#60;p&#62;**type-of warning** &#60;/p&#62;&#60;p&#62;**typecase when** &#60;/p&#62;&#60;p&#62;**typep wild-pathname-p** &#60;/p&#62;&#60;p&#62;**unbound-slot with-accessors** &#60;/p&#62;&#60;p&#62;**unbound-slot-instance with-compilation-unit** &#60;/p&#62;&#60;p&#62;**unbound-variable with-condition-restarts undefined-function with-hash-table-iterator unexport with-input-from-string unintern with-open-file** &#60;/p&#62;&#60;p&#62;**union with-open-stream** &#60;/p&#62;&#60;p&#62;**unless with-output-to-string** &#60;/p&#62;&#60;p&#62;**unread-char with-package-iterator** &#60;/p&#62;&#60;p&#62;**unsigned-byte with-simple-restart** &#60;/p&#62;&#60;p&#62;**untrace with-slots** &#60;/p&#62;&#60;p&#62;**unuse-package with-standard-io-syntax unwind-protect write** &#60;/p&#62;&#60;p&#62;**update-instance-for-different-class write-byte** &#60;/p&#62;&#60;p&#62;**update-instance-for-redefined-class write-char** &#60;/p&#62;&#60;p&#62;**upgraded-array-element-type write-line** &#60;/p&#62;&#60;p&#62;**upgraded-complex-part-type write-sequence** &#60;/p&#62;&#60;p&#62;**upper-case-p write-string** &#60;/p&#62;&#60;p&#62;**use-package write-to-string** &#60;/p&#62;&#60;p&#62;**use-value y-or-n-p** &#60;/p&#62;&#60;p&#62;**user-homedir-pathname yes-or-no-p** &#60;/p&#62;&#60;p&#62;**values zerop**&#60;/p&#62;|
| :- |


**Figure 1–15. Symbols in the COMMON-LISP package (part twelve of twelve).** 
