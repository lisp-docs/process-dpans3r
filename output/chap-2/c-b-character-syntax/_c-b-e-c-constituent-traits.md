 



Every *character* has one or more *constituent traits* that define how the *character* is to be interpreted by the *Lisp reader* when the *character* is a *constituent character* . These *constituent traits* are *alphabetic*<sub>2</sub>, digit, *package marker* , plus sign, minus sign, dot, decimal point, *ratio marker* , *exponent marker* , and *invalid*. Figure 2–8 shows the *constituent traits* of the *standard characters* and of certain *semi-standard characters*; no mechanism is provided for changing the *constituent trait* of a *character* . Any *character* with the alphadigit *constituent trait* in that figure is a digit if the *current input base* is greater than that character’s digit value, otherwise the *character* is *alphabetic*<sub>2</sub>. Any *character* quoted by a *single escape* is treated as an *alphabetic*<sub>2</sub> constituent, regardless of its normal syntax.  







|<p>**constituent traits constituent traits** </p><p>**character character**</p>|

| :- |

|<p>Backspace *invalid* \&#123; *alphabetic*<sub>2</sub> </p><p>Tab *invalid*\* \&#125; *alphabetic*<sub>2</sub> </p><p>Newline *invalid*\* + *alphabetic*<sub>2</sub>, plus sign Linefeed *invalid*\* - *alphabetic*<sub>2</sub>, minus sign Page *invalid*\* . *alphabetic*<sub>2</sub>, dot, decimal point Return *invalid*\* / *alphabetic*<sub>2</sub>, *ratio marker* Space *invalid*\* A, a alphadigit </p><p>! *alphabetic*<sub>2</sub> B, b alphadigit </p><p>" *alphabetic*<sub>2</sub>\* C, c alphadigit </p><p># *alphabetic*<sub>2</sub>\* D, d alphadigit, double-float *exponent marker* $ *alphabetic*<sub>2</sub> E, e alphadigit, float *exponent marker* % *alphabetic*<sub>2</sub> F, f alphadigit, single-float *exponent marker* & *alphabetic*<sub>2</sub> G, g alphadigit </p><p>’ *alphabetic*<sub>2</sub>\* H, h alphadigit </p><p>( *alphabetic*<sub>2</sub>\* I, i alphadigit </p><p>) *alphabetic*<sub>2</sub>\* J, j alphadigit </p><p>\* *alphabetic*<sub>2</sub> K, k alphadigit </p><p>, *alphabetic*<sub>2</sub>\* L, l alphadigit, long-float *exponent marker* 0-9 alphadigit M, m alphadigit </p><p>: *package marker* N, n alphadigit </p><p>; *alphabetic*<sub>2</sub>\* O, o alphadigit </p><p>< *alphabetic*<sub>2</sub> P, p alphadigit </p><p>= *alphabetic*<sub>2</sub> Q, q alphadigit </p><p>> *alphabetic*<sub>2</sub> R, r alphadigit </p><p>? *alphabetic*<sub>2</sub> S, s alphadigit, short-float *exponent marker* @ *alphabetic*<sub>2</sub> T, t alphadigit </p><p>[ *alphabetic*<sub>2</sub> U, u alphadigit </p><p>\ *alphabetic*<sub>2</sub>\* V, v alphadigit </p><p>] *alphabetic*<sub>2</sub> W, w alphadigit </p><p><i><sup>∧</sup> alphabetic</i><sub>2</sub> X, x alphadigit </p><p>*alphabetic*<sub>2</sub> Y, y alphadigit </p><p>‘ *alphabetic*<sub>2</sub>\* Z, z alphadigit </p><p>| *alphabetic*<sub>2</sub>\* Rubout *invalid* </p><p>~ *alphabetic*<sub>2</sub></p>|





**Figure 2–8. Constituent Traits of Standard Characters and Semi-Standard Characters** 



The interpretations in this table apply only to *characters* whose *syntax type* is *constituent*. Entries marked with an asterisk (\*) are normally *shadowed* <sub>2</sub> because the indicated *characters* are of *syntax type whitespace*<sub>2</sub>, *macro character* , *single escape*, or *multiple escape*; these *constituent traits* apply to them only if their *syntax types* are changed to *constituent*.  







