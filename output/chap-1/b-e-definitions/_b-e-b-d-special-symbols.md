 



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



<i><sup>or</sup>→</i> 



The notation “ <i><sup>or</sup>→</i>” is used to denote one of several possible alternate results. The example 



(char-name #\a) 



*→* NIL 



<i><sup>or</sup>→</i> "LOWERCASE-a" 



<i><sup>or</sup>→</i> "Small-A" 



<i><sup>or</sup>→</i> "LA01" 



indicates that **nil**, "LOWERCASE-a", "Small-A", "LA01" are among the possible results of (char-name #\a)—each with equal preference. Unless explicitly specified otherwise, it should not be assumed that the set of possible results shown is exhaustive. Formally, the above example is equivalent to 



(char-name #\a) *→ implementation-dependent* 



but it is intended to provide additional information to illustrate some of the ways in which it is permitted for implementations to diverge. 



<i><sup>not</sup> →</i> 



The notation “<i><sup>not</sup> →</i>” is used to denote a result which is not possible. This might be used, for example, in order to emphasize a situation where some anticipated misconception might lead the reader to falsely believe that the result might be possible. For example, 



(function-lambda-expression 



(funcall #’(lambda (x) #’(lambda () x)) nil)) 



*→* NIL, *true*, NIL 



<i><sup>or</sup>→</i> (LAMBDA () X), <i>true</i>, NIL 



<i><sup>not</sup> →</i> NIL, <i>false</i>, NIL 



<i><sup>not</sup> →</i> (LAMBDA () X), <i>false</i>, NIL 



*≡* 



This indicates code equivalence. For example: 



(gcd x (gcd y z)) *≡* (gcd (gcd x y) z) 



This means that the results and observable side-effects of evaluating the *form* 



(gcd x (gcd y z)) are always the same as the results and observable side-effects of (gcd (gcd x y) z) for any x, y, and z. 



▷  







Common Lisp specifies input and output with respect to a non-interactive stream model. The specific details of how interactive input and output are mapped onto that non-interactive model are *implementation-defined*. 



For example, *conforming implementations* are permitted to differ in issues of how interactive input is terminated. For example, the *function* **read** terminates when the final delimiter is typed on a non-interactive stream. In some *implementations*, an interactive call to **read** returns as soon as the final delimiter is typed, even if that delimiter is not a *newline*. In other *implementations*, a final *newline* is always required. In still other *implementations*, there might be a command which “activates” a buffer full of input without the command itself being visible on the program’s input stream. 



In the examples in this document, the notation “▷” precedes lines where interactive input and output occurs. Within such a scenario, “this notation” notates user input. 



For example, the notation 



(+ 1 (print (+ (sqrt (read)) (sqrt (read))))) 



▷ 9 16 



▷ 7 



*→* 8 



shows an interaction in which “(+ 1 (print (+ (sqrt (read)) (sqrt (read)))))” is a *form* to be *evaluated*, “9 16 ” is interactive input, “7” is interactive output, and “8” is the *value yielded* from the *evaluation*. 



The use of this notation is intended to disguise small differences in interactive input and output behavior between *implementations*. 



Sometimes, the non-interactive stream model calls for a *newline*. How that *newline* character is interactively entered is an *implementation-defined* detail of the user interface, but in that case, either the notation “*hNewlinei*” or “*←-*” might be used. 



(progn (format t "&#126;&amp;Who? ") (read-line)) 



▷ Who? Fred, Mary, and Sally*←-* 



*→* "Fred, Mary, and Sally", *&#126;&#126;fal&#126;&#126;se* 



