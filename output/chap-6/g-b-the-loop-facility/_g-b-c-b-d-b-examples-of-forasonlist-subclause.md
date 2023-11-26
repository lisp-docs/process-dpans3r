**6.1.2.1.3.1 Examples of for-as-on-list subclause** 

;; Collect successive tails of a list. 

(loop for sublist on ’(a b c d) 

collect sublist) 

*→* ((A B C D) (B C D) (C D) (D)) 

;; Print a list by using destructuring with the loop keyword ON. 

(loop for (item) on ’(1 2 3) 

do (print item)) 

&#9655; 1 

&#9655; 2 

&#9655; 3 

*→* NIL 

