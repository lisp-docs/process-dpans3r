*∗<DictionaryLink styled={true} term={"print-radix"}><b>*print-radix*</b></DictionaryLink>∗ Variable* 



**Value Type:** 



**\*print-base\***—a <GlossaryTerm styled={true} term={"radix"}><i>radix</i></GlossaryTerm> . <DictionaryLink styled={true} term={"print-radix"}><b>\*print-radix\*</b></DictionaryLink>—a *generalized boolean*. 



**Initial Value:** 



The initial <GlossaryTerm styled={true} term={"value"}><i>value</i></GlossaryTerm> of **\*print-base\*** is 10. The initial <GlossaryTerm styled={true} term={"value"}><i>value</i></GlossaryTerm> of <DictionaryLink styled={true} term={"print-radix"}><b>\*print-radix\*</b></DictionaryLink> is <GlossaryTerm styled={true} term={"false"}><i>false</i></GlossaryTerm>. 



**Description:** 



**\*print-base\*** and <DictionaryLink styled={true} term={"print-radix"}><b>\*print-radix\*</b></DictionaryLink> control the printing of <GlossaryTerm styled={true} term={"rational"}><i>rationals</i></GlossaryTerm>. The <GlossaryTerm styled={true} term={"value"}><i>value</i></GlossaryTerm> of **\*print-base\*** is called the *current output base*. 



The <GlossaryTerm styled={true} term={"value"}><i>value</i></GlossaryTerm> of **\*print-base\*** is the <GlossaryTerm styled={true} term={"radix"}><i>radix</i></GlossaryTerm> in which the printer will print <GlossaryTerm styled={true} term={"rational"}><i>rationals</i></GlossaryTerm>. For radices above 10, letters of the alphabet are used to represent digits above 9. 



If the <GlossaryTerm styled={true} term={"value"}><i>value</i></GlossaryTerm> of <DictionaryLink styled={true} term={"print-radix"}><b>\*print-radix\*</b></DictionaryLink> is <GlossaryTerm styled={true} term={"true"}><i>true</i></GlossaryTerm>, the printer will print a radix specifier to indicate the <GlossaryTerm styled={true} term={"radix"}><i>radix</i></GlossaryTerm> in which it is printing a <GlossaryTerm styled={true} term={"rational"}><i>rational</i></GlossaryTerm> number. The radix specifier is always printed using lowercase letters. If **\*print-base\*** is 2, 8, or 16, then the radix specifier used is #b, #o, or #x, respectively. For *integers*, base ten is indicated by a trailing decimal point instead of a leading radix specifier; for <GlossaryTerm styled={true} term={"ratio"}><i>ratios</i></GlossaryTerm>, #10r is used. 



**Examples:**
```lisp

(let ((\*print-base\* 24.) (\*print-radix\* t)) 
  (print 23.)) 
▷ #24rN 
*→* 23 
(setq \*print-base\* 10) *→* 10 
(setq \*print-radix\* nil) *→* NIL 
(dotimes (i 35) 
  (let ((\*print-base\* (+ i 2))) ;print the decimal number 40 
    (write 40) ;in each base from 2 to 36 
    (if (zerop (mod i 10)) (terpri) (format t " ")))) 
▷ 101000 
▷ 1111 220 130 104 55 50 44 40 37 34 
▷ 31 2C 2A 28 26 24 22 20 1J 1I 
▷ 1H 1G 1F 1E 1D 1C 1B 1A 19 18 
▷ 17 16 15 14 
*→* NIL 
(dolist (pb ’(2 3 8 10 16)) 

  
  
  (let ((\*print-radix\* t) ;print the integer 10 and 
	(\*print-base\* pb)) ;the ratio 1/10 in bases 2, 
    (format t "~&~S ~S~%" 10 1/10))) ;3, 8, 10, 16 
▷ #b1010 #b1/1010 
▷ #3r101 #3r1/101 
▷ #o12 #o1/12 
▷ 10. #10r1/10 
▷ #xA #x1/A 
*→* NIL 

```
**Affected By:** 



Might be <GlossaryTerm styled={true} term={"bound"}><i>bound</i></GlossaryTerm> by <DictionaryLink styled={true} term={"format"}><b>format</b></DictionaryLink>, and <DictionaryLink styled={true} term={"write"}><b>write</b></DictionaryLink>, <DictionaryLink styled={true} term={"write-to-string"}><b>write-to-string</b></DictionaryLink>. 



**See Also:** 



<DictionaryLink styled={true} term={"format"}><b>format</b></DictionaryLink>, <DictionaryLink styled={true} term={"write"}><b>write</b></DictionaryLink>, <DictionaryLink styled={true} term={"write-to-string"}><b>write-to-string</b></DictionaryLink> 



