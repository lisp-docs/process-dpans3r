**and** <GlossaryTerm styled={true} term={"macro"}><i>Macro</i></GlossaryTerm> 



**Syntax:** 



<DictionaryLink styled={true} term={"and"}><b>and</b></DictionaryLink> <GlossaryTerm styled={true} term={"form"}><i>\{form\}</i></GlossaryTerm>\* *→ \{result\}*\* 



**Arguments and Values:** 



<GlossaryTerm styled={true} term={"form"}><i>form</i></GlossaryTerm>—a <GlossaryTerm styled={true} term={"form"}><i>form</i></GlossaryTerm>. 



*results*—the <GlossaryTerm styled={true} term={"value"}><i>values</i></GlossaryTerm> resulting from the evaluation of the last <GlossaryTerm styled={true} term={"form"}><i>form</i></GlossaryTerm>, or the symbols <DictionaryLink styled={true} term={"nil"}><b>nil</b></DictionaryLink> or <DictionaryLink styled={true} term={"t"}><b>t</b></DictionaryLink>. 



**Description:** 



The macro <DictionaryLink styled={true} term={"and"}><b>and</b></DictionaryLink> evaluates each <GlossaryTerm styled={true} term={"form"}><i>form</i></GlossaryTerm> one at a time from left to right. As soon as any <GlossaryTerm styled={true} term={"form"}><i>form</i></GlossaryTerm> evaluates to <DictionaryLink styled={true} term={"nil"}><b>nil</b></DictionaryLink>, <DictionaryLink styled={true} term={"and"}><b>and</b></DictionaryLink> returns <DictionaryLink styled={true} term={"nil"}><b>nil</b></DictionaryLink> without evaluating the remaining <GlossaryTerm styled={true} term={"form"}><i>forms</i></GlossaryTerm>. If all <GlossaryTerm styled={true} term={"form"}><i>forms</i></GlossaryTerm> but the last evaluate to <GlossaryTerm styled={true} term={"true"}><i>true</i></GlossaryTerm> values, <DictionaryLink styled={true} term={"and"}><b>and</b></DictionaryLink> returns the results produced by evaluating the last <GlossaryTerm styled={true} term={"form"}><i>form</i></GlossaryTerm>. 



If no <GlossaryTerm styled={true} term={"form"}><i>forms</i></GlossaryTerm> are supplied, (and) returns <DictionaryLink styled={true} term={"t"}><b>t</b></DictionaryLink>. 



<DictionaryLink styled={true} term={"and"}><b>and</b></DictionaryLink> passes back multiple values from the last <GlossaryTerm styled={true} term={"subform"}><i>subform</i></GlossaryTerm> but not from subforms other than the last. **Examples:**
```lisp

(if (and (>= n 0) 



	 (< n (length a-simple-vector)) 
	 (eq (elt a-simple-vector n) ’foo)) 
    (princ "Foo!")) 
The above expression prints Foo! if element n of a-simple-vector is the symbol foo, provided also that n is indeed a valid index for a-simple-vector. Because **and** guarantees left-to-right testing of its parts, **elt** is not called if n is out of range. 
(setq temp1 1 temp2 1 temp3 1) *→* 1 
(and (incf temp1) (incf temp2) (incf temp3)) *→* 2 
(and (eql 2 temp1) (eql 2 temp2) (eql 2 temp3)) *→ true* 
(decf temp3) *→* 1 
(and (decf temp1) (decf temp2) (eq temp3 ’nil) (decf temp3)) *→* NIL 
(and (eql temp1 temp2) (eql temp2 temp3)) *→ true* 
(and) *→* T 

```
**See Also:** 



<DictionaryLink styled={true} term={"cond"}><b>cond</b></DictionaryLink>, <DictionaryLink styled={true} term={"every"}><b>every</b></DictionaryLink>, <DictionaryLink styled={true} term={"if"}><b>if</b></DictionaryLink>, <DictionaryLink styled={true} term={"or"}><b>or</b></DictionaryLink>, <DictionaryLink styled={true} term={"when"}><b>when</b></DictionaryLink> 



**Notes:** 



(and <GlossaryTerm styled={true} term={"form"}><i>form</i></GlossaryTerm>) *≡* (let () <GlossaryTerm styled={true} term={"form"}><i>form</i></GlossaryTerm>) 



(and *form1 form2* ...) *≡* (when *form1* (and *form2* ...)) 



