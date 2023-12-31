**dpb** <GlossaryTerm styled={true} term={"function"}><i>Function</i></GlossaryTerm> 



**Syntax:** 



<DictionaryLink styled={true} term={"dpb"}><b>dpb</b></DictionaryLink> *newbyte bytespec integer → result-integer* 



**Pronunciation:** 



[ **d***  **pib** ] or [ **d***  **p*** **b** ] or [ **d—e p—e b—e** ] 



**Arguments and Values:** 



*newbyte*—an *integer* . 



*bytespec*—a *byte specifier* . 



*integer*—an *integer* . 



*result-integer*—an *integer* . 



**Description:** 



<DictionaryLink styled={true} term={"dpb"}><b>dpb</b></DictionaryLink> (deposit byte) is used to replace a field of bits within *integer*. <DictionaryLink styled={true} term={"dpb"}><b>dpb</b></DictionaryLink> returns an *integer* that is the same as *integer* except in the bits specified by *bytespec*. 



Let s be the size specified by *bytespec*; then the low s bits of *newbyte* appear in the result in the byte specified by *bytespec*. *Newbyte* is interpreted as being right-justified, as if it were the result of <DictionaryLink styled={true} term={"ldb"}><b>ldb</b></DictionaryLink>. 



**Examples:**
```lisp

(dpb 1 (byte 1 10) 0) *→* 1024 
(dpb -2 (byte 2 10) 0) *→* 2048 
(dpb 1 (byte 2 10) 2048) *→* 1024 

```
**See Also:** 



<DictionaryLink styled={true} term={"byte"}><b>byte</b></DictionaryLink>, <DictionaryLink styled={true} term={"deposit-field"}><b>deposit-field</b></DictionaryLink>, <DictionaryLink styled={true} term={"ldb"}><b>ldb</b></DictionaryLink> 



**Notes:** 



(logbitp *j* (dpb *m* (byte *s p*) *n*)) 



*≡* (if (and (&gt;= *j p*) (&lt; *j* (+ *p s*))) 



(logbitp (- *j p*) *m*) 



(logbitp *j n*)) 



In general, 





 



 



(dpb *x* (byte 0 *y*) *z*) *! z* 



for all valid values of *x*, *y*, and *z*. 



Historically, the name “dpb” comes from a DEC PDP-10 assembly language instruction meaning “deposit byte.” 



