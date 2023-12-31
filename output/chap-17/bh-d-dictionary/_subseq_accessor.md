**subseq** <GlossaryTerm styled={true} term={"accessor"}><i>Accessor</i></GlossaryTerm> 



**Syntax:** 



<DictionaryLink styled={true} term={"subseq"}><b>subseq</b></DictionaryLink> *sequence start* &amp;optional *end → subsequence* 



**(setf (subseq** *sequence start* &amp;optional *end***)** *new-subsequence***)** 



**Arguments and Values:** 



*sequence*—a *proper sequence*. 



*start*, *end*—*bounding index designators* of *sequence*. The default for *end* is <DictionaryLink styled={true} term={"nil"}><b>nil</b></DictionaryLink>. 



*subsequence*—a *proper sequence*. 



*new-subsequence*—a *proper sequence*. 



**Description:** 



<DictionaryLink styled={true} term={"subseq"}><b>subseq</b></DictionaryLink> creates a *sequence* that is a copy of the subsequence of *sequence bounded* by *start* and *end*. 



*Start* specifies an offset into the original *sequence* and marks the beginning position of the subsequence. *end* marks the position following the last element of the subsequence. 



<DictionaryLink styled={true} term={"subseq"}><b>subseq</b></DictionaryLink> always allocates a new *sequence* for a result; it never shares storage with an old *sequence*. The result subsequence is always of the same <GlossaryTerm styled={true} term={"type"}><i>type</i></GlossaryTerm> as *sequence*. 



If *sequence* is a <GlossaryTerm styled={true} term={"vector"}><i>vector</i></GlossaryTerm> , the result is a *fresh simple array* of <GlossaryTerm styled={true} term={"rank"}><i>rank</i></GlossaryTerm> one that has the same *actual array element type* as *sequence*. If *sequence* is a <GlossaryTerm styled={true} term={"list"}><i>list</i></GlossaryTerm>, the result is a *fresh list*. 



<DictionaryLink styled={true} term={"setf"}><b>setf</b></DictionaryLink> may be used with <DictionaryLink styled={true} term={"subseq"}><b>subseq</b></DictionaryLink> to destructively replace <GlossaryTerm styled={true} term={"element"}><i>elements</i></GlossaryTerm> of a subsequence with <GlossaryTerm styled={true} term={"element"}><i>elements</i></GlossaryTerm> taken from a *sequence* of new values. If the subsequence and the new sequence are not of equal 







 



 



length, the shorter length determines the number of elements that are replaced. The remaining <GlossaryTerm styled={true} term={"element"}><i>elements</i></GlossaryTerm> at the end of the longer sequence are not modified in the operation. 



**Examples:**
```lisp

(setq str "012345") *→* "012345" 
(subseq str 2) *→* "2345" 
(subseq str 3 5) *→* "34" 
(setf (subseq str 4) "abc") *→* "abc" 
str *→* "0123ab" 
(setf (subseq str 0 2) "A") *→* "A" 
str *→* "A123ab" 

```
**Exceptional Situations:** 



Should be prepared to signal an error of <GlossaryTerm styled={true} term={"type"}><i>type</i></GlossaryTerm> <DictionaryLink styled={true} term={"type-error"}><b>type-error</b></DictionaryLink> if *sequence* is not a *proper sequence*. Should be prepared to signal an error of <GlossaryTerm styled={true} term={"type"}><i>type</i></GlossaryTerm> <DictionaryLink styled={true} term={"type-error"}><b>type-error</b></DictionaryLink> if *new-subsequence* is not a *proper sequence*. 



**See Also:** 



<DictionaryLink styled={true} term={"replace"}><b>replace</b></DictionaryLink> 



