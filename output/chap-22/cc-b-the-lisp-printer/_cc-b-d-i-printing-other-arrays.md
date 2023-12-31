 



If <DictionaryLink styled={true} term={"print-array"}><b>\*print-array\*</b></DictionaryLink> is <GlossaryTerm styled={true} term={"true"}><i>true</i></GlossaryTerm> and <DictionaryLink styled={true} term={"print-readably"}><b>\*print-readably\*</b></DictionaryLink> is <GlossaryTerm styled={true} term={"false"}><i>false</i></GlossaryTerm>, any <GlossaryTerm styled={true} term={"array"}><i>array</i></GlossaryTerm> other than a <GlossaryTerm styled={true} term={"vector"}><i>vector</i></GlossaryTerm> is printed using #nA format. Let n be the <GlossaryTerm styled={true} term={"rank"}><i>rank</i></GlossaryTerm> of the <GlossaryTerm styled={true} term={"array"}><i>array</i></GlossaryTerm>. Then # is printed, then n as a decimal integer, then A, then n open parentheses. Next the <GlossaryTerm styled={true} term={"element"}><i>elements</i></GlossaryTerm> are scanned in row-major order, using <DictionaryLink styled={true} term={"write"}><b>write</b></DictionaryLink> on each <GlossaryTerm styled={true} term={"element"}><i>element</i></GlossaryTerm>, and separating <GlossaryTerm styled={true} term={"element"}><i>elements</i></GlossaryTerm> from each other with <GlossaryTerm styled={true} term={"whitespace"}><i>whitespace</i></GlossaryTerm><sub>1</sub>. The array’s dimensions are numbered 0 to n-1 from left to right, and are enumerated with the rightmost index changing fastest. Every time the index for dimension j is incremented, the following actions are taken: 



*•* If j &lt; n-1, then a close parenthesis is printed. 



*•* If incrementing the index for dimension j caused it to equal dimension j, that index is reset to zero and the index for dimension j-1 is incremented (thereby performing these three steps recursively), unless j=0, in which case the entire algorithm is terminated. If incrementing the index for dimension j did not cause it to equal dimension j, then a space is printed. 



*•* If j &lt; n-1, then an open parenthesis is printed. 



This causes the contents to be printed in a format suitable for :initial-contents to <DictionaryLink styled={true} term={"make-array"}><b>make-array</b></DictionaryLink>. The lists effectively printed by this procedure are subject to truncation by **\*print-level\*** and <DictionaryLink styled={true} term={"print-length"}><b>\*print-length\*</b></DictionaryLink>. 



If the <GlossaryTerm styled={true} term={"array"}><i>array</i></GlossaryTerm> is of a specialized <GlossaryTerm styled={true} term={"type"}><i>type</i></GlossaryTerm>, containing bits or characters, then the innermost lists generated by the algorithm given above can instead be printed using bit-vector or string syntax, provided that these innermost lists would not be subject to truncation by <DictionaryLink styled={true} term={"print-length"}><b>\*print-length\*</b></DictionaryLink>. 



If both <DictionaryLink styled={true} term={"print-array"}><b>\*print-array\*</b></DictionaryLink> and <DictionaryLink styled={true} term={"print-readably"}><b>\*print-readably\*</b></DictionaryLink> are <GlossaryTerm styled={true} term={"false"}><i>false</i></GlossaryTerm>, then the <GlossaryTerm styled={true} term={"array"}><i>array</i></GlossaryTerm> is printed in a format (using #&lt;) that is concise but not readable. 



If <DictionaryLink styled={true} term={"print-readably"}><b>\*print-readably\*</b></DictionaryLink> is <GlossaryTerm styled={true} term={"true"}><i>true</i></GlossaryTerm>, the <GlossaryTerm styled={true} term={"array"}><i>array</i></GlossaryTerm> prints in an <GlossaryTerm styled={true} term={"implementation-defined"}><i>implementation-defined</i></GlossaryTerm> manner; see the <GlossaryTerm styled={true} term={"variable"}><i>variable</i></GlossaryTerm> <DictionaryLink styled={true} term={"print-readably"}><b>\*print-readably\*</b></DictionaryLink>. In particular, this may be important for arrays having some dimension 0. 



For information on how the *Lisp reader* parses these “other <GlossaryTerm styled={true} term={"array"}><i>arrays</i></GlossaryTerm>,” see Section 2.4.8.12 (Sharpsign A). 



