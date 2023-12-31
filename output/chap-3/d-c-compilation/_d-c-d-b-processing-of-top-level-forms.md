 



Processing of *top level forms* in the file compiler is defined as follows: 



1\. If the <GlossaryTerm styled={true} term={"form"}><i>form</i></GlossaryTerm> is a *compiler macro form* (not disabled by a <DictionaryLink styled={true} term={"notinline"}><b>notinline</b></DictionaryLink> <GlossaryTerm styled={true} term={"declaration"}><i>declaration</i></GlossaryTerm>), the <GlossaryTerm styled={true} term={"implementation"}><i>implementation</i></GlossaryTerm> might or might not choose to compute the *compiler macro expansion* of the <GlossaryTerm styled={true} term={"form"}><i>form</i></GlossaryTerm> and, having performed the expansion, might or might not choose to process the result as a *top level form* in the same processing mode (compile-time-too or not-compile time). If it declines to obtain or use the expansion, it must process the original <GlossaryTerm styled={true} term={"form"}><i>form</i></GlossaryTerm>. 



2\. If the form is a *macro form*, its *macro expansion* is computed and processed as a *top level form* in the same processing mode (compile-time-too or not-compile-time). 



3\. If the form is a <DictionaryLink styled={true} term={"progn"}><b>progn</b></DictionaryLink> form, each of its body <GlossaryTerm styled={true} term={"form"}><i>forms</i></GlossaryTerm> is sequentially processed as a *top level form* in the same processing mode. 



4\. If the form is a <DictionaryLink styled={true} term={"locally"}><b>locally</b></DictionaryLink>, <DictionaryLink styled={true} term={"macrolet"}><b>macrolet</b></DictionaryLink>, or <DictionaryLink styled={true} term={"symbol-macrolet"}><b>symbol-macrolet</b></DictionaryLink>, <DictionaryLink styled={true} term={"compile-file"}><b>compile-file</b></DictionaryLink> establishes the appropriate bindings and processes the body forms as *top level forms* with those bindings in effect in the same processing mode. (Note that this implies that the lexical <GlossaryTerm styled={true} term={"environment"}><i>environment</i></GlossaryTerm> in which *top level forms* are processed is not necessarily the *null lexical environment*.) 



5\. If the form is an <DictionaryLink styled={true} term={"eval-when"}><b>eval-when</b></DictionaryLink> form, it is handled according to Figure 3–7.  







|**CT LT E Mode Action New Mode**|

| :- |

|<p>Yes Yes — — Process compile-time-too No Yes Yes CTT Process compile-time-too No Yes Yes NCT Process not-compile-time No Yes No — Process not-compile-time Yes No — — Evaluate — </p><p>No No Yes CTT Evaluate — No No Yes NCT Discard — No No No — Discard —</p>|





**Figure 3–7. EVAL-WHEN processing** 



Column **CT** indicates whether :compile-toplevel is specified. Column **LT** indicates whether :load-toplevel is specified. Column **E** indicates whether :execute is specified. Column **Mode** indicates the processing mode; a dash (—) indicates that the processing mode is not relevant. 



The
 **Action**
 column specifies one of three actions: 



**Process:** process the body as *top level forms* in the specified mode. 



**Evaluate:** evaluate the body in the dynamic execution context of the compiler, using the *evaluation environment* as the global environment and the *lexical environment* in which the <DictionaryLink styled={true} term={"eval-when"}><b>eval-when</b></DictionaryLink> appears. 



**Discard:** ignore the <GlossaryTerm styled={true} term={"form"}><i>form</i></GlossaryTerm>. 



The **New Mode** column indicates the new processing mode. A dash (—) indicates the compiler remains in its current mode. 



6\. Otherwise, the form is a *top level form* that is not one of the special cases. In compile time-too mode, the compiler first evaluates the form in the evaluation <GlossaryTerm styled={true} term={"environment"}><i>environment</i></GlossaryTerm> and then minimally compiles it. In not-compile-time mode, the <GlossaryTerm styled={true} term={"form"}><i>form</i></GlossaryTerm> is simply minimally compiled. All <GlossaryTerm styled={true} term={"subform"}><i>subforms</i></GlossaryTerm> are treated as *non-top-level forms*. 



Note that *top level forms* are processed in the order in which they textually appear in the file and that each *top level form* read by the compiler is processed before the next is read. However, the order of processing (including macro expansion) of <GlossaryTerm styled={true} term={"subform"}><i>subforms</i></GlossaryTerm> that are not *top level forms* and the order of further compilation is unspecified as long as Common Lisp 



semantics are preserved. 



<DictionaryLink styled={true} term={"eval-when"}><b>eval-when</b></DictionaryLink> forms cause compile-time evaluation only at top level. Both :compile-toplevel and :load-toplevel situation specifications are ignored for *non-top-level forms*. For *non-top-level*  







<GlossaryTerm styled={true} term={"form"}><i>forms</i></GlossaryTerm>, an <DictionaryLink styled={true} term={"eval-when"}><b>eval-when</b></DictionaryLink> specifying the :execute situation is treated as an *implicit progn* including the <GlossaryTerm styled={true} term={"form"}><i>forms</i></GlossaryTerm> in the body of the <DictionaryLink styled={true} term={"eval-when"}><b>eval-when</b></DictionaryLink> <GlossaryTerm styled={true} term={"form"}><i>form</i></GlossaryTerm>; otherwise, the <GlossaryTerm styled={true} term={"form"}><i>forms</i></GlossaryTerm> in the body are ignored. 



