**write, prin1, print, pprint, princ** *Function* 



**Syntax:** 



**write** *object* &amp;key *array base case circle escape gensym* 



*length level lines miser-width pprint-dispatch* 



*pretty radix readably right-margin stream* 



*→ object* 



**prin1** *object* &amp;optional *output-stream → object* 



**princ** *object* &amp;optional *output-stream → object* 







 



 



**write, prin1, print, pprint, princ** 



**print** *object* &amp;optional *output-stream → object* 



**pprint** *object* &amp;optional *output-stream → hno valuesi* 



**Arguments and Values:** 



*object*—an *object*. 



*output-stream*—an *output stream designator* . The default is *standard output*. 



*array*—a *generalized boolean*. 



*base*—a *radix* . 



*case*—a *symbol* of *type* (member :upcase :downcase :capitalize). 



*circle*—a *generalized boolean*. 



*escape*—a *generalized boolean*. 



*gensym*—a *generalized boolean*. 



*length*—a non-negative *integer* , or **nil**. 



*level*—a non-negative *integer* , or **nil**. 



*lines*—a non-negative *integer* , or **nil**. 



*miser-width*—a non-negative *integer* , or **nil**. 



*pprint-dispatch*—a *pprint dispatch table*. 



*pretty*—a *generalized boolean*. 



*radix*—a *generalized boolean*. 



*readably*—a *generalized boolean*. 



*right-margin*—a non-negative *integer* , or **nil**. 



*stream*—an *output stream designator* . The default is *standard output*. 



**Description:** 



**write**, **prin1**, **princ**, **print**, and **pprint** write the printed representation of *object* to *output-stream*. 



**write** is the general entry point to the *Lisp printer* . For each explicitly supplied *keyword parameter* named in Figure 22–7, the corresponding *printer control variable* is dynamically bound to its *value* while printing goes on; for each *keyword parameter* in Figure 22–7 that is not explicitly supplied, the value of the corresponding *printer control variable* is the same as it was at the time **write** was invoked. Once the appropriate *bindings* are *established*, the *object* is output by the *Lisp printer* . 







 



 



**write, prin1, print, pprint, princ** 



|**Parameter Corresponding Dynamic Variable**|

| :- |

|<p>*array* **\*print-array\*** </p><p>*base* **\*print-base\*** </p><p>*case* **\*print-case\*** </p><p>*circle* **\*print-circle\*** </p><p>*escape* **\*print-escape\*** </p><p>*gensym* **\*print-gensym\*** </p><p>*length* **\*print-length\*** </p><p>*level* **\*print-level\*** </p><p>*lines* **\*print-lines\*** </p><p>*miser-width* **\*print-miser-width\*** </p><p>*pprint-dispatch* **\*print-pprint-dispatch\*** </p><p>*pretty* **\*print-pretty\*** </p><p>*radix* **\*print-radix\*** </p><p>*readably* **\*print-readably\*** </p><p>*right-margin* **\*print-right-margin\***</p>|





**Figure 22–7. Argument correspondences for the WRITE function.** 



**prin1**, **princ**, **print**, and **pprint** implicitly *bind* certain print parameters to particu lar values. The remaining parameter values are taken from **\*print-array\***, **\*print-base\***, **\*print-case\***, **\*print-circle\***, **\*print-escape\***, **\*print-gensym\***, **\*print-length\***, **\*print-level\***, **\*print-lines\***, **\*print-miser-width\***, **\*print-pprint-dispatch\***, **\*print-pretty\***, **\*print-radix\***, and **\*print-right-margin\***. 



**prin1** produces output suitable for input to **read**. It binds **\*print-escape\*** to *true*. 



**princ** is just like **prin1** except that the output has no *escape characters*. It binds **\*print-escape\*** to *false* and **\*print-readably\*** to *false*. The general rule is that output from **princ** is intended to look good to people, while output from **prin1** is intended to be acceptable to **read**. 



**print** is just like **prin1** except that the printed representation of *object* is preceded by a newline and followed by a space. 



**pprint** is just like **print** except that the trailing space is omitted and *object* is printed with the **\*print-pretty\*** flag *non-nil* to produce pretty output. 



*Output-stream* specifies the *stream* to which output is to be sent. 



**Affected By:** 



**\*standard-output\***, **\*terminal-io\***, **\*print-escape\***, **\*print-radix\***, **\*print-base\***, **\*print-circle\***, **\*print-pretty\***, **\*print-level\***, **\*print-length\***, **\*print-case\***, **\*print-gensym\***, **\*print-array\***, **\*read-default-float-format\***. 







 



 



**See Also:** 



**readtable-case**, Section 22.3.4 (FORMAT Printer Operations) 



**Notes:** 



The *functions* **prin1** and **print** do not bind **\*print-readably\***. 



(prin1 object output-stream) 



*≡* (write object :stream output-stream :escape t) 



(princ object output-stream) 



*≡* (write object stream output-stream :escape nil :readably nil) 



(print object output-stream) 



*≡* (progn (terpri output-stream) 



(write object :stream output-stream 



:escape t) 



(write-char #\space output-stream)) 



(pprint object output-stream) 



*≡* (write object :stream output-stream :escape t :pretty t) 



