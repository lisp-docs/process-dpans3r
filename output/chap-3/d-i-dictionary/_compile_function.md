**compile** *Function* 



**Syntax:** 



**compile** *name* &amp;optional *definition → function, warnings-p, failure-p* 



**Arguments and Values:** 



*name*—a *function name*, or **nil**. 



*definition*—a *lambda expression* or a *function*. The default is the function definition of *name* if it names a *function*, or the *macro function* of *name* if it names a *macro*. The consequences are undefined if no *definition* is supplied when the *name* is **nil**. 



*function*—the *function-name*, or a *compiled function*. 



*warnings-p*—a *generalized boolean*.  







**compile** 



*failure-p*—a *generalized boolean*. 



**Description:** 



Compiles an *interpreted function*. 



**compile** produces a *compiled function* from *definition*. If the *definition* is a *lambda expression*, it is coerced to a *function*. If the *definition* is already a *compiled function*, **compile** either produces that function itself (*i.e.*, is an identity operation) or an equivalent function. 



If the *name* is **nil**, the resulting *compiled function* is returned directly as the *primary value*. If a *non-nil name* is given, then the resulting *compiled function* replaces the existing *function* definition of *name* and the *name* is returned as the *primary value*; if *name* is a *symbol* that names a *macro*, its *macro function* is updated and the *name* is returned as the *primary value*. 



*Literal objects* appearing in code processed by the **compile** function are neither copied nor *coalesced*. The code resulting from the execution of **compile** references *objects* that are **eql** to the corresponding *objects* in the source code. 



**compile** is permitted, but not required, to *establish* a *handler* for *conditions* of *type* **error**. For example, the *handler* might issue a warning and restart compilation from some *implementation dependent* point in order to let the compilation proceed without manual intervention. 



The *secondary value*, *warnings-p*, is *false* if no *conditions* of *type* **error** or **warning** were detected by the compiler, and *true* otherwise. 



The *tertiary value*, *failure-p*, is *false* if no *conditions* of *type* **error** or **warning** (other than **style-warning**) were detected by the compiler, and *true* otherwise. 



**Examples:**
```lisp

(defun foo () "bar") *→* FOO 
(compiled-function-p #’foo) *→ implementation-dependent* 
(compile ’foo) *→* FOO 
(compiled-function-p #’foo) *→ true* 
(setf (symbol-function ’foo) 
      (compile nil ’(lambda () "replaced"))) *→* #<Compiled-Function> 
(foo) *→* "replaced" 

```
**Affected By:** 



**\*error-output\***, **\*macroexpand-hook\***. 



The presence of macro definitions and proclamations. 



**Exceptional Situations:** 



The consequences are undefined if the *lexical environment* surrounding the *function* to be compiled contains any *bindings* other than those for *macros*, *symbol macros*, or *declarations*. 



For information about errors detected during the compilation process, see Section 3.2.5 (Exceptional Evaluation and 











Situations in the Compiler). 



**See Also:** 



**compile-file** 


