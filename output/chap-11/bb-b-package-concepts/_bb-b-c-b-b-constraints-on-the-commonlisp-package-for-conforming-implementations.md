 



In a *conforming implementation*, an *external symbol* of the COMMON-LISP *package* can have a *function*, *macro*, or *special operator* definition, a *global variable* definition (or other status as a *dynamic variable* due to a **special** *proclamation*), or a *type* definition only if explicitly permitted in this standard. For example, **fboundp** *yields false* for any *external symbol* of the COMMON-LISP *package* that is not the *name* of a *standardized function*, *macro* or *special operator* , and **boundp** returns *false* for any *external symbol* of the COMMON-LISP *package* that is not the *name* of a *standardized global variable*. It also follows that *conforming programs* can use *external symbols* of the COMMON-LISP *package* as the *names* of local *lexical variables* with confidence that those *names* have not been *proclaimed* **special** by the *implementation* unless those *symbols* are *names* of *standardized global variables*. 



A *conforming implementation* must not place any *property* on an *external symbol* of the COMMON-LISP *package* using a *property indicator* that is either an *external symbol* of any *standardized package* or a *symbol* that is otherwise *accessible* in the COMMON-LISP-USER *package*. 



