 

**Syntax:** 

**make-dispatch-macro-character** *char* &optional *non-terminating-p readtable →* **t** 

**Arguments and Values:** 

*char*—a *character* . 

*non-terminating-p*—a *generalized boolean*. The default is *false*. 

*readtable*—a *readtable*. The default is the *current readtable*. 

**Description:** 

**make-dispatch-macro-character** makes *char* be a *dispatching macro character* in *readtable*. 

Initially, every *character* in the dispatch table associated with the *char* has an associated function that signals an error of *type* **reader-error**. 



 

 

If *non-terminating-p* is *true*, the *dispatching macro character* is made a *non-terminating macro character* ; if *non-terminating-p* is *false*, the *dispatching macro character* is made a *terminating macro character* . 

**Examples:** 

(get-macro-character #\&#123;) *→* NIL, *false* 

(make-dispatch-macro-character #\&#123;) *→* T 

(not (get-macro-character #\&#123;)) *→ false* 

The *readtable* is altered. 

**See Also:** 

**\*readtable\***, **set-dispatch-macro-character** 
