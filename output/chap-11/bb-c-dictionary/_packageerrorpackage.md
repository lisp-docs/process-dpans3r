 

**Syntax:** 

**package-error-package** *condition → package* 

**Arguments and Values:** 

*condition*—a *condition* of *type* **package-error**. 

*package*—a *package designator* . 

**Description:** 

Returns a *designator* for the offending *package* in the *situation* represented by the *condition*. **Examples:** 

(package-error-package 

(make-condition ’package-error 

:package (find-package "COMMON-LISP"))) 

*→* #&#60;Package "COMMON-LISP"&#62; 

**See Also:** 

**package-error** 


