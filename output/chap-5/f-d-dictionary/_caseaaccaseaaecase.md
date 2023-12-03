 

**Syntax:** 

**case** *keyform &#123;↓normal-clause&#125;*\* [*↓otherwise-clause*] *→ &#123;result&#125;*\* 

**ccase** *keyplace &#123;↓normal-clause&#125;*\* *→ &#123;result&#125;*\* 

**ecase** *keyform &#123;↓normal-clause&#125;*\* *→ &#123;result&#125;*\* 

*normal-clause::*=(*keys &#123;form&#125;*\*) 

*otherwise-clause::*=(*&#123;otherwise | t&#125; &#123;form&#125;*\*) 

*clause::*=*normal-clause | otherwise-clause* 

**Arguments and Values:** 

*keyform*—a *form*; evaluated to produce a *test-key*. 

*keyplace*—a *form*; evaluated initially to produce a *test-key*. Possibly also used later as a *place* if no *keys* match. 

*test-key*—an object produced by evaluating *keyform* or *keyplace*. 

*keys*—a *designator* for a *list* of *objects*. In the case of **case**, the *symbols* **t** and **otherwise** may not be used as the *keys designator* . To refer to these *symbols* by themselves as *keys*, the designators (t) and (otherwise), respectively, must be used instead. 

*forms*—an *implicit progn*. 

*results*—the *values* returned by the *forms* in the matching *clause*. 

**Description:** 

These *macros* allow the conditional execution of a body of *forms* in a *clause* that is selected by matching the *test-key* on the basis of its identity. 

The *keyform* or *keyplace* is *evaluated* to produce the *test-key*. 

Each of the *normal-clauses* is then considered in turn. If the *test-key* is the *same* as any *key* for that *clause*, the *forms* in that *clause* are *evaluated* as an *implicit progn*, and the *values* it returns are returned as the value of the **case**, **ccase**, or **ecase** *form*. 

These *macros* differ only in their *behavior* when no *normal-clause* matches; specifically: **case** 

If no *normal-clause* matches, and there is an *otherwise-clause*, then that *otherwise-clause* 





**case, ccase, ecase** 

automatically matches; the *forms* in that *clause* are *evaluated* as an *implicit progn*, and the *values* it returns are returned as the value of the **case**. 

If there is no *otherwise-clause*, **case** returns **nil**. 

**ccase** 

If no *normal-clause* matches, a *correctable error* of *type* **type-error** is signaled. The offending datum is the *test-key* and the expected type is *type equivalent* to (member *key1 key2* ...). The **store-value** *restart* can be used to correct the error. 

If the **store-value** *restart* is invoked, its *argument* becomes the new *test-key*, and is stored in *keyplace* as if by (setf *keyplace test-key*). Then **ccase** starts over, considering each *clause* anew. 

The subforms of *keyplace* might be evaluated again if none of the cases holds. 

**ecase** 

If no *normal-clause* matches, a *non-correctable error* of *type* **type-error** is signaled. The offending datum is the *test-key* and the expected type is *type equivalent* to (member *key1 key2* ...). 

Note that in contrast with **ccase**, the caller of **ecase** may rely on the fact that **ecase** does not return if a *normal-clause* does not match. 

**Examples:** 

(dolist (k ’(1 2 3 :four #\v () t ’other)) 

(format t "~S " 

(case k ((1 2) ’clause1) 

(3 ’clause2) 

(nil ’no-keys-so-never-seen) 

((nil) ’nilslot) 

((:four #\v) ’clause4) 

((t) ’tslot) 

(otherwise ’others)))) 

&#9655; CLAUSE1 CLAUSE1 CLAUSE2 CLAUSE4 CLAUSE4 NILSLOT TSLOT OTHERS 

*→* NIL 

(defun add-em (x) (apply #’+ (mapcar #’decode x))) 

*→* ADD-EM 

(defun decode (x) 

(ccase x 

((i uno) 1) 

((ii dos) 2) 

((iii tres) 3) 

((iv cuatro) 4))) 

*→* DECODE 

Data and Control 





(add-em ’(uno iii)) *→* 4 

(add-em ’(uno iiii)) 

&#9655; Error: The value of X, IIII, is not I, UNO, II, DOS, III, 

&#9655; TRES, IV, or CUATRO. 

&#9655; 1: Supply a value to use instead. 

&#9655; 2: Return to Lisp Toplevel. 

&#9655; Debug&#62; :CONTINUE 1 

&#9655; Value to evaluate and use for X: ’IV 

*→* 5 

**Side Effects:** 

The debugger might be entered. If the **store-value** *restart* is invoked, the *value* of *keyplace* might be changed. 

**Affected By:** 

**ccase** and **ecase**, since they might signal an error, are potentially affected by existing *handlers* and **\*debug-io\***. 

**Exceptional Situations:** 

**ccase** and **ecase** signal an error of *type* **type-error** if no *normal-clause* matches. 

**See Also:** 

**cond**, **typecase**, **setf**, Section 5.1 (Generalized Reference) 

**Notes:** 

(case *test-key* 

*&#123;*((*&#123;key&#125;*\*) *&#123;form&#125;*\*)*&#125;*\*) 

*≡* 

(let ((#1=#:g0001 *test-key*)) 

(cond *&#123;*((member #1# ’(*&#123;key&#125;*\*)) *&#123;form&#125;*\*)*&#125;*\*)) 

The specific error message used by **ecase** and **ccase** can vary between implementations. In situations where control of the specific wording of the error message is important, it is better to use **case** with an *otherwise-clause* that explicitly signals an error with an appropriate message. 

