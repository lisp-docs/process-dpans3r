 

**Syntax:** 

**invoke-restart-interactively** *restart → &#123;result&#125;*\* 

**Arguments and Values:** 

*restart*—a *restart designator* . 

*results*—the *values* returned by the *function* associated with *restart*, if that *function* returns. 

**Description:** 

**invoke-restart-interactively** calls the *function* associated with *restart*, prompting for any necessary arguments. If *restart* is a name, it must be valid in the current *dynamic environment*. 

**invoke-restart-interactively** prompts for arguments by executing the code provided in the :interactive keyword to **restart-case** or :interactive-function keyword to **restart-bind**. 

If no such options have been supplied in the corresponding **restart-bind** or **restart-case**, then the consequences are undefined if the *restart* takes required arguments. If the arguments are optional, an argument list of **nil** is used. 

Once the arguments have been determined, **invoke-restart-interactively** executes the following: (apply #’invoke-restart *restart arguments*) 

**Examples:** 

(defun add3 (x) (check-type x number) (+ x 3)) 

(add3 ’seven) 

&#9655; Error: The value SEVEN was not of type NUMBER. 

&#9655; To continue, type :CONTINUE followed by an option number: 

&#9655; 1: Specify a different value to use. 

&#9655; 2: Return to Lisp Toplevel. 

&#9655; Debug&#62; (invoke-restart-interactively ’store-value) 

&#9655; Type a form to evaluate and use: 7 

*→* 10 

**Side Effects:** 

If prompting for arguments is necesary, some typeout may occur (on *query I/O*). 

A non-local transfer of control might be done by the restart. 

**Affected By:** 

**\*query-io\***, active *restarts* 



 

 

**Exceptional Situations:** 

If *restart* is not valid, an error of *type* **control-error** is signaled. 

**See Also:** 

**find-restart**, **invoke-restart**, **restart-case**, **restart-bind** 

**Notes:** 

**invoke-restart-interactively** is used internally by the debugger and may also be useful in implementing other portable, interactive debugging tools. 
