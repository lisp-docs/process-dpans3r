**error** *Function* 



**Syntax:** 



**error** *datum* &amp;rest *arguments →* 



**Arguments and Values:** 



*datum*, *arguments*—*designators* for a *condition* of default type **simple-error**. 



**Description:** 



**error** effectively invokes **signal** on the denoted *condition*. 



If the *condition* is not handled, (invoke-debugger *condition*) is done. As a consequence of calling **invoke-debugger**, **error** cannot directly return; the only exit from **error** can come by non-local transfer of control in a handler or by use of an interactive debugging command. 



**Examples:**
```lisp

(defun factorial (x) 
  (cond ((or (not (typep x ’integer)) (minusp x)) 
	 (error "&#126;S is not a valid argument to FACTORIAL." x)) 
	((zerop x) 1) 

	
	
	**error** 
	(t (\* x (factorial (- x 1)))))) 
*→* FACTORIAL 
(factorial 20) 
*→* 2432902008176640000 
(factorial -1) 
▷ Error: -1 is not a valid argument to FACTORIAL. 
▷ To continue, type :CONTINUE followed by an option number: 
▷ 1: Return to Lisp Toplevel. 
▷ Debug> 
(setq a ’fred) 
*→* FRED 
(if (numberp a) (1+ a) (error "&#126;S is not a number." A)) 
▷ Error: FRED is not a number. 
▷ To continue, type :CONTINUE followed by an option number: 
▷ 1: Return to Lisp Toplevel. 
▷ Debug> :Continue 1 
▷ Return to Lisp Toplevel. 
(define-condition not-a-number (error) 
  ((argument :reader not-a-number-argument :initarg :argument)) 
  (:report (lambda (condition stream) 
	     (format stream "&#126;S is not a number." 
		     (not-a-number-argument condition))))) 
*→* NOT-A-NUMBER 
(if (numberp a) (1+ a) (error ’not-a-number :argument a)) 
▷ Error: FRED is not a number. 
▷ To continue, type :CONTINUE followed by an option number: 
▷ 1: Return to Lisp Toplevel. 
▷ Debug> :Continue 1 
▷ Return to Lisp Toplevel. 

```
**Side Effects:** 



*Handlers* for the specified condition, if any, are invoked and might have side effects. Program execution might stop, and the debugger might be entered. 



**Affected By:** 



Existing handler bindings. 



**\*break-on-signals\*** 



Signals an error of *type* **type-error** if *datum* and *arguments* are not *designators* for a *condition*. 



**See Also:** 



**cerror**, **signal**, **format**, **ignore-errors**, **\*break-on-signals\***, **handler-bind**, Section 9.1 (Condition 



 



 



System Concepts) 



**Notes:** 



Some implementations may provide debugger commands for interactively returning from individual stack frames. However, it should be possible for the programmer to feel confident about writing code like: 



(defun wargames:no-win-scenario () 



(if (error "pushing the button would be stupid.")) 



(push-the-button)) 



In this scenario, there should be no chance that **error** will return and the button will get pushed. 



While the meaning of this program is clear and it might be proven ‘safe’ by a formal theorem prover, such a proof is no guarantee that the program is safe to execute. Compilers have been known to have bugs, computers to have signal glitches, and human beings to manually intervene in ways that are not always possible to predict. Those kinds of errors, while beyond the scope of the condition system to formally model, are not beyond the scope of things that should seriously be considered when writing code that could have the kinds of sweeping effects hinted at by this example. 


