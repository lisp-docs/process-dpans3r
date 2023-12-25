 



These rules define the congruence of a set of *lambda lists*, including the *lambda list* of each method for a given generic function and the *lambda list* specified for the generic function itself, if given. 



1\. Each *lambda list* must have the same number of required parameters. 



2\. Each *lambda list* must have the same number of optional parameters. Each method can supply its own default for an optional parameter. 



3\. If any *lambda list* mentions **&amp;rest** or **&amp;key**, each *lambda list* must mention one or both of them. 







 



 



4\. If the *generic function lambda list* mentions **&amp;key**, each method must accept all of the keyword names mentioned after **&amp;key**, either by accepting them explicitly, by specifying **&amp;allow-other-keys**, or by specifying **&amp;rest** but not **&amp;key**. Each method can accept additional keyword arguments of its own. The checking of the validity of keyword names is done in the generic function, not in each method. A method is invoked as if the keyword argument pair whose name is :allow-other-keys and whose value is <GlossaryTerm styled={true} term={"true"}><i>true</i></GlossaryTerm> were supplied, though no such argument pair will be passed. 



5\. The use of **&amp;allow-other-keys** need not be consistent across *lambda lists*. If **&amp;allow-other-keys** is mentioned in the *lambda list* of any applicable <GlossaryTerm styled={true} term={"method"}><i>method</i></GlossaryTerm> or of the *generic function*, any keyword arguments may be mentioned in the call to the *generic function*. 



6\. The use of **&amp;aux** need not be consistent across methods. 



If a *method-defining operator* that cannot specify *generic function* options creates a *generic function*, and if the *lambda list* for the method mentions keyword arguments, the *lambda list* of the generic function will mention **&amp;key** (but no keyword arguments). 



