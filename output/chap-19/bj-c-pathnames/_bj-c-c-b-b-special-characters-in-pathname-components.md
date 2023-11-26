**19.2.2.1.1 Special Characters in Pathname Components** 

*Strings* in *pathname* component values never contain special *characters* that represent separation between *pathname* fields, such as *slash* in Unix *filenames*. Whether separator *characters* are permitted as part of a *string* in a *pathname* component is *implementation-defined*; however, if the *implementation* does permit it, it must arrange to properly “quote” the character for the *file system* when constructing a *namestring*. For example, 

;; In a TOPS-20 implementation, which uses \<i\>\<sup\>∧\</sup\>\</i\>V to quote 

(NAMESTRING (MAKE-PATHNAME :HOST "OZ" :NAME "\<TEST\>")) 

\<i\>→\</i\> #P"OZ:PS:\<i\>\<sup\>∧\</sup\>\</i\>V\<TEST\<i\>\<sup\>∧\</sup\>\</i\>V\>" 

\<i\>\<sup\>not\</sup\> →\</i\> #P"OZ:PS:\<TEST\>" 
