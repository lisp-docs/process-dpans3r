 



Note that *conforming code* may rely on particular *implementation-defined* values or features. Also note that the requirements for *conforming code* and *conforming implementations* do not require that the results produced by conforming code always be the same when processed by a *conforming implementation*. The results may be the same, or they may differ. 



Conforming code may run in all conforming implementations, but might have allowable *implementation-defined* behavior that makes it non-portable code. For example, the following are examples of *forms* that are conforming, but that might return different *values* in different implementations: 



(evenp most-positive-fixnum) *→ implementation-dependent* 



(random) *→ implementation-dependent* 



(> lambda-parameters-limit 93) *→ implementation-dependent* 



(char-name #\A) *→ implementation-dependent* 



