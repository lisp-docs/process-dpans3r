 

**Syntax:** 

**hash-table-p** *object → generalized-boolean* 

**Arguments and Values:** 

*object*—an *object*. 

*generalized-boolean*—a *generalized boolean*. 

**Description:** 

Returns *true* if *object* is of *type* **hash-table**; otherwise, returns *false*. 

Hash 

 

 

**Examples:** 

(setq table (make-hash-table)) *→* #&#60;HASH-TABLE EQL 0/120 32511220&#62; 

(hash-table-p table) *→ true* 

(hash-table-p 37) *→ false* 

(hash-table-p ’((a . 1) (b . 2))) *→ false* 

**Notes:** 

(hash-table-p *object*) *≡* (typep *object* ’hash-table) 

