 

**Syntax:** 

**clrhash** *hash-table → hash-table* 

**Arguments and Values:** 

*hash-table*—a *hash table*. 



 

 

**Description:** 

Removes all entries from *hash-table*, and then returns that empty *hash table*. 

**Examples:** 

(setq table (make-hash-table)) *→* #&#60;HASH-TABLE EQL 0/120 32004073&#62; 

(dotimes (i 100) (setf (gethash i table) (format nil "~R" i))) *→* NIL 

(hash-table-count table) *→* 100 

(gethash 57 table) *→* "fifty-seven", *true* 

(clrhash table) *→* #&#60;HASH-TABLE EQL 0/120 32004073&#62; 

(hash-table-count table) *→* 0 

(gethash 57 table) *→* NIL, *false* 

**Side Effects:** 

The *hash-table* is modified. 

