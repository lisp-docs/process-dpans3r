 

**Syntax:** 

**clear-input** &optional *input-stream →* **nil** 

**Arguments and Values:** 

*input-stream*—an *input stream designator* . The default is *standard input*. 

**Description:** 

Clears any available input from *input-stream*. 

If **clear-input** does not make sense for *input-stream*, then **clear-input** does nothing. **Examples:** 

;; The exact I/O behavior of this example might vary from implementation 

;; to implementation depending on the kind of interactive buffering that 

;; occurs. (The call to SLEEP here is intended to help even out the 

;; differences in implementations which do not do line-at-a-time buffering.) 

(defun read-sleepily (&optional (clear-p nil) (zzz 0)) 

(list (progn (print ’&#62;) (read)) 

;; Note that input typed within the first ZZZ seconds 

;; will be discarded. 

(progn (print ’&#62;) 

(if zzz (sleep zzz)) 

(print ’») 

(if clear-p (clear-input)) 

(read)))) 

(read-sleepily) 

&#9655; &#62; 10 

&#9655; &#62; 

&#9655; » 20 

*→* (10 20) 

(read-sleepily t) 

&#9655; &#62; 10 

&#9655; &#62; 

&#9655; » 20 

*→* (10 20) 

(read-sleepily t 10) 

&#9655; &#62; 10 

&#9655; &#62; 20 ; Some implementations won’t echo typeahead here. 



 

 

&#9655; » 30 

*→* (10 30) 

**Side Effects:** 

The *input-stream* is modified. 

**Affected By:** 

**\*standard-input\*** 

**Exceptional Situations:** 

Should signal an error of *type* **type-error** if *input-stream* is not a *stream designator* . 

**See Also:** 

**clear-output** 

