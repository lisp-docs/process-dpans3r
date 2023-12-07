

(progn) *!* NIL 
(progn 1 2 3) *!* 3 
(progn (values 1 2 3)) *!* 1, 2, 3 
(setq a 1) *!* 1 
(if a 
    (progn (setq a nil) ’here) 
    (progn (setq a t) ’there)) *!* HERE 
a *!* NIL 
