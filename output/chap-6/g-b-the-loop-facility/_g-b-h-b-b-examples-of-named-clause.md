**6.1.7.1.1 Examples of NAMED clause** 

;; Just name and return. 

(loop named max 

for i from 1 to 10 

do (print i) 

do (return-from max ’done)) 

&#9655; 1 

*→* DONE 
