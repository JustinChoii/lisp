;; Justin Choi 40532889
;; Klefstad
;; CS 141 - Fall
;; Homework 4
;; 10/28/2018



;; Helper Functions

;; If it is a list, test the top element to see if it is an atom or another list.
(defun checkSublist(L)
	(cond	
		((null L) nil)
		;; if the first element is a list, then return true. otherwise, continue.
		((listp (car L)) t)
		(t (checkSublist (cdr L)))
		)
)


;; Count the occurences in a one-dimensional list.
(defun counter (A L)
	(cond 
		((null L) 0)
		((eq A (car L)) (+ 1 (counter A (cdr L))))
		(t (counter A (cdr L)))
		
	)
 )

;; Functions

(defun my-length (L)
	;; If the list is empty, then return null
    (cond
    	((null (car L)) 0)
    	;; Else, return x + 1 and recursively iterate through L, until it is null.
        (t (+ 1 (my-length (cdr L)) ))
    )
)


(defun my-memq (A L)
	;; if the list is empty, then return null.
	(cond
		((null L) nil)
		;; if the list is not empty, then:
		;; if the first element of L is equal to A, print the rest of the list
		((eq A (car L)) (cons A (cdr L)))
		
		;; Otherwise, recursively call my-memq with cdr L
		(t (my-memq A (cdr L)) )
		
	)
)



(defun my-append(L1 L2)
	(cond 
 		((null L1) L2)
 		((null L2) L1)
		;; Construct a list of L1[0] + L2 all the way until L1[n] + L2
		;; cons adds to the front of the list.
		;; cons L1 then my-append( (cdr L1) L2)
		(t (cons (car L1) (my-append (cdr L1) L2)))

	)
 )
 
 
 (defun my-attach (O L)
	(cond 
		((null O) L)
		((null L) (cons O nil))
		;; cons the list then O
		;; cons L1 then my-attach
		(t (cons (car L) (my-attach O (cdr L))))
	
	)
 
 )
(defun copy (x) 
	(cond 
		((atom x) x)
		(t (cons (copy (car x)) (copy (cdr x))))
	)
)

 
 
 (defun my-assoc (A L)
	(cond
		((null L) nil)
		((eq A (car(car L))) (car L))
		(t (my-assoc A (cdr L)))		
	)
 )
 
 (defun freq (A L)
	(cond 
		((null L) 0)
		((eq (checkSublist L) t) (+ 0 (freq A (cdr(car L)))))
		((eq A (car L)) (+ 2 (freq A (cdr L))))
		(t (freq A (cdr L)))
		
	)
 )
 
 
 (defun mapping (L val)
	(cond
		;((< (car (car L)) val) (my-append (cdr (car L)) ()))
		((null L) nil)
		((< (car(car L)) val) (my-append (cdr(car L)) (mapping (cdr L) val)))
		(t (mapping (cdr L)  val))
	
	
	)
 
 )
 
 
 (defun my-last (A L)
	;; we can use my-memq to search and return the list
	(cond
		;; if the list is empty, return nil
		((null L ) nil)
		
		;; Made a helper function that counts the occurences of a 1-Dimensional list called counter.
		;; If counter == 1, then run my-memq on the rest of the list.
		;; Otherwise, continue iterating through until we get to the last occurence.

		
		((eq (counter A L) 1) (my-memq A L))
		(t (my-last A (cdr L)))

	)
 
 )
 


 (defun my-reverse (L)
	(cond
		((eq (cdr L) nil) L)
		(t (my-attach (car L) (my-reverse (cdr L)) ))
	
	)
 
 )
 
 
 (defun first-atom (L)
	(cond
		((null L) nil)
		((atom (car L)) (car L))
		((listp (car L)) (first-atom (car L)))
	)
 )
 
