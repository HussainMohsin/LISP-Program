;****************************************************************************
;The function  f1 takes a list and counts  the  number  of  lists  in  that  list
(defun F1 (L)
	(cond ((null L) 0) 			;if the list is empty return 0
		((atom (car L)) 		;check if it is an atom
			(F1 (cdr L)))		;if it an atom check the tail
		(T (+ 1 (F1 (cdr L))))	;if it is a list add 1
	)
)

;****************************************************************************
;The  function  f2  takes a list and  decides  whether  the  list has  an  atom  inside.
(defun F2 (L)
	(cond ((null L) nil)		;if the list is empty return nil
		((listp (car L)) 		;if the element in the list is a list skip
			(F2 (cdr L)))		;look thru the tail of the list
		(T T)					;else return true beacuse if it isnt a list it is an atom  
	)
)

;****************************************************************************
;The function f3 takes a list of integers and returns a list containing only odd
(defun F3 (L)
	(cond ((null L ) nil)				;if the list is empty return nil
		((evenp (car L))                ;if the atom is even skip
			(F3 (cdr L)))               ;look thru the tail of the list
		(T (cons (car L)(F3 (cdr L))))  ;else cons it to the list with only odds
	)
) 

;****************************************************************************
;The function  f4 takes a List and  returns  the  minimum  value  of  an  integer  list.
(defun F4 (L)
	(cond ((null (cdr L)) (car L))		;if there is only one element
	   ((> (F4(cdr L))(car L)) 			;if the cdr L is greater than car L
			(car L))					;get the head of the list
		(T (F4(cdr L)))					; else check the rest of the tail
	)
)
       
;****************************************************************************
;The function f5 takes a List and returns the reverse of the list
(defun F5 (L)  
	(cond ((null L) L)              			;if L is empty then reverse is empty
		(T (append (F5 (cdr L)) (list (car L))))  ;(list (car L))))
	)  												;else reverse cdr of L and append first		
)													;element of L at the end
;****************************************************************************
;The function  f6 takes a List and returns  a  list  containing  every  other  element  of  that  list
(defun F6 (L)
	(cond ((null L) nil)						;if the list is empty return nill
		(T (cons  (car L) (F6 (cdr (cdr L)))))	;combine car L  with the rest of the tail
	)
)

;****************************************************************************
;The function f7 takes a list and int atom and returns the element at a given location of a list.
(defun F7 (L  x)				
	(cond ((= x 0)(car L))			;if we are lookin at index 0, return the head of the List
		( T (F7 (cdr L) (- x 1)))	;else decrment the index until you get the to zero
	)
)

;****************************************************************************
;The function  f8 takes a list and returns  the  product  of  all  integers  everywhere  in  that  list.
(defun F8 (L)							
    (cond ((null L) 1)						;if the list is empty return 1
		((listp (car L)) 					;check if the element is a list
			( * (F8(car L))(F8(cdr L))))	;multiple each number in list then Muliple it with the num before it
        ( T ( * (car L) (F8 (cdr L))))		;else multiple it with the next number in the list
    )
)

;****************************************************************************
;The function is_member takes an atom and list, and decides whether x is member of L¬
(defun is_member (x L)				
	(cond ((null L) nil)			;if L is empty then x is not in L
		((equal x (car L)) T)		;if x is first element of L then x is in L
		(T (is_member x (cdr L)))   ;else check x is in cdr of L
	)
)

;****************************************************************************
;The function f9 takes a list and removes duplicates from that list
(defun F9 (L)
	(cond ((null L) L)					;check if the list is empty, return empty list
		((is_member (car L) (cdr L))	;if the head is a member of the tail
			(F9 (cdr L)))				;return only one of the duplicates
		(T (cons (car L) (F9 (cdr L))))	;else combine dont add the element to the list of duplicates
	)
)

;****************************************************************************
;The function f10 takes two lists and finds the intersection of two lists. 
(defun F10 (L I)
	(cond ( (null L) nil )					;if list L is empty return nill
		( (null n) nil )					;if list L is empty return nill
		(( is_member (car L) I)				;if a member of list L is in List I
			(cons (car L) (F10 (cdr L)I)))	;combine the element to the list
		(T (F10 (cdr L) I))					;else check thru the rest of the list
	)
)
