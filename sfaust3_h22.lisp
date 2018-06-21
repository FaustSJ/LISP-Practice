;Notes:
	;error: "an object cannot start with #\" means there's an extra ")"
	;(print (format t "~$" num))  to print a number
	;(print (format t "~{~a~^, ~}" list)) to print a list
    ;run in clisp with the command (load "sfaust3_h2.lisp")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;THESE ARE EXTRA METHODS THAT ARE USED THROUGHOUT ALL OTHER METHODS

;numOfElements is a function that returns the amount of elements in a given list
(defun numOfElements(lis)
	(if (null lis) 0
		(+ 1 (numOfElements (cdr lis)))))

;a lil' sum'sum to make things easier
(defun indexElem (cur i lis lisSize) ;returns the element in lis at index i
	(if (or (> cur i) (= cur lisSize)) (print "no can do!")
		(if (= cur i) (car lis) ;else if index is reached return the value
			(indexElem (+ cur 1) i (cdr lis) lisSize)))) ;else keep iterating

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;fib and its helper return the n'th value in the fibonacci sequence
(defun fib (n) 
	(cond ((= n 0) 0)	
		((or (= n 1) (= n 2)) 1) 	;checks if num is 0 1 or 2
		(t (fibHelper n 3 1 1))))	;for n>2 we require recursive calc
(defun fibHelper (n cnt twoFore oneFore);this recursively counts thru fib seq
	(if (>= cnt n);if n is reached
		(if (= (rem n 2) 0) oneFore (+ twoFore oneFore));then return a value
		(fibHelper n (+ cnt 2) (+ twoFore oneFore) (+ oneFore oneFore twoFore))));else, continue calculating
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;reversed and its helper return a reversed version of the given list
(defun reversed (xs)
	(let ((amouR (numOfElements xs)));how many elements does xs contain
	(if (= amouR 0) '()	;if xs is empty return empty list
		(reversedHelper xs '() amouR)))) ;else start the reversing loop
;the recursive helper method
(defun reversedHelper (from to cnt)
	(if (= cnt 0) to	;are we at the end of xs?
		(progn 
			(push (car from) to) ;else push next elem from xs to sxR
			(reversedHelper (cdr from) to (- cnt 1)))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;prime returns a bool statimg whether the given number is prime or not
(defun is_prime (n) ;we knock out any easy non-prime numbers...
	(if (<= n 1) nil
		(if (<= n 3) t
			(if (or (= (mod n 2) 0) (= (mod n 3) 0)) nil
					(primeHelper n 5))))) ;...then we check closer
;run through numbers most likely to be prime and check
	;if n is divisable by them.
(defun primeHelper (num iter)
		(if (> (* iter iter) num) t
			(if (= (mod num iter) 0) nil
				(if (= (mod num (+ iter 2)) 0) nil
					(primeHelper num (+ iter 6))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;nub returns the list xs without any duplicate elements
(defun nub (xs) 
	(let ((noDupN '())	;the list that will contaim no duplicates
		(amouN (numOfElements xs)));number of elements in xs	
	(if (= amouN 0) noDupN	;if xs is empty return an empty list
		(progn
			(push (car xs) noDupN) ;else push in the first element
			(nubHelper (cdr xs) noDupN amouN)))));and call recursive check
;the recursive helper function
(defun nubHelper (from to cnt)
	(if (= cnt 0) (remove nil (reversed to)) ;is xs has no more elements return noDupN
		(progn ;else
			(if (not (member (car from) to)) ;is the cur elem not in noDupN?
				(push (car from) to)) ;add cur elem to noDupN
			(nubHelper (cdr from) to (- cnt 1))))) ;check next elem

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;zip the elements from xs and ys together with the given function
(defun zip_with (f xs ys)
	(if (< (numOfElements ys) (numOfElements xs))	;grabs the smallest list amount
		(progn ;then
			(let ((indexerZ (numOfElements ys))
				(resultZ '()))
			(if (= indexerZ 0) resultZ ;if one list is empty return empty list
				(progn
					(push (funcall f (car xs) (car ys)) resultZ) ;else transfer the first elements
					(zip_withHelper f (cdr xs) (cdr ys) resultZ (- indexerZ 1))))));and recurse
		(progn ;else
			(let ((indexerZ (numOfElements xs))
				(resultZ '()))
			(if (= indexerZ 0) resultZ ;if one list is empty return empty list
				(progn
					(push (funcall f (car xs) (car ys)) resultZ) ;else transfer the first elements
					(zip_withHelper f (cdr xs) (cdr ys) resultZ (- indexerZ 1))))))));and recurse
			
;the recursive helper function
(defun zip_withHelper (somf fromx fromy to cnt)
	(if (= cnt 0) (reversed to)
		(progn
			(push (funcall somf (car fromx) (car fromy)) to)
			(zip_withHelper somf (cdr fromx) (cdr fromy) to (- cnt 1)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;creates a sequence of numbers from n down to 1 following a set of rules
(defun collatz (n)
	(if (= n 0) '(0)  ;if n=0 return alist of just 0
		(collatzHelper n (list (+ 0 n))))) ;else start the sequence calculation
;the recursive helper function
(defun collatzHelper (num lis)
	(if (= num 1) (reversed lis) ;is n at 1 yet? if so return the sequence
		(if (= (rem num 2) 0) ;if num is even next num is n/2
			(progn
				(let ((newNumC (/ num 2)))
				(push newNumC lis)
				(collatzHelper newNumC lis)))
			(progn
				(let ((newNumC (+ (* num 3) 1))) ;else next num is n*3+1
				(push newNumC lis)
				(collatzHelper newNumC lis))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;we start by declaring variables that will be shared among the mode methods
(defvar noDupm '())
(defvar modem '())
(defvar foundm 0)
;this is the main function that finds the mean med and mode of a given list
(defun list_report (xs)
	(let ((xs (sort xs #'<)))
	(if (= 0 (numOfElements xs)) '()
		(if (= 1 (numOfElements xs)) (list (car xs) (car xs) (car xs))
			(list (/ (mean xs) (numOfElements xs)) (median xs) (mode xs))))))
;finds the mean
(defun mean (xs)
	(if (null xs) 0.0
		(float (+ (car xs) (mean (cdr xs))))))
;finds the median
(defun median (xs)
	(let ((sizeMED (numOfElements xs)))
	(if (= (rem sizeMED 2) 0);if the list is even
		(progn ;then
			(let ((xMED (indexElem 0 (/ sizeMED 2) xs sizeMED))
				(yMED (indexElem 0 (- (/ sizeMED 2) 1) xs sizeMED)))
			(float (/ (+ xMED yMED) 2))))
		(float (indexElem 0 (- (/ sizeMED 2) 0.5) xs sizeMED)))));else return the middle-most num	
;mode finds the mode using two recursive helpers and the above global variables
(defun mode (xs)
	(setq modem '())
	(setq noDupm '())
	(let ((sizeMO (numOfElements xs)));stores the sizeMO of the given list
	(modeHelperOuter 0 sizeMO xs);calls the first recursive method
	(if (= (numOfElements modem) 0) (setf modem (reversed noDupm)) (reversed modem))))
;the first mode helper loops through xs
(defun modeHelperOuter (icount size xs)
	(if (= icount size) (setq modem (nub modem));if loop is done return nub'ed modem list
		(progn
			(setq foundm 0);else... sets the foundm global variable to false
			(let ((ndSizeMO (numofElements noDupm)));defines the size of noDup list
			(if (> ndSizeMO 0) (modeHelperInner 0 icount size ndSizeMO xs)))
			(if (= foundm 0) (push (indexElem 0 icount xs size) noDupm))
			(modeHelperOuter (+ icount 1) size xs))))
;the second mode helper loops through noDupm
(defun modeHelperInner (kcount icount xsSize ndSize xs)
	(if (< kcount ndSize) ;while kcount<noDupm's size
		(if (= (indexElem 0 icount xs xsSize) (indexElem 0 kcount noDupm ndSize));xs[i] in nd? 
			(progn
				(push (indexElem 0 icount xs xsSize) modem);if yes its a duplicate
				(setf foundm 1);change foundm to let outer method know
				(modeHelperInner (+ kcount 1) icount xsSize ndSize xs))
			(modeHelperInner (+ kcount 1) icount xsSize ndSize xs))));recursion!


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun check_sudoku (grid)
	;return rowCheck ^ colCheck ^ boxCheck
	(and (checkRows 9 grid) 	;9 b/c counting elements
		(checkCols 8 grid)			;8 b/c checking indexes
		(checkBoxes 9 grid)))		;9 b/c checking 9 boxes

;checking rows
(defun checkRows (cnt sudo)
	(let ((cRow (nub (car sudo))));grab the next row
	(if (<= cnt 0) t;if all rows were checked return t
		(if (< (numOfElements cRow) 9) nil ;if duplicates found then F
				(checkRows (- cnt 1) (cdr sudo)))))) ;else next row

;checking columns
(defun checkCols (cnti sudo)
	(if (< cnti 0) t;if all cols were checked return t
		(progn
			(let ((curLis (colsHelper cnti 0 '() sudo)))
			(if (< (numOfElements (nub curLis)) 9) nil
				(checkCols (- cnti 1) sudo))))))
;since it's a nested loop, the helper needs a helper
(defun colsHelper (cnti cntj lis sudo)
	(if (= cntj 9) lis
		(progn
		(push (indexElem 0 cnti (indexElem 0 cntj sudo 9) 9) lis)
		(colsHelper cnti (+ cntj 1) lis sudo))))

;Checks the sudoku 3x3 boxes
(defun checkBoxes (cntb sudo)
	(if (= cntb 0) t;if all rows were checked return t
		(cond ((= cntb 1) (let ((curBox (theMainLoop 0 3 0 3 '() sudo)))
					(let ((checkBox (nub curBox)))
					(if (< (numOfElements checkBox) (numOfElements curBox)) nil
							(checkBoxes (- cntb 1) sudo)))));check next box
			((= cntb 2) (let ((curBox (theMainLoop 0 3 3 6 '() sudo)))
					(let ((checkBox (nub curBox)))
					(if (< (numOfElements checkBox) (numOfElements curBox)) nil
							(checkBoxes (- cntb 1) sudo)))));check next box
			((= cntb 3) (let ((curBox (theMainLoop 0 3 6 9 '() sudo)))
					(let ((checkBox (nub curBox)))
					(if (< (numOfElements checkBox) (numOfElements curBox)) nil
							(checkBoxes (- cntb 1) sudo)))));check next box
			((= cntb 4) (let ((curBox (theMainLoop 3 6 0 3 '() sudo)))
					(let ((checkBox (nub curBox)))
					(if (< (numOfElements checkBox) (numOfElements curBox)) nil
							(checkBoxes (- cntb 1) sudo)))));check next box
			((= cntb 5) (let ((curBox (theMainLoop 3 6 3 6 '() sudo)))
					(let ((checkBox (nub curBox)))
					(if (< (numOfElements checkBox) (numOfElements curBox)) nil
							(checkBoxes (- cntb 1) sudo)))));check next box
			((= cntb 6) (let ((curBox (theMainLoop 3 6 6 9 '() sudo)))
					(let ((checkBox (nub curBox)))
					(if (< (numOfElements checkBox) (numOfElements curBox)) nil
							(checkBoxes (- cntb 1) sudo)))));check next box
			((= cntb 7) (let ((curBox (theMainLoop 6 9 0 3 '() sudo)))
					(let ((checkBox (nub curBox)))
					(if (< (numOfElements checkBox) (numOfElements curBox)) nil
							(checkBoxes (- cntb 1) sudo)))));check next box
			((= cntb 8) (let ((curBox (theMainLoop 6 9 3 6 '() sudo)))
					(let ((checkBox (nub curBox)))
					(if (< (numOfElements checkBox) (numOfElements curBox)) nil
							(checkBoxes (- cntb 1) sudo)))));check next box
			((= cntb 9) (let ((curBox (theMainLoop 6 9 6 9 '() sudo)))
					(let ((checkBox (nub curBox)))
					(if (< (numOfElements checkBox) (numOfElements curBox)) nil
							(checkBoxes (- cntb 1) sudo)))));check next box
			(t '()))));in error, an empty list is returned
;This reduces three recursive methods to one. 
;this builds up an array of the nums in a 3x3 sudoku box for checkBoxes to check
(defun theMainLoop (jfrom jto kfrom kto curBox sudo)
	(if (>= jfrom jto) curBox
		(if (>= kfrom kto) (theMainLoop (+ jfrom 1) jto (- kfrom 3) kto curBox sudo)
			(progn
				(push (indexElem 0 jfrom (indexElem 0 kfrom sudo 9) 9) curBox)
				(theMainLoop jfrom jto (+ kfrom 1) kto curBox sudo)))))




