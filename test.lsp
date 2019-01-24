;;--------------------------------------------------------------------------------- 
;; This program for to finding progression criteria  
;; Calculation begin from SECOND element. Miminal size sequences is equal 4 elements.
;;
;; Onto original idea of A.P.Kiselev 
;;
;; Best regards, J/Z/Katsman (c) January.2019   
;; email:call89269081096@gmail.com
;; phone:89269081096
;;---------------------------------------------------------------------------------

;;--------------------------------------------------------------------------------- 
;; Build instruction:
;; 1) sbcl --load=quicklisp.lisp
;; 2) (require :sb-posix)
;; 3) (load "test.lsp")
;; 4) (save-core #p"test_compile6") 
;;--------------------------------------------------------------------------------- 

;;--------------------------------------------------------------------------------- 
;; Use instruction:
;; ./test_compile6  0, 1, 4, 16
;;--------------------------------------------------------------------------------- 
 
(defvar *hash-print* '(
	:print-may-be-geom-seq         " May be this seq. is (G)eometric progression with divergense series"
        :print-none-geom-prog-seq      " This is (N)ogeometric progression because i(0) == [0]"
        :print-arif-prog-seq           " This is (A)rithmetic progression"
        :print-vey-short-seq           " (U)nknow sequence (very short seq))"
        :print-unknow-seq              " (U)nknow sequence)"
        :print-geom-prog-seq           " This is (G)eometric  progression"
        :print-i-dont-know-seq         " I don's know what is this (not defined)"
	:print-is-nil-seq              " It's nil sequence (seq. not found)"
)) 
 
(defun  typeEmpty (a)
  (if (not (string-equal a "")) (return-from typeEmpty 1) (return-from typeEmpty nil)))

(deftype  checkEmptyString()
  `(satisfies typeEmpty))

;; Sort List for Arf.prog		
(defun quicksort (arrList) 
(if (null arrList) nil 
 (let* ((x (car arrList)) (r (cdr arrList)) (fn (lambda (a) (< a x)))) 
(append (quicksort (remove-if-not fn r)) (list x) (quicksort (remove-if fn r)))))
)  
  
;; Calc list's length
(defun calcLength(arrList)(length arrList))
;; Calc element's summ
(defun calcSumm(arrList)(reduce #'+ arrList))
;; Calc progression's denominator
;; 
(defun changeSignSeries (arrList)
  (list (reduce #'max arrList) (reduce #'min arrList))
)   

;;(format t "[ ~S ~S ]" arrList (changeSignSeries arrList))
;; first negative value /  first  positive value 
(defun denominatorp (arrList) 
(if ( and (< (nth 1 (changeSignSeries arrList)) 0) (> (nth 1 (changeSignSeries arrList)) 0))
(handler-case ( / (reduce #'min (changeSignSeries arrList)) (reduce #'max (changeSignSeries arrList)))  
(error ()(progn (format t " division 0 (1)")(quit))))
(handler-case ( / (nth 2 arrList) (nth 1 arrList))  
(error ()(progn(format t " division 0 (2)")(quit))))
))

;; Calc Ariphmetic summ
(defun ArfProgCalcSumm(arrList)  ( / ( * (+ (nth 0 arrList) (nth  ( - (calcLength arrList) 1 ) arrList)) (calcLength arrList)) 2))

(defun more_one(arrList)
(handler-case (/ (- (* (nth  (- (calcLength arrList) 1) arrList)  (denominatorp arrList)) (nth  0 arrList)) (- (denominatorp  arrList)  1))  
(error ()(progn(format t " division 0 (3)")(quit))))
)

(defun less_one(arrList)
(handler-case (/ (- (nth  0 arrList)  (* (nth  (- (calcLength arrList) 1) arrList) (denominatorp arrList))) ( - 1  (denominatorp arrList))) 
(error ()(progn(format t " division 0 (4)")(quit))))
)	   

;; Calc Geometric summ
(defun GeomProgCalcSumm(arrList)

;; if first item > last item   Descrising seq.
;; if first item < last item   Inscrising seq.	
(if (> (denominatorp arrList) 0)
	   (more_one arrList) 
       (less_one arrList)  
))

;;Split token with: comma(,)
(defun commaSplit (string)
  (loop for start = 0 then (1+ finish)
        for finish = (position #\, string :start start)
        collecting (subseq string start finish)
        until (null finish)))

;;Main loop for input-array
(defun splitString (arrList)
(loop
  for item in arrList
  collect (commaSplit item) 
))
	 
;;Convert nested-list to plain-list  
(defun convertToPlainList (arrList &optional acc) 
  (cond ((null arrList) acc)
        ((atom arrList) (cons arrList acc))
        ((convertToPlainList (car arrList) (convertToPlainList (cdr arrList) acc)))))

;;To remove not-alphabetic data element
(defun removeSpace (arrList)
(loop
  for item in arrList
  collect (typecase item (checkEmptyString item) (t nil))  
))     

;;Read string value from input for anyType number
(defun convertToNumber (arrList)
(loop
  for item in arrList
  collect  (with-input-from-string (in item) (read in))
))

;;print result
(defun printResult (printList)
(loop
  for item in printList
  do (format t " ~a " (getf *hash-print* item)) 
))

;;check criteria of progressions
(defun checkCriteria (arrList)
(let ((printList nil)) 
  (if (and (not (eq nil arrList)))
        (progn
		
		   (if ( < (length arrList) 3) 
		    (progn
		    (setq printList (append printList   (list ':print-vey-short-seq)))
            (quit)
		    )
		   )
		   (if ( and ( = (calcSumm arrList) (ArfProgCalcSumm arrList))                   
		       (not (or (and( = (calcSumm arrList) 0) ( = (ArfProgCalcSumm arrList) 0))  
		   	   (= (-  (nth 2 arrList) (nth 1 arrList)) 0))))
		   (setq printList (append printList   (list ':print-arif-prog-seq))))
           
	       (if (and ( = (calcSumm arrList) (GeomProgCalcSumm arrList)) (= (/ (nth 1 arrList)(nth 0 arrList)) (/ (nth 2 arrList)(nth 1 arrList))))
		   (setq printList (append printList   (list ':print-geom-prog-seq))))
		  
		   (if ( = (nth 0 (quicksort arrList)) 0) 
		   (setq printList (append printList   (list ':print-none-geom-prog-seq))))
		   
		   
		   ;; call print function 
		   (if (not ( = (length printList) 0)) (printResult printList))
		   (if ( = (length printList) 0) (printResult (list ':print-i-dont-know-seq)))
		  ) 
		 (printResult (list ':print-is-nil-seq))
		)))

(defun mainProcedure ()
     (checkCriteria 
     (remove nil 
     (convertToNumber
     (remove nil 
     (removeSpace
     (convertToPlainList 
     (splitString 
     (cdr sb-ext:*posix-argv*))))))))
)

(defun save-core (core-fn)
	(sb-ext:save-lisp-and-die core-fn :toplevel #'mainProcedure :executable t)    
)


