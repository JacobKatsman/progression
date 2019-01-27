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
(defvar *arrList* '()) 
 
(defun  typeEmpty (a)
  (if (not (string-equal a "")) (return-from typeEmpty 1) (return-from typeEmpty nil)))

(deftype  checkEmptyString()
  `(satisfies typeEmpty))
  
;; cutting
(defun round-to (number precision &optional (what #'round))
    (let ((div (expt 10 precision)))
         (/ (funcall what (* number div)) div)))

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

;;-----------

(defun denominator_arf (arrList)
   (if ( > (length arrList) 1) 
   (- (nth 1 arrList) (nth 0 arrList))))

(defun denominator_a (arrList)
 (car (mapcar #'denominator_arf (mapcon #'list arrList))))
 

(defun denomExclude (arrItem)
 (if ( > (length arrItem) 1)
  (if 
   (= ( - (nth 1 arrItem) (nth 0 arrItem)) ( denominator_a *arrList*)) 
   (list  (nth 0 arrItem)) 
   (list 0)
  )(list (nth 0 arrItem)))) 

(defun denom (arrList)
 (mapcon #'denomExclude arrList))

(defun checkArf(arrList)
(if  (= (length (remove 0 (denom arrList)))  (length (remove 0 arrList)))
  1
  0 ))
;;----------- 
 
;;------------
(defun denominator_geo (arrList)
   (if ( > (length arrList) 1)
   (if( and (not( = (nth 0 arrList) 0)) (not( = (nth 1 arrList) 0)))  (/ (nth 1 arrList) (nth 0 arrList)) 0) 
   0 ))

(defun denominator_g (arrList)
 (car (remove 0 (mapcar #'denominator_geo (mapcon #'list arrList))))) 

(defun summgeo(x y)
(let ((denominator_g1 (denominator_g *arrList*)))
 (progn
 (if (not (= x 0))
 (if (< (abs (- (/  y  x)  denominator_g1)) 0.0000001 ) y 0)
 0 ))))

(defun checkGeo(arrlist)
 
 (if (< ( abs ( - (reduce #'summgeo arrList) (nth (- (length arrList) 1 ) arrList))) 0.0000001)
  1
  0 ))

;;---------------
(defun checkCriteria (arrList)
  (progn 
  (setf *arrList* arrList) 
  (checkGeo *arrList*)
  (checkArf *arrList*)
  (if (and  (= (checkGeo *arrList*) 0) (= (checkArf *arrList*) 0))
   (format t "(U)nknow sequence"))
  
  (if (= (checkGeo *arrList*) 1)
   (format t "(G)eometric progression ~%"))
  (if (= (checkArf *arrList*) 1)
   (format t "(A)rfmetic progression ~%"))))
;;----------------

(defun mainProcedure ()
     (checkCriteria 
     (remove nil 
     (convertToNumber
     (remove nil 
     (removeSpace
     (convertToPlainList 
     (splitString 
     (cdr sb-ext:*posix-argv*)))))))))

(defun save-core (core-fn)
	(sb-ext:save-lisp-and-die core-fn :toplevel #'mainProcedure :executable t))


