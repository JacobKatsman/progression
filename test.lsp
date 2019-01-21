;;--------------------------------------------------------------------------------- 
;; This program for to finding progression criteria  
;; Req: Calculation begin from SECOND element. Miminal size sequences is equal 4 elements.
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
 
 
(defvar *reslist* '())

(defun  typeEmpty (a)
  (if (not (string-equal a "")) (return-from typeEmpty 1) (return-from typeEmpty nil)))

(deftype  checkEmptyString()
  `(satisfies typeEmpty))


;; list's length
(defun calcLength(arrList)(length arrList))
;; element's summ
(defun calcSumm(arrList)(reduce #'+ arrList))
;; progression denominator
(defun denominatorProg(arrList) ( / (nth 1 arrList) (nth 0 arrList)))

;; Ariphmetic
(defun ArfProgCalcSumm(arrList)  ( / ( * (+ (nth 0 arrList) (nth( - (calcLength arrList) 1 ) arrList)) (calcLength arrList)) 2))
;; Geometric
(defun GeomProgCalcSumm(arrList)
    (abs (/ (-  (*  (nth( - (calcLength arrList) 1 ) arrList)  (denominatorProg arrList)) (nth 0 arrList)) 
       (- 1  (denominatorProg arrList))
    ))   
    )

;;check criteria of progressions
(defun checkCriteria (arrList)
  (let ((arrList (sort arrList  #'<)))
    (cond ((= (calcSumm (cdr arrList)) (ArfProgCalcSumm (cdr arrList)))                   ( format t "~% This is (A)rithmetic progression ~%" ))
          ((or(< (nth 1 arrList) 2 ) (< (length arrList)  4))                             ( format t "~% (U)nknow sequences (very short seq)) ~%" ))  
	  ((= (calcSumm (cdr arrList)) (GeomProgCalcSumm (cdr arrList)))                  ( format t "~% This is (G)eometric  progression ~%" ))
          ( t ( format t "(U)nknow sequences ~%" ))
	)
  )
)

;; split token with div. symbol: comma
(defun commaSplit (string)
  (loop for start = 0 then (1+ finish)
        for finish = (position #\, string :start start)
        collecting (subseq string start finish)
        until (null finish)))

;; loop for array
(defun listExcludeType (arrList)
     (loop
      for item in arrList
      collect (commaSplit item) 
     )
     )
;;nested list to plain list transform 
(defun flat (arrList &optional acc) 
  (cond ((null arrList) acc)
        ((atom arrList) (cons arrList acc))
        ((flat (car arrList) (flat (cdr arrList) acc)))))

;;The change weird symbols to nil 
(defun removeNonnumericList (arrList)
  (when arrList
   (progn 
      (typecase (car arrList)
       (checkEmptyString
       (progn
	(push *reslist* (car arrList))
	(removeNonnumericList (cdr arrList))
	)
       )
      )
     (removeNonnumericList (cdr arrList))
   )
  )
)
;;To remove not alphabetic data
(defun spaceRemove (arrList)
     (loop
      for item in arrList
      collect (typecase item (checkEmptyString item) (t nil))  
	  )
)     

;;convert value to integer
(defun resultRemove (arrList)
     (loop
      for item in arrList
      collect (parse-integer (remove #\Space item)  :junk-allowed t)
	  )
)

(defun mainProcedure ()
  (checkCriteria (remove nil (resultRemove (remove nil (spaceRemove (flat (listExcludeType (cdr sb-ext:*posix-argv*))))))))
)

(defun save-core (core-fn)
	(sb-ext:save-lisp-and-die core-fn :toplevel #'mainProcedure :executable t)    
)

