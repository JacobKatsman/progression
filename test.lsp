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
 
(defun  typeEmpty (a)
  (if (not (string-equal a "")) (return-from typeEmpty 1) (return-from typeEmpty nil)))

(deftype  checkEmptyString()
  `(satisfies typeEmpty))

;; Calc list's length
(defun calcLength(arrList)(length arrList))
;; Calc element's summ
(defun calcSumm(arrList)(reduce #'+ arrList))
;; Calc progression's denominator
(defun denominatorProg(arrList) ( / (nth 1 arrList) (nth 0 arrList)))
;; Calc Ariphmetic summ
(defun ArfProgCalcSumm(arrList)  ( / ( * (+ (nth 0 arrList) (nth( - (calcLength arrList) 1 ) arrList)) (calcLength arrList)) 2))
;; Calc Geometric summ
(defun GeomProgCalcSumm(arrList)
    (abs (/ (-  (*  (nth( - (calcLength arrList) 1 ) arrList)  (denominatorProg arrList)) (nth 0 arrList)) 
       (- 1  (denominatorProg arrList))
    )))
	
 
;;check criteria of progressions
(defun checkCriteria (arrList)
  (if ( and (not (eq nil arrList))  (= (length (remove-duplicates arrList)) (calcLength arrList))) 
   (let ((arrList (sort arrList  #'<)))
    (cond ((= (calcSumm arrList) (ArfProgCalcSumm arrList))                   
	       ( format t "~% This is (A)rithmetic progression ~%" ))
		   
          ((or(< (nth 1 arrList) 2 ) (< (length arrList)  4))                             
		  ( format t "~% (U)nknow sequence (very short seq)) ~%" ))
		  
	      ((= (calcSumm arrList) (GeomProgCalcSumm arrList))                  
		  ( format t "~% This is (G)eometric  progression ~%" ))
          ( t ( format t "(U)nknow sequence ~%" ))
	)
   )
   (format t "It's null sequence (symbol not found) or we are find more one duplicate values")
  )
)

;;https://common-lisp.net/project/bese/docs/arnesi/html/api/function_005FIT.BESE.ARNESI_003A_003APARSE-FLOAT.html 
(defun radix-values (radix)
  (assert (<= 2 radix 35)
          (radix)
          "RADIX must be between 2 and 35 (inclusive), not ~D." radix)
  (make-array radix
              :displaced-to "0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ"
              :displaced-index-offset 0
              :element-type 'character))
			  

(defun parse-float (float-string
                    &key (start 0) (end nil) (radix 10)
                         (junk-allowed t)
                         (type 'single-float)
                         (decimal-character #\.))
  (let ((radix-array (radix-values radix))
        (integer-part 0)
        (mantissa 0)
        (mantissa-size 1)
        (sign 1))
    (with-input-from-string (float-stream (string-upcase float-string) :start start :end end)
      (labels ((peek () (peek-char nil float-stream nil nil nil))
               (next () (read-char float-stream nil nil nil))
               (sign () ;; reads the (optional) sign of the number
                 (cond
                   ((char= (peek) #\+) (next) (setf sign 1))
                   ((char= (peek) #\-) (next) (setf sign -1)))
                 (integer-part))
               (integer-part ()
                 (cond
                   ((position (peek) radix-array)
                    ;; the next char is a valid char
                    (setf integer-part (+ (* integer-part radix)
                                          (position (next) radix-array)))
                    ;; again
                    (return-from integer-part (integer-part)))
                   ((null (peek))
                    ;; end of string
                    (done))
                   ((char= decimal-character (peek))
                    ;; the decimal seperator
                    (next)
                    (return-from integer-part (mantissa)))                   
                   ;; junk
                   (junk-allowed (done))
                   (t (bad-string))))
               (mantissa ()                 
                 (cond
                   ((position (peek) radix-array)
                    (setf mantissa (+ (* mantissa radix)
                                      (position (next) radix-array))
                          mantissa-size (* mantissa-size radix))
                    (return-from mantissa
                      (mantissa)))
                   ((or (null (peek)) junk-allowed)
                    ;; end of string
                    (done))
                   (t (bad-string))))
               (bad-string ()
                 (error "Unable to parse ~S." float-string))
               (done ()
                 (return-from parse-float
                   (coerce (* sign (+ integer-part (/ mantissa mantissa-size))) type))))
        (sign)))))
		
		

;;Split token with: comma(,)
(defun commaSplit (string)
  (loop for start = 0 then (1+ finish)
        for finish = (position #\, string :start start)
        collecting (subseq string start finish)
        until (null finish)))

;;Main loop for input-array
(defun listExcludeType (arrList)
(loop
  for item in arrList
  collect (commaSplit item) 
))
	 
;;Convert nested-list to plain-list  
(defun flat (arrList &optional acc) 
  (cond ((null arrList) acc)
        ((atom arrList) (cons arrList acc))
        ((flat (car arrList) (flat (cdr arrList) acc)))))

;;To remove not-alphabetic data
(defun spaceRemove (arrList)
(loop
  for item in arrList
  collect (typecase item (checkEmptyString item) (t nil))  
))     

;;Convert string value to integer
(defun resultRemove (arrList)
(loop
  for item in arrList
  ;; work in Real val.
  collect (parse-float (remove #\Space item)  :junk-allowed t)
))

;;(defun mainProcedure ()
;;  (format t "~a"	(remove 0.0
;;    (remove nil 
;;    (resultRemove 
;;    (remove nil 
;;    (spaceRemove 
;;    (flat 
;;    (listExcludeType 
;;    (cdr sb-ext:*posix-argv*)))))))))
;;   )

(defun mainProcedure ()
    (checkCriteria 
	  (remove 0.0
    (remove nil 
    (resultRemove 
    (remove nil 
    (spaceRemove 
    (flat 
    (listExcludeType 
    (cdr sb-ext:*posix-argv*))))))))
	)
)

(defun save-core (core-fn)
	(sb-ext:save-lisp-and-die core-fn :toplevel #'mainProcedure :executable t)    
)
