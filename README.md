# progression
To test sequence for math.progression criteria
  
  This program for to finding progression criteria  
  
  

  Onto original idea of A.P.Kiselev:) 

   
  email:call89269081096@gmail.com
 

  
 Build instruction:
 1) sbcl --load=quicklisp.lisp
 2) (require :sb-posix)
 3) (load "test.lsp")
 4) (save-core #p"test_compile6") 
 

 
 Use instruction:
 ./test_compile6  0, 1, 4, 16, 32
 
  ./test_compile6 50,-25,12.5,-6.25,3.125

 arrlist (50.0 -25.0 12.5 -6.25 3.125)  deno: -0.5  nth 50.0  nth -1.5625

 This is (G)eometric  progression
 
 $ ./test_compile6   -1, -2, -4, -8, -16

 arrlist (-1.0 -2.0 -4.0 -8.0 -16.0)  deno: 2.0  nth -1.0  nth -32.0

 This is (G)eometric  progression
 
 $ ./test_compile6  0, 1, 2, 4, 8, 16, 32

 arrlist (1.0 2.0 4.0 8.0 16.0 32.0)  deno: 2.0  nth 1.0  nth 64.0

 This is (G)eometric  progression
 
 
$ ./test_compile6 3.5,5,6.5,8,9.5

This is (A)rithmetic progression

$ ./test_compile6 2,6,18,54

 arrlist (2.0 6.0 18.0 54.0)  deno: 3.0  nth 2.0  nth 162.0
 
$ ./test_compile6 1,-1,1,-1,1,-1,1
This is (G)eometric  progression 1.0 -1.0


$ ./test_compile6 1,-1,1,-1,1,-1,1,-1
May be this seq. is (G)eometric progression with divergense series 

 This is (G)eometric  progression
 
 Best regards, J/Z/Katsman (c) January.2019 
 
