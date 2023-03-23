module PrettyPrint where

import IR

printFunctions :: [Function] -> IO()
printFunctions [] = print "END"
printFunctions ((Function name params code):funcs) =
    do  print name
        print params
        mapM_ print code
        printFunctions funcs

printProg :: Prog -> IO()
printProg (Prog id funcs) =
    do  print ("PROGRAM: " ++ id)
        printFunctions funcs