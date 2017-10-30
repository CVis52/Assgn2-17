module AlphaNamer where

--------------------------------------------------------
-- The module to alpha-rename variables and functions --
-- Author: Clayton Vis -- UCID: 30015308 -- CPSC 521  --
--------------------------------------------------------


import ParseProg
import AST


data ST = ST ([(String, Int)], [(String, Int)], Int, Int)
    deriving Show

addFun :: ST -> String -> ST
addFun (ST (fcts, vars, fn, vn)) name = ST( ([(name, fn)] ++ fcts), vars, (fn + 1), vn)

addVar :: ST -> [String] -> ST
addVar syms [] = syms
addVar (ST (fcts, vars, fn, vn)) (n:names) = 
                        addVar (ST (fcts, ([(n,vn)] ++ vars), fn, (vn + 1))) names

getFuns :: ST -> [(String, Int)]
getFuns (ST (fcts, _, _, _)) = fcts

getVars :: ST -> [(String, Int)]
getVars (ST (_, vars, _, _)) = vars

getArgs :: [(String, Int)] -> [String] -> [String]
getArgs ls [] = []
getArgs ls (a:as) = ("x" ++ show(getPair' ls a 0)) : (getArgs ls as)

getPair :: [(String, Int)] -> String -> Int
getPair ls name = getPair' ls name 0
getPair' [] _ n = n
getPair' ((str, num):ls) name n 
                                | str == name && num > n        = getPair' ls name num
                                | otherwise                     = getPair' ls name n

----------- FOR TESTING PURPOSES -----------------
firstFun :: AST.Prog a b -> AST.Fun a b         --
firstFun (Prog (f:fs)) = f                      --
                                                --
firstExp :: AST.Fun a b -> AST.Exp a b          --
firstExp (Fun (name, args, expr)) = expr        --
                                                --
stripExp :: AST.Exp a b -> AST.Exp a b          --
stripExp (LET (f:fs) expr) = firstExp f         --
stripExp (ADD expr1 expr2) = expr1              --
                                                --
stripLET :: AST.Exp a b -> AST.Fun a b          --
stripLET (LET (f:fs) expr) = f                  --
--------------------------------------------------

--data Exp a b = ADD (Exp a b)(Exp a b) | SUB (Exp a b)(Exp a b) 
--             | MUL (Exp a b)(Exp a b) | DIV (Exp a b)(Exp a b)
--             | NEG (Exp a b)    | CONST Int     | VAR b
--             | APP a [(Exp a b)]      | LET [Fun a b] (Exp a b)
--foldExp :: ((AST.Exp c d) -> b -> b) -> (d -> b) -> AST.Exp c d -> b
--foldExp f v (VAR b) = v b
--foldExp f v (ADD expr1 expr2) = 
--foldExp f v (ADD expr1 expr2) = (ADD(foldExp f v expr1)(foldExp f v expr2))


--NOT FINISHED
foldExp :: Show a => Show b => (c -> c -> c) -> (c -> c -> c) -> (b -> c) -> (Exp a b) -> c
foldExp af sf v (VAR b) = v b
foldExp af sf v (ADD expr1 expr2) = af (foldExp af sf v expr1) (foldExp af sf v expr2)
foldExp af sf v (SUB expr1 expr2) = sf (foldExp af sf v expr1) (foldExp af sf v expr2)
--NOT FINISHING THIS, WILL BE UGLY AND NOT WORTH IT

myFunFunc :: (AST.Fun String String) -> ST -> (AST.Fun String String)
myFunFunc (Fun (name, args, expr)) sym = Fun (aName, aArgs, aExpr)
        where
            newTable = addVar (addFun sym name) args
            aName = "f" ++ show(getPair (getFuns newTable) name)
            aArgs = getArgs (getVars newTable) args
            aExpr = stripST $ myExpFunc expr newTable


myFunFunc' :: [AST.Fun String String] -> ST -> [AST.Fun String String]
myFunFunc' [] sym = []
myFunFunc' (f:funs) sym = (myFunFunc f sym) : (myFunFunc' funs sym)

myExpFunc :: (AST.Exp String String) -> ST -> ((AST.Exp String String), ST)
myExpFunc (ADD expr1 expr2) sym = ((ADD (stripST $ myExpFunc expr1 sym) (stripST $ myExpFunc expr2 sym)), sym)
myExpFunc (SUB expr1 expr2) sym = ((SUB (stripST $ myExpFunc expr1 sym) (stripST $ myExpFunc expr2 sym)), sym)
myExpFunc (MUL expr1 expr2) sym = ((MUL (stripST $ myExpFunc expr1 sym) (stripST $ myExpFunc expr2 sym)), sym)
myExpFunc (DIV expr1 expr2) sym = ((DIV (stripST $ myExpFunc expr1 sym) (stripST $ myExpFunc expr2 sym)), sym)
myExpFunc (NEG expr1) sym = ((NEG (stripST $ myExpFunc expr1 sym)), sym)
myExpFunc (CONST i) sym = ((CONST i), sym)
myExpFunc (VAR b) sym = ((VAR ("x" ++ show(getPair (getVars sym) b))), sym)
myExpFunc (APP a exprs) sym = ((APP a (myExpFunc' exprs sym)), sym)
myExpFunc (LET fns expr1) sym = ((LET (myFunFunc' fns sym) (stripST $ myExpFunc expr1 sym)), sym)

myExpFunc' :: [AST.Exp String String] -> ST -> [AST.Exp String String]
myExpFunc' [] sym = []
myExpFunc' (expr:exprs) sym = (stripST $ myExpFunc expr sym) : (myExpFunc' exprs sym)


stripST :: ((AST.Exp String String), ST) -> (AST.Exp String String)
stripST (expr, sym) = expr
--data Fun a b = Fun (a, [b], Exp a b)
--foldFun :: (a -> b -> b) -> b -> (AST.Fun c d) -> b
--myFunFold f accum func = f 

--myFunFold f accum symTbl (Fun (name, args, expr)) = 
--                    (Fun (alphaName, alphaArgs, alphaExpr))
--                        where
--                            alphaName = rename 



main = do
        let fname = "testFile1"
        conts <- readFile fname
        let ast = progToAST conts
        let mainFun = firstFun ast
        let symTbl = ST([],[],1,1)
        let mainExp = stripExp $ firstExp mainFun
        let firstVar = stripExp mainExp
--        let firstFold = foldExp (\x y -> x) (\y -> VAR y) firstVar 
        putStrLn.show $ firstVar




fname = "testFile1"
conts = readFile fname
ast = fmap progToAST conts
mainFun = fmap firstFun ast
letExp = fmap firstExp mainFun
mainExp = fmap stripExp letExp
firstVar = fmap stripExp mainExp
firstTest = fmap stripLET letExp
symTbl = ST([], [], 1, 1)
testFunc = fmap (\x -> myFunFunc x symTbl) firstTest
