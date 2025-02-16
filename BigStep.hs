-- Definição das árvore sintática para representação dos programas:

data E = Num Int
      |Var String
      |Soma E E
      |Sub E E
      |Mult E E
      |Div E E
   deriving(Eq,Show)

data B = TRUE
      | FALSE
      | Not B
      | And B B
      | Or  B B
      | Leq E E
      | Igual E E 
   deriving(Eq,Show)

data C = While B C
    | If B C C
    | Seq C C
    | Atrib E E
    | Skip
    | Twice C
    | RepeatUntil C B
    | ExecN C E
    | Assert B C
    | Swap E E
    | DAtrrib E E E E
    | DoWhile B C
   deriving(Eq,Show)               


-----------------------------------------------------
-----
----- As próximas funções, servem para manipular a memória (sigma)
-----
------------------------------------------------


--- A próxima linha de código diz que o tipo memória é equivalente a uma lista de tuplas, onde o
--- primeiro elemento da tupla é uma String (nome da variável) e o segundo um Inteiro
--- (conteúdo da variável):


type Memoria = [(String,Int)]

exSigma :: Memoria
exSigma = [ ("x", 10), ("temp",0), ("y",0)]


--- A função procuraVar recebe uma memória, o nome de uma variável e retorna o conteúdo
--- dessa variável na memória. Exemplo:
---
--- *Main> procuraVar exSigma "x"
--- 10


procuraVar :: Memoria -> String -> Int
procuraVar [] s = error ("Variavel " ++ s ++ " nao definida no estado")
procuraVar ((s,i):xs) v
  | s == v     = i
  | otherwise  = procuraVar xs v


--- A função mudaVar, recebe uma memória, o nome de uma variável e um novo conteúdo para essa
--- variável e devolve uma nova memória modificada com a varíável contendo o novo conteúdo. A
--- chamada
---
--- *Main> mudaVar exSigma "temp" 20
--- [("x",10),("temp",20),("y",0)]
---
---
--- essa chamada é equivalente a operação exSigma[temp->20]

mudaVar :: Memoria -> String -> Int -> Memoria
mudaVar [] v n = error ("Variavel " ++ v ++ " nao definida no estado")
mudaVar ((s,i):xs) v n
  | s == v     = ((s,n):xs)
  | otherwise  = (s,i): mudaVar xs v n


-------------------------------------
---
--- Completar os casos comentados das seguintes funções:
---
---------------------------------


--- EXPRESSÕES ARITMÉTICAS
ebigStep :: (E,Memoria) -> Int
ebigStep (Var x,s) = procuraVar s x
ebigStep (Num n,s) = n
ebigStep (Soma e1 e2,s)  = ebigStep (e1,s) + ebigStep (e2,s)
ebigStep (Sub e1 e2,s)  = ebigStep (e1,s) - ebigStep (e2,s)
ebigStep (Mult e1 e2,s)  = ebigStep (e1,s) * ebigStep (e2,s)
ebigStep (Div e1 e2,s)
    | ebigStep (e2,s) == 0 = error "Divisão por zero"
    | otherwise = ebigStep (e1,s) `div` ebigStep (e2,s)


--- EXPRESSÕES BOOLEANAS
bbigStep :: (B,Memoria) -> Bool
bbigStep (TRUE,s)  = True
bbigStep (FALSE,s) = False
bbigStep (Not b,s) 
   | bbigStep (b,s) == True     = False
   | otherwise                  = True 
bbigStep (And b1 b2,s )
    | bbigStep (b1,s) == False  = False
    | otherwise                 = bbigStep (b2,s)
bbigStep (Or b1 b2,s )
    | bbigStep (b1,s) == True   = True
    | otherwise                 = bbigStep (b2,s)
bbigStep (Leq e1 e2,s)
    | ebigStep (e1,s) <= ebigStep (e2,s)    = True
    | otherwise                             = False
bbigStep (Igual e1 e2,s)
    | ebigStep (e1,s) == ebigStep (e2,s)    = True
    | otherwise                             = False


-- COMANDOS
cbigStep :: (C,Memoria) -> (C,Memoria)
cbigStep (Skip,s) = (Skip,s)
cbigStep (If b c1 c2,s)
    | bbigStep (b,s) == True    = cbigStep (c1,s)
    | otherwise                 = cbigStep (c2,s)
cbigStep (Seq c1 c2,s) 
    | c1 == Skip    = cbigStep (c2,s)
    | otherwise     = let (c1',s') = cbigStep (c1,s) in (cbigStep (c2,s'))
cbigStep (Atrib (Var x) e,s) = (Skip,mudaVar s x (ebigStep (e,s)))
cbigStep (Twice c,s) = cbigStep (Seq c c,s)
cbigStep (RepeatUntil c b,s) = cbigStep (Seq c (If b Skip (RepeatUntil c b)),s)
cbigStep (ExecN c e,s)
    | ebigStep (e,s) == 0       = (Skip,s)
    | otherwise                 = let (c',s') = cbigStep (c,s) in cbigStep (ExecN c (Num (ebigStep (e,s)-1)),s')
cbigStep (Swap (Var x) (Var y),s)
    | x == y        = (Skip,s)
    | otherwise     = (Skip,mudaVar (mudaVar s x (procuraVar s y)) y (procuraVar s x))
cbigStep (DAtrrib (Var x) (Var y) e1 e2,s) = (Skip,mudaVar (mudaVar s x (ebigStep (e1,s))) y (ebigStep (e2,s)))
cbigStep (Assert b c,s)
    | bbigStep (b,s) == False   = (Skip,s)
    | otherwise                 = cbigStep (c,s)
cbigStep (While b c,s)
    | bbigStep (b,s) == False   = (Skip,s)
    | otherwise                 = let (c',s') = cbigStep (c,s) in cbigStep (While b c,s')
cbigStep (DoWhile b c,s) = cbigStep (Seq c (While b c), s)

    



--------------------------------------
---
--- Exemplos de programas para teste
---
--- O ALUNO DEVE IMPLEMENTAR EXEMPLOS DE PROGRAMAS QUE USEM:
--- * Loop 
--- * Dupla Atribuição
--- * Do While
-------------------------------------

exSigma2 :: Memoria
exSigma2 = [("x",3), ("y",0), ("z",0)]


---
--- O progExp1 é um programa que usa apenas a semântica das expressões aritméticas. Esse
--- programa já é possível rodar com a implementação inicial  fornecida:

progExp1 :: E
progExp1 = Soma (Num 3) (Soma (Var "x") (Var "y"))

---
--- para rodar:
-- *Main> ebigStep (progExp1, exSigma)
-- 13
-- *Main> ebigStep (progExp1, exSigma2)
-- 6

--- Para rodar os próximos programas é necessário primeiro implementar as regras da semântica
---


---
--- Exemplos de expressões booleanas:


teste1 :: B
teste1 = (Leq (Soma (Num 3) (Num 3))  (Mult (Num 2) (Num 3)))

teste2 :: B
teste2 = (Leq (Soma (Var "x") (Num 3))  (Mult (Num 2) (Num 3)))


-- ---
-- -- Exemplos de Programas Imperativos:

testec1 :: C
testec1 = (Seq (Seq (Atrib (Var "z") (Var "x")) (Atrib (Var "x") (Var "y"))) (Atrib (Var "y") (Var "z")))

fatorial :: C
fatorial = (Seq (Atrib (Var "y") (Num 1))
                (While (Not (Igual (Var "x") (Num 1)))
                       (Seq (Atrib (Var "y") (Mult (Var "y") (Var "x")))
                            (Atrib (Var "x") (Sub (Var "x") (Num 1))))))


testec2 :: C
testec2 = (Assert (Leq (Var "x") (Num 10)) (Atrib (Var "x") (Num 0)))

testec3 :: C
testec3 = (DoWhile (Leq (Var "x") (Num 10)) (Atrib (Var "x") (Soma (Var "x") (Num 1))))