{-# LANGUAGE TypeOperators #-} -- Just in case..
module Tp01 where

data Lista = Vazio | Cons Int Lista deriving Show

-- Dica: Use o espaço determinado para sua dupla.
-- Faça o FORK, trabalhe no seu código, e ao término
-- abra um PULL REQUEST.

-- TODO: Criar para o tipo Lista uma função que execute
-- exatamente o que a seguinte função nativa faz.

-- reverse
-- TODO (Caio Liste e João)

-- END TODO (Caio Liste e João)

-- concatena
-- TODO (Christian e Luis)

concatena :: Lista -> [Int] -> [Int]
concatena Vazio xs = xs
concatena (Cons i is) xs = i : concatena is xs

-- > concatena (Cons 1 (Cons 2 (Cons 3 (Cons 4 Vazio)))) [5, 6, 7, 8]
-- > Resultado [1, 2, 3, 4, 5, 6, 7, 8]

concatena' :: Lista -> Lista -> [Int]
concatena' Vazio Vazio = []
concatena' Vazio (Cons x xs) = x : concatena' Vazio xs
concatena' (Cons i is) xs = i : concatena' is xs

-- > concatena' (Cons 1 (Cons 2 (Cons 3 (Cons 4 Vazio)))) (Cons 5 (Cons 6 (Cons 7 (Cons 8 Vazio))))
-- > Resultado [1, 2, 3, 4, 5, 6, 7, 8]

-- END TODO (Christian e Luis)

-- drop
-- TODO (tais e rene)

-- END TODO (tais e rene)

-- take
-- TODO (marcos e amauri)

-- END TODO (marcos e amauri)

-- last
-- TODO (daniel, karen)

-- END TODO (daniel, karen)

-- inits
-- TODO (Jonathas e Rosangela)

-- END TODO (Jonathas e Rosangela)

-- tails
-- TODO (Felipe e Carlos Eduardo)

tail' :: Lista -> Lista
tail' Vazio = Vazio
tail' (Cons i is) = is

tails :: Lista -> [[Lista]]
tails Vazio = [[Vazio]]
tails is = [is] : tails (tail' is)

-- -> Entrada: tails (Cons 3 (Cons 2 (Cons 1 Vazio)))
-- -> Saída: [[Cons 3 (Cons 2 (Cons 1 Vazio))],[Cons 2 (Cons 1 Vazio)],[Cons 1 Vazio],[Vazio]]

-- END TODO (Felipe e Carlos Eduardo)

-- transpose
-- TODO (Pedro Luiz e Caroline)

-- END TODO (Pedro Luiz e Caroline)

-- groupBy
-- TODO (Felipe e Thalles)
groupBy :: [Int] -> [[Int]]
groupBy [] = []
groupBy [first] = [[first]]
groupBy (first:second:rest)
   | first /= second = [first]:groupBy(second:rest)
   | otherwise = (first:[second]):groupBy rest

-- groupBy' :: [Lista] -> [[Lista]]
-- groupBy' [] = []
-- groupBy' [first] = [[first]]
-- groupBy' (first:second:rest)
--    //Linha com erro
--    | first /= second = [first]:groupBy'(second:rest) 
--    | otherwise = (first:[second]):groupBy' rest

-- END TODO (Felipe e Thalles)

-- filter
-- TODO (Daniele e Michelle)

-- END TODO (Daniele e Michelle)

-- subsequences
-- TODO (Vic e Coelho)

-- END TODO (Vic e Coelho)

-- foldl
-- TODO (cyro e henrique)

-- END TODO (cyro e henrique)

-- scanr
-- TODO (Helio e Lucca)

-- END TODO (Helio e Lucca)


-- Have fun!
