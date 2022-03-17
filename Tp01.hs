{-# LANGUAGE TypeOperators #-} -- Just in case..
module Tp01 where

data Lista = Vazio | Cons Int Lista deriving Show

-- Dica: Salve seu progresso em um arquivo separado.
-- Sempre que for efetuar um PUSH, primeiro execute um PULL
-- para atualizar a versão do arquivo localmente. Cole seu
-- código no espaço determinado para sua dupla. Salve,
-- efetue o COMMIT (git add .) e (git commit -m "${MENSAGEM}")
-- e o PUSH (git push).

-- TODO: Criar para o tipo Lista uma função que execute
-- exatamente o que a seguinte função nativa faz.

-- reverse
-- TODO (Caio Liste e João)

-- END TODO (Caio Liste e João)

-- concatena
-- TODO (Christian e Luis)

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

tails :: Lista -> [[Lista]]
tails Vazio = []
tails (Cons i is) = [is] :  tails is

-- > tails (Cons 4 (Cons 3 (Cons 2 (Cons 1 Vazio))))
-- > Resultado: [[Cons 3 Cons 2 (Cons 1 Vazio)], [Cons 2 (Cons 1 Vazio)], [Cons 1 Vazio], [Vazio]]

tails' :: Lista -> [Lista]
tails' Vazio = []
tails' (Cons i is) = is :  tails' is

-- > tails' (Cons 4 (Cons 3 (Cons 2 (Cons 1 Vazio))))
-- > Resultado: [Cons 3 Cons 2 (Cons 1 Vazio), Cons 2 (Cons 1 Vazio), Cons 1 Vazio, Vazio]

-- END TODO (Felipe e Carlos Eduardo)

-- transpose
-- TODO (Pedro Luiz e Caroline)

-- END TODO (Pedro Luiz e Caroline)

-- groupBy
-- TODO (Felipe e Thalles)

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
scanl' :: (Lista, Int) -> Int
scanl' (Vazio, acumulador) = acumulador
scanl' ((Cons valor resto), acumulador) = scanl'(resto, acumulador+valor)
-- END TODO (Helio e Lucca)


-- Have fun!