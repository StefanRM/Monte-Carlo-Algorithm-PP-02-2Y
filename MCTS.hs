{-# LANGUAGE MultiParamTypeClasses #-}

module MCTS where

import GameState

import Prelude hiding (traverse)
import System.Random
import Data.List
import Data.Ord

{-
    *** TODO ***

    Implementați tipul `Tree s a`, al arborilor de căutare, unde `s` reprezintă
    tipul stărilor, iar `a`, tipul acțiunilor.

    Sunt necesare câmpuri, precum:
    * starea curentă
    * acțiunea prin care s-a ajuns la stare
    * numărul de vizitări
    * scorul
    * copiii.
-}
data Tree s a = TreeNil | TreeNode {
    currState :: s,
    actionToState :: Maybe a,
    nrVisits :: Int,
    score :: Float,
    children :: [Tree s a]
}

{-
    *** TODO ***

    Implementați tipul `Zipper s a`, pentru parcurgerea arborilor de căutare
    de tipul `Tree s a`, unde `s` reprezintă tipul stărilor, iar `a`, tipul
    acțiunilor.

    Pe lângă componentele specifice unui zipper (vezi tutorialul din enunț),
    se va reține și un generator de numere aleatoare, modificat pe parcursul
    explorării arborelui.
-}
data TreeCrumb s a = TreeCrumb s (Maybe a) Int Float [Tree s a] [Tree s a]
 
data Zipper s a = Z {
    currSubTree :: Tree s a,
    treeCrumbs :: [TreeCrumb s a],
    gen :: StdGen
}

{-
    *** TODO ***

    Instanțiați clasa `Show` cu tipul `Tree s a`.
-}
instance (Show s, Show a) => Show (Tree s a) where 
    show tree = disp 0 tree where
                disp level tree = case tree of
                    TreeNil -> space level ++ "- \n"
                    TreeNode currState actionToState nrVisits score children -> space level ++ show currState ++ " (" ++ show actionToState ++ ") " ++ show nrVisits ++ " " ++ show score ++ "\n" ++ 
                        if all isNil children then "" else
                            concatMap (disp $ level + 1) children
                space sp = [' ' | _ <- [1..sp * 2]]
                isNil TreeNil = True
                isNil _ = False
{-
    ****************
    Funcții de acces
    ****************
-}

{-
    *** TODO ***

    Întoarce starea asociată unui nod.
-}
treeState :: Tree s a -> s
treeState TreeNil = undefined
treeState (TreeNode currState actionToState nrVisits score children) = currState

{-
    *** TODO ***

    Întoarce starea asociată unui nod.
-}
treeAction :: Tree s a -> a
treeAction TreeNil = undefined
treeAction (TreeNode currState actionToState nrVisits score children) = case actionToState of
                                                                            Nothing -> undefined
                                                                            Just action -> action

{-
    *** TODO ***

    Întoarce scorul unui nod.
-}
treeScore :: Tree s a -> Float
treeScore TreeNil = undefined
treeScore (TreeNode currState actionToState nrVisits score children) = score

{-
    *** TODO ***

    Întoarce numărul de vizitări ale unui nod.
-}
treeVisits :: Tree s a -> Int
treeVisits TreeNil = undefined
treeVisits (TreeNode currState actionToState nrVisits score children) = nrVisits

{-
    *** TODO ***

    Întoarce copiii unui nod.
-}
treeChildren :: Tree s a -> [Tree s a]
treeChildren TreeNil = undefined
treeChildren (TreeNode currState actionToState nrVisits score children)  = children

{-
    *** TODO ***

    Întoarce nodul pe care este centrat zipper-ul.
-}
zipperTree :: Zipper s a -> Tree s a
zipperTree (Z currSubTree treeCrumbs gen) = currSubTree

{-
    *** TODO ***

    Întoarce generatorul de numere aleatoare din interiorul zipper-ului.
-}
zipperGen :: Zipper s a -> StdGen
zipperGen (Z currSubTree treeCrumbs gen) = gen

{-
    *****************
    Funcții pe arbori
    *****************
-}

{-
    *** TODO ***

    Construiește un arbore de căutare (eventual infinit), pornind de la funcția
    de generare a succesoarelor unei stări și de la starea inițială.
-}
expand :: (s -> [(a, s)])  -- Generatorul stărilor succesoare
       -> s                -- Starea inițială
       -> Tree s a         -- Arborele de căutare
expand stateGen s0 = expandHelper stateGen s0 Nothing

expandHelper :: (s -> [(a, s)])  -- Generatorul stărilor succesoare
             -> s                -- Starea inițială
             -> Maybe a          -- Actiunea initiala
             -> Tree s a         -- Arborele de căutare
expandHelper stateGen s0 a0 = if length (stateGen s0) == 0 then
                                (TreeNode s0 a0 0 0 [TreeNil]) 
                               else (TreeNode s0 a0 0 0 (foldl (\acc (action, state) -> acc ++ [(expandHelper stateGen state (Just action))]) [] (stateGen s0)))

{-
    *** TODO ***

    Explorează arborele, alegând la fiecare pas un succesor aleator,
    până la atingerea unei stări terminale (victorie/ remiză).

    Întoarce:
    * calea urmată, în ordine inversă, astfel că primul element din cale
      este nodul terminal
    * semnificația stării terminale (victorie/ remiză)
    * varianta finală a generatorului de numere aleatoare.
-}
rolloutTree :: GameState s a => Tree s a -> StdGen -> ([Tree s a], Outcome, StdGen)
rolloutTree tree genState = rolloutTreeHelper tree genState ([], Ongoing, genState)

rolloutTreeHelper :: GameState s a => Tree s a -> StdGen -> ([Tree s a], Outcome, StdGen) -> ([Tree s a], Outcome, StdGen)
rolloutTreeHelper tree genState (resultList, _, _) = 
    case tree of
        TreeNil -> undefined
        TreeNode currState actionToState nrVisits score children ->
            case outcome currState of
                Ongoing -> rolloutTreeHelper (children !! (fst $ randomR (0, (length children) - 1) genState)) genState (tree:resultList, Ongoing, snd $ randomR (0, (length children) - 1) genState)
                currentOutcome -> (tree:resultList, currentOutcome, snd $ next genState)

{-
    *** TODO ***

    Determină cel mai bun copil al unui nod, din perspectiva raportului
    scor / număr de vizitări.

    Hint: `maximumBy` și `comparing`.
-}
bestChild :: Tree s a -> Tree s a
bestChild tree = case tree of
                    TreeNil -> undefined
                    TreeNode currState actionToState nrVisits score children ->
                        case children of
                            [TreeNil] -> undefined
                            childrenTrees -> maximumBy (comparing
                                (\(TreeNode currStateChild actionToStateChild nrVisitsChild scoreChild childrenChild) ->
                                    scoreChild / (fromIntegral nrVisitsChild))) children

{-
    *******************
    Funcții de zipper-e
    *******************
-}

{-
    *** TODO ***

    Construiește un zipper centrat pe arborele dat, care stochează generatorul
    de numere aleatoare precizat.
-}
getZipper :: Tree s a -> StdGen -> Zipper s a
getZipper tree gen = case tree of
                        TreeNil -> undefined
                        _ -> (Z tree [] gen)

{-
    *** TODO ***

    Verifică dacă zipper-ul este centrat pe rădăcina arborelui.
-}
isRoot :: Zipper s a -> Bool
isRoot (Z currSubTree treeCrumbs gen) = case currSubTree of
                                            TreeNil -> undefined
                                            TreeNode currState actionToState nrVisits score children ->
                                                case actionToState of
                                                    Nothing -> True
                                                    Just x -> False

{-
    *** TODO ***

    Valoarea ucb1 din filmuleț (constanta C = 2).
-}
ucb1 :: Float  -- scorul copilului
     -> Int    -- numărul de vizitări ale copilului
     -> Int    -- numărul de vizitări ale părintelui
     -> Float  -- estimarea
ucb1 scoreChild nrVisitsChild nrVisitsParent = if nrVisitsChild == 0 then
                                                    999999999
                                                else
                                                    (scoreChild / (fromIntegral nrVisitsChild)) + 2 * (sqrt ((log $ fromIntegral nrVisitsParent) / (fromIntegral nrVisitsChild)))

{-
    *** TODO ***

    Pentru nodul pe care este centrat zipper-ul dat ca parametru, selectează
    copilul având valoarea ucb1 maximă. Întoarce zipper-ul centrat pe copilul
    respectiv.

    Atenție! Așa cum rezultă și din filmuleț, un nod nevizitat are valoarea ucb1
    infinită, și va fi întotdeauna ales în defavoarea altor noduri deja vizitate.
-}
select :: Eq s => Zipper s a -> Zipper s a
select (Z currSubTree treeCrumbs gen) =
    case currSubTree of
        TreeNil -> undefined
        TreeNode currState actionToState nrVisits score children ->
            (Z (fst4 bestOfAllChildren) (treeCrumbs ++ [(TreeCrumb currState actionToState nrVisits score (thrd bestOfAllChildren) (frth bestOfAllChildren))]) gen)
            where
                bestOfAllChildren =
                    (foldl (\(tree, scoreAcc, listL, listR) treeChild ->
                        case treeChild of
                            TreeNil -> undefined
                            TreeNode currStateChild actionToStateChild nrVisitsChild scoreChild childrenChild ->
                                if (ucb1 scoreChild nrVisitsChild nrVisits) > scoreAcc then
                                    ((TreeNode currStateChild actionToStateChild nrVisitsChild scoreChild childrenChild), (ucb1 scoreChild nrVisitsChild nrVisits), listL ++ listR, [])
                                else
                                    (tree, scoreAcc, listL, listR ++ [(TreeNode currStateChild actionToStateChild nrVisitsChild scoreChild childrenChild)])
                                    )
                    (TreeNil, -1, [], []) children)

fst4 (x, y, z, t) = x;
thrd (x, y, z, t) = z;
frth (x, y, z, t) = t;

{-
    *** TODO ***

    Aplică repetat `select` până la atingerea unui nod nevizitat sau terminal.
-}
traverse :: (Eq s, GameState s a) => Zipper s a -> Zipper s a
traverse (Z currSubTree treeCrumbs gen) =
    case currSubTree of
        TreeNil -> undefined
        TreeNode currState actionToState nrVisits score children ->
            case outcome currState of
                Ongoing -> if nrVisits == 0 then
                                (Z currSubTree treeCrumbs gen)
                            else
                                traverse $ select (Z currSubTree treeCrumbs gen)
                _ -> (Z currSubTree treeCrumbs gen)

{-
    *** TODO ***

    Aplică `rolloutTree` pentru arborele pe care este centrat zipper-ul.

    Întoarce:
    * scorul cu care vor fi actualizate nodurile de pe calea către rădăcină
    * numărul jucătorului pentru care se realizează actualizarea
      (se poate ignora pentru cerința cu un singur jucător)
    * noul zipper, actualizat cu generatorul de numere aleatoare întors
      de `rolloutTree`.

    Pentru cerința cu cel puțin doi jucători, scorul și numărul jucătorului
    se calculează astfel:
    * Pentru victorie, se utilizează scorul din obictul `Outcome` și numărul
      jucătorului aferent stării terminale.
    * Pentru remiză, se utilizează scorul din obiectul `Outcome` împărțit
      la numărul de jucători, și `Nothing`.
-}
rolloutZipper :: GameState s a => Zipper s a -> (Float, Maybe Int, Zipper s a)
rolloutZipper (Z currSubTree treeCrumbs gen) =
    case currSubTree of
        TreeNil -> undefined
        TreeNode currState actionToState nrVisits score children ->
            case (head $ fst3 rolloutResult) of
                TreeNil -> undefined
                TreeNode currStateRes actionToStateRes nrVisitsRes scoreRes childrenRes ->
                    (scoreRes, Nothing, (Z currSubTree treeCrumbs (trd3 rolloutResult)))
            where
                rolloutResult = (rolloutTree currSubTree gen)

fst3 (x, y, z) = x;
snd3 (x, y, z) = y;
trd3 (x, y, z) = z;
        --rolloutTree :: GameState s a => Tree s a -> StdGen -> ([Tree s a], Outcome, StdGen)

{-
    *** TODO ***

    Urcă un nivel în arbore.
-}
toParent :: Zipper s a -> Zipper s a
toParent (Z currSubTree ((TreeCrumb s a v sc ls rs):bs) gen) =
    case currSubTree of
        TreeNil -> undefined
        TreeNode currState actionToState nrVisits score children ->
            (Z (TreeNode s a v sc (ls ++ [currSubTree] ++ rs)) bs gen)

{-
    *** TODO ***

    Implementează pasul de backpropagation, unde cei trei parametri sunt cele
    trei componente întoarse de `rolloutZipper`.

    Astfel, se urmează calea către rădăcină și se crește cu 1 numărul
    de vizitări ale tuturor nodurilor. În plus, scorul se modifică astfel:
    * Pentru cerința cu un singur jucător, se modifică toate nodurile.
    * Pentru cerința cu mai mulți jucători, avem următoarele cazuri:
      * În caz de victorie, se actualizează doar nodurile cu numărul de jucător
        dat de parametru.
      * În caz de remiză, se actualizează toate nodurile.
    
    Zipper-ul final este centrat pe rădăcină.
-}
backProp :: GameState s a => Float -> Maybe Int -> Zipper s a -> Zipper s a
backProp scoreRolled player (Z currSubTree treeCrumbs gen) =
    case currSubTree of
        TreeNil -> undefined
        TreeNode currState actionToState nrVisits score children -> 
            if isRoot (Z currSubTree treeCrumbs gen) == False then
                backProp scoreRolled player (toParent (Z (TreeNode currState actionToState (nrVisits + 1) (score + scoreRolled) children) treeCrumbs gen))
             else
                (Z (TreeNode currState actionToState (nrVisits + 1) (score + scoreRolled) children) treeCrumbs gen)
{-
    *** TODO ***

    Realizează o iterație completă a MCTS, incluzând toate etapele, pornind
    de la un nod oarecare din arbore și finalizând pe rădăcină.
-}
exploreOne :: (Eq s, GameState s a) => Zipper s a -> Zipper s a
exploreOne (Z currSubTree treeCrumbs gen) =
    case currSubTree of
        TreeNil -> undefined
        TreeNode currState actionToState nrVisits score children ->
            backProp finalScore player zipper
        where
            (finalScore, player, zipper) = rolloutZipper (Z currSubTree treeCrumbs gen)

{-
    *** TODO ***

    Realizează un număr dat de iterații complete ale MCTS.
-}
exploreMany :: (Eq s, GameState s a) => Int -> Zipper s a -> Zipper s a
exploreMany nr zipper = foldl (\acc it -> (exploreOne acc)) zipper [1..nr]

{-
    *** TODO ***

    Alege o acțiune pornind de la o stare dată și un număr de iterații ale MCTS.
    Întoarce o pereche cu acțiunea și starea următoare.

    Funcția ar trebui să verifice mai întâi dacă nu cumva una dintre stările
    imediat următoare reprezintă o victorie, caz în care o alege direct.
    Altfel, demarează procesul standard de explorare.

    Atenție! La prima iterație a algoritmului, cu toate că numărul de vizitări
    ale rădăcinii este 0, NU se face rollout la rădăcină, ci se trece direct
    la explorarea copiilor acesteia. Acest lucru este vizibil și în filmuleț.

    După realizarea numărului dat de iterații, se alege efectiv acțiunea,
    utilizând `bestChild`.
-}
choose :: (Eq s, GameState s a) => Int -> s -> StdGen -> (a, s)
choose = undefined
