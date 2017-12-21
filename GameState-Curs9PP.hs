module C9 where

import Data.Char
import Debug.Trace

-- Pair 1 2
-- List [1,2,3]
l = List [Atom 1, Atom 2, List [Atom 3, Atom 4], Atom 5]

data Pair a = P a a
data NestedList a 	=	Atom a |	List [NestedList a]

instance Show a => Show (NestedList a) where
	show (Atom x) = "<" ++ show x ++ ">"
	show (List l) = "{" ++ (show $ head l) ++ concat (zipWith (++) (repeat ", ") $ map show $ tail l) ++ "}"
instance Show a => Show (Pair a) where
	show (P x y) = "<" ++ show x ++ ", " ++ show y ++ ">"

class Invertible a where
	invert	::	a -> a
	invert	=	id
instance Invertible (Pair a) where
	invert (P x y) = P y x
instance Invertible (NestedList a) where 
	invert a@(Atom x)	= a
	invert (List x)		= List $ reverse $ map invert x
instance Invertible [a] where
	invert = reverse

{-- v1
class Container a where	contents :: a -> [a]

instance Container [x] where
--	contents [x] = [x]
	contents = id
--}

{-- v2
class Container a where	contents :: a -> [b]

instance Container [x] where
	contents = id
--}


--v3
class Container t where
	contents :: t a -> [a]
instance Container Pair where
	contents (P x y) = [x, y]
instance Container NestedList where
	contents (Atom x)	=	[x]
	contents (List x)	=	concat $ map contents x
instance Container [] where
	contents = id
--}


--
f1 :: Eq a => a -> a -> a -> a
f1 x y z	=	if x == y then x else z

f2 :: (Eq (t a), Container t, Invertible (t a)) => t a -> t a -> [a]
f2 x y		=	if (invert x) == (invert y)
					then contents x
					else contents y

f3 :: Invertible a => [a] -> [a] -> [a]
f3 x y		=	(invert x) ++ (invert y)

f4 :: Ord a => a -> a -> a -> a
f4 x y z	=	if x == y then z else 
					if x > y then x else y
--}



---------------------



data ParsRes b = Parsed b
				| Failed
		deriving (Show, Eq)

fromRes (Parsed res) = res
fromRes Failed = undefined


type Parser a b = [a] -> (ParsRes b, [a])
--type Parser = [Char] -> (ParsRes Expr, [Char])


data Expr a = Number a | Op (Expr a) a (Expr a) deriving (Show, Eq)

fromN (Number x) = x

tokenFunc :: (a -> Bool) -> Parser a a
--tokenFunc f = \(h:t) -> if f h then (Parsed h, t] else (Failed, (h:t))
tokenFunc _ [] = (Failed, [])
tokenFunc f input@(h:t) = if f h then (Parsed h, t) else (Failed, input)

token s = tokenFunc (==s)

digit = tokenFunc isDigit
-- :t digit
operator = tokenFunc (flip elem $ ['+', '*'])
-- :t operator

-- take 10 $ iterate ((tokenFunc isDigit) . snd)  $ tokenFunc isDigit "2345"
-- take 20 $ map fst $ iterate ((tokenFunc isDigit) . snd) $ tokenFunc isDigit $ concatMap show [1..]
-- take 20 $ map (fromInteger . read . (\x -> [x]) . fromRes . fst) $ iterate ((tokenFunc isDigit) . snd) $ tokenFunc isDigit $ concatMap show [1..]

(>+>) :: Parser a b -> Parser a c -> Parser a (b, c)
(>+>) p1 p2 = parseWithP2 . p1 where
		parseWithP2 (res1, rem1) = case res1 of
			Parsed r1 ->
				case p2 rem1 of
					(Parsed r2, rem2) ->
							(Parsed (r1, r2), rem2)
					(Failed, _) -> (Failed, rem1)
			Failed -> (Failed, rem1)


-- :t digit >+> digit
-- digit >+> digit $ "2345"
-- digit >+> operator $ "2+5"


(>=>) :: Parser a b -> (b -> c) -> Parser a c
(>=>) parser process = (\(res, rem) -> case res of
						 Parsed r -> (Parsed $ process r, rem)
						 otherwise -> (Failed, rem)
						 ) . parser

-- :t digit >+> digit >=> (\(n1, n2) -> read [n1] * 10 + read [n2])
-- digit >+> digit >=> (\(n1, n2) -> read [n1] * 10 + read [n2]) $ "1234"


secv parser input
		| res == Failed = (Failed, rem)
		| resMore == Failed = let Parsed r = res in (Parsed [r], rem)
		| otherwise = (parser >+> secv parser) >=> (\(a, b) -> a:b) $ input
		where
			(res, rem) = parser input
			(resMore, remMore) = parser rem

-- secv digit "2345+"

(>|>) :: Parser a b -> Parser a b -> Parser a b
(>|>) p1 p2 input = case (res1, res2) of
			(Failed, Failed) -> (Failed, input)
			(Parsed r, _) -> r1
			(_, Parsed r) -> r2
		where
			r1@(res1, rem1) = p1 input
			r2@(res2, rem2) = p2 input


--(>*>) parser process = secv parser >=> process

-- (digit >*> (fromInteger . read)) $ "1234+"

--number = digit >*> id >=> Number
number = secv digit >=> Number

operand = ((token '(' >+> expr >+> token ')') >=> (\((_, e), _) -> e)) >|> number

expr = ((operand >+> operator >+> operand) >=>
		(\((e1, op), e2)-> Op e1 [op] e2  )) >|> number

get = fromRes . fst

compute (Number a) = read a
compute (Op e1 op e2) = (case op of "+" -> (+) ; "*" -> (*) ) (compute e1) (compute e2)

-- compute . get . expr $ "123+((25+3)*5)"


-------------------------------------------------------

-- tree = [1, [tree], [tree]] -> tip neuniform al elementelor din listă
-- tree = (1, tree, tree) -> tip infinit pentru ultimele două elemente din pereche

data Tree a = Nil				-- arbore vid
			| Leaf a			-- frunză
			| Node a [Tree a]	-- nod cu un număr oarecare de copii

root Nil = undefined
root (Leaf r) = r
root (Node r _) = r
			
tree1 = Node 1 [tree1, tree1]

-- arbore binar care la BFS dă lista de numere
treeB = Node 1 [(tBL 1), (tBR 1)]
	where
		tBL p = let l=2*p in if l > 7 then Leaf l else Node l [(tBL l), (tBR l)]
		tBR p = let r=2*p+1 in if r > 7 then Leaf r else Node r [(tBL r), (tBR r)]

-- arbore binar infinit care la BFS dă lista de numere
treeIB = Node 1 [(tBL 1), (tBR 1)]
	where
		tBL p = let l=2*p in Node l [(tBL l), (tBR l)]
		tBR p = let r=2*p+1 in Node r [(tBL r), (tBR r)]
		
preord Nil = []
preord (Leaf root) = [root]
preord (Node root children) = foldl (++) [root] $ map preord children

terminate 0 _ = Nil
terminate at t = case t of
	Node x children -> Node x $ map (terminate (at-1)) children
	otherwise -> t


instance Show a => Show (Tree a) where
	show tree = pt 0 tree where
				pt level tree = case tree of
					Nil -> space level ++ "- \n"
					Leaf val -> space level ++ show val ++ "\n"
					Node val children -> space level ++ show val ++ "\n" ++ 
						if all isNil children then "" else
							concatMap (pt $ level + 1) children
				space sp = [' ' | _ <- [1..sp * 2]]
				isNil Nil = True
				isNil _ = False



class TreeLike t where
	makeOne :: a -> t a
	toTree :: (Show a, Eq a) => (t a) -> Tree a
	reduce :: (Show a, Eq a) => (a -> [b] -> b) -> (a -> b) -> b -> (t a) -> b
	reduce f1 f2 v0 struct = reduceIt $ toTree struct where
				reduceIt Nil = v0
				reduceIt (Leaf x) = f2 x
				reduceIt (Node x children) = f1 x $ map reduceIt children

instance TreeLike Tree where
	makeOne = Leaf
	toTree x = x

sumTree tree = reduce (flip $ (+) . foldl (+) 0) id 0 tree


instance TreeLike Expr where
	makeOne = Number
	toTree (Number x) = Leaf x
	toTree (Op e1 op e2) = Node op [toTree e1, toTree e2]


computeExpr e = reduce f1 f2 0 $ get $ expr e where
					f2 = read
					f1 "+" = foldl (+) 0
					f1 "*" = foldl (*) 1

-- computeExpr $ "123+((25+3)*5)"

-----------------------



data Graph a = G [(a, a)]

graph = G [(3, 8), (3, 10), (3, 5), (5, 11), (7, 8), (7, 11),
		(8, 9), (11, 2), (11, 9), (11, 10)]

outgoing x = map snd . (filter $ (== x) . fst)
nodes (G []) = []
nodes (G (e:g)) = eNodes ++ (filter (not . (flip elem $ eNodes)) $ nodes $ G g) where eNodes = [fst e, snd e]

find _ Nil = False
find x (Leaf y) = x == y
find x (Node y l) = x == y || (or . (map $ find x) $ l)

-- toTree implementat doar pentru grafuri care pot fi parcurse integral pornind din primul nod din prima muchie.

instance TreeLike Graph where
	makeOne x = G [(x, undefined)]
{-
-- varianta 1: dfs întoarce o pereche între arborele rezultat din parcurgere
--  și lista tuturor nodurilor parcurse până în prezent
-- rezultatul lui fold (și deci al lui procDFS) este o pereche între lista subarborilor copiilor și 
--  lista tuturor nodurilor parcurse până în prezent
	toTree (G edges) = fst $ dfs (fst . head $ edges) [] where
			dfs v h = case outgoing v edges of
				[] -> (Leaf v, v:h)
				l -> (Node v $ reverse . fst $ res, snd res)
					where
						res = foldl procDFS ([], v:h) $ outgoing v edges
						procDFS r@(trees, hLocal) c = if elem c hLocal then r
							else let dd = dfs c hLocal in (fst dd : trees, snd dd)
						-- în rezultatul unui procDFS colectăm subarborele pentru fiecare copil (într-o listă de arbori)
						-- și totalitatea nodurilor parcurse până acum (ca în dfs-ul clasic)
--}
--{--
-- varianta 2: dfs întoarce doar arborele
-- rezultatul lui fold (și deci al lui procDFS) este o listă de arbori parcurși până acum
-- ultimul element din listă (adăugat înainte de intrarea în fold) este un arbore cu elementele
--  parcurse până acum în afara copiilor nodului curent
	toTree (G edges) = dfs first (Leaf first) where
			first = fst . head $ edges
			dfs v done = case filter (not . (flip find) done) $ outgoing v edges of
						-- verificăm copii care nu au fost parcurși încă
				[] -> Leaf v -- nu există -> frunză
				(_:_) -> --trace ("enter node " ++ show v ++ " with " ++ show (preord done)) $
					 Node v $ tail $ reverse res -- pentru crearea rezultatului luăm doar arborii rezultați 
					 	-- din parcurgerea copiilor, nu și arborele 'done', venit de mai sus (ultimul din res)
					where
						res = foldl procDFS [done] $ outgoing v edges
						procDFS trees c = if or . (map $ find c) $ trees then trees
							else dfs c (Node c trees) : trees
							-- pentru fiecare copil neparcurs încă (nu este în niciun arbore din trees)
							-- intrăm recursiv în DFS
							-- trimitem un arbore cu copilul ca rădăcină, având drept copii
							--  arborele primit de mai sus plus arborii parcurși în copiii anterior ai aceluiași părinte
--}



-- verificare makeOne
-- makeOne 5 -- nu știm ce tip trebuie să întoarcă makeOne (este ambiguu)
-- preord $ makeOne 5
-- fromN $ makeOne 5
-- nodes $ makeOne 5 -- excepție (undefined)
-- head $ nodes $ makeOne 5 -- nu dă excepție mulțumită evaluării leneșe






