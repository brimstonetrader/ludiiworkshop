Let's think of games as a Finite Series of Matrix Transforms. The root nodes here will 
represent the agents involved. Any game has some number of Players. We can refer to them as  
"P 1", "P 2", et. al. "Game" refers to transforms enacted with no player's approval. The other 
things are operations we'll fill in later, which will return some list of players, or the empty
list.

> data Player where
>   P     ::      Int -> Player
>   Game  ::             Player
>   Prev  ::             Player
>   Curr  ::             Player
>   Next  ::             Player
>   All   ::             Player
>   Any   ::             Player
>   Rand  ::             Player
>   Not   :: [Player] -> Player

Visible could be "Is True", "Is False", or "For [True,False]", with the last meaning that 
the location is only visible to P1.

> data Visible where 
>   Is  ::   Bool -> Visible
>   For :: [Bool] -> Visible

The boundary of the board could be Rigid (no escape), or Abyss (you can go off but there's 
nothing) or Torus (the ends wrap around on each other) 

> data Boundary where 
>   Rigid ::                 Boundary
>   Abyss ::                 Boundary
>   Torus :: [(Int, Int)] -> Boundary

We will accept three graph notations. 

> data Graph where
>   Dictionary :: Int ->     [[Int]] -> Graph
>   EdgeList   :: Int -> [(Int,Int)] -> Graph
>   Triangle   :: Int ->    [[Bool]] -> Graph

-- GRAPH      DICTIONARY     EDGELIST           TRIANGLE     
-- 1   2     [[3,4],         [(1,3),(1,4)      [[0],                
-- |\ /|      [4,5],          (2,4),(2,5)       [1,0],          
-- 3-4-5      [1,4,6],        (3,4),(3,6)       [1,1,1],            
-- \/  |      [1,2,3,5,6],    (4,5),(4,6)       [0,1,0,1],            
-- 6   7      [2,4,7],        (5,7)]            [0,0,1,1,0],       
--            [3,4],                            [0,0,0,0,1,0]]
--            [5]]                     

A matrix consists of a network of connections, the set of locations, 
and the object on each.

A linear geometry means that a node at index n will be connected to 
those at (n-1) and (n+1) only. We will also autogenerate graphs of a 
specified size utilizing any of the three regular tilings. 


> data Geometry where 
>   Lin :: Int -> Graph -> Visible -> Boundary -> Geometry
>   Squ :: Int -> Graph -> Visible -> Boundary -> Geometry 
>   Tri :: Int -> Graph -> Visible -> Boundary -> Geometry
>   Hex :: Int -> Graph -> Visible -> Boundary -> Geometry
>   Grp :: Graph -> Visible -> Boundary -> Geometry

> type Matrix = [(Geometry, [(Node, Int)])]








There exists a "core" component for each player, through which they 
convey their decisions. If there is not exactly one for the active player, the game ends immediately in a loss. The standard should be that these exist

Eventually we will have a matrix, or grid, or graph. Each location, or, cell, within it will need a 
Node. If nothing's there, it will be Null. One thing (like a pawn or a bishop) is a Unit. In one Cell
we may place a large amount of Nodes, in a Pile. Lastly, a Node may be a Knot, tangled with other nodes.
On startup, knotted units get placed randomly, either with replacement (like dice) or without replacement
(like cards).

> data Node where
>   Core :: Player -> Node
>   Null ::           Node  
>   Unit ::    DNA -> Node
>   Pile :: [Node] -> Node
>   Knot ::   Knot -> Node

> data Knot where
>   WOR ::       [Node] -> Knot
>   WR  :: [(Int,Node)] -> Knot

> type DNA = Player -> Color -> (Either Char (Char, Int))

> data Color where
>   Red     :: Color
>   Orange  :: Color
>   Yellow  :: Color
>   Lime    :: Color
>   Green   :: Color
>   Cyan    :: Color
>   Blue    :: Color
>   Purple  :: Color
>   Magenta :: Color
>   Pink    :: Color
>   LBrown  :: Color
>   DBrown  :: Color
>   LGrey   :: Color
>   DGrey   :: Color
>   White   :: Color
>   Black   :: Color

> data Cell where
>   E :: Cell
>   Q :: Node -> Cell














> data Space = [Plane]






> type Capture = Bool
> type Contiguity = Bool
> type Edges   = Bool
> type Pie  = Bool

> data Limit where
>   Num   :: Int        -> Limit
>   Range :: (Int, Int) -> Limit
>   Choose   :: [Int]      -> Limit
>   No    ::               Limit

> data Group where
>   Line    :: Contiguity -> Limit -> String -> Group
>   Shape   ::             [Omino] -> String -> Group
>   Region  ::                        String -> Group
>   Enclose ::    Capture -> Edges -> String -> Group
>   Connect ::      [Int] -> [Int] -> String -> Group


> data Step where
>   F :: Step
>   L :: Step
>   R :: Step
>   O :: Step

> type Omino = [Step]


> type Var = (String, Value)

> data Value where
>   Int  :: Int -> Value
>   Bit  :: Bool -> Value
>   List :: [Value] -> Value   
>   deriving Show


> type Turn = Action

> data Action where
>   Pass   :: Visible -> Action
>   If     :: Visible -> Bool -> Action -> Action -> Action
>   Move   :: Visible -> From -> To -> Action
>   Add    :: Visible -> Unit -> To -> Action
>   Remove :: Visible -> From -> Action
>   Let    :: Visible -> String -> Value -> Action
>   Switch :: Visible -> From -> To -> Action
>   Change :: Visible -> From -> Unit -> Action
>   Do     :: [Action] -> Action
>   Choice :: [Action] -> Action
>   Loop   :: Int -> Action 

> data From where
>   Sites    :: [Loc] -> From
>   Mine     ::          From
>   LastFrom ::          From
>   LastTo   ::          From

> data Move where
>   Sing :: Vector -> Limit -> Move
>   List :: [Move] -> Move

> data To where
>   Sites    ::         [Loc] -> To
>   Empty    ::                  To
>   Mine     ::                  To
>   LastFrom ::                  To
>   LastTo   ::                  To
>   Where    :: Move -> Limit -> To

> type Loc = Int

> data Win where
>   NGroups   :: Int -> Group -> Win
>   Target    :: Player -> [Loc] -> Win
>   KillAll   :: Player -> Unit -> Win
>   Points    :: [Int] -> Win
>   Empirical :: Win
>   LGC       :: Group -> Win