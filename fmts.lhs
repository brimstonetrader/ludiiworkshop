> data Tiling where
>   Tri         :: Tiling
>   Squ         :: Tiling
>   Hex         :: Tiling
>   T_3_12'2    :: Tiling
>   T_3_4_6_4   :: Tiling
>   T_4_6_12    :: Tiling
>   T_3_6_3_6   :: Tiling
>   T_4_8'2     :: Tiling
>   T_4_3_4_3'2 :: Tiling
>   T_3'3_4'2   :: Tiling
>   T_6_3'4     :: Tiling








> inst :: Int -> Int
> inst r = ((1664525*r+1013904223) `mod` (2 ^ 32)) 

> lcg2 :: Int -> Int -> [Int]
> lcg2 _ 1  = [1]
> lcg2 r i = do 
>   let n = inst r 
>   (((n `mod` i)+1) : (lcg2 n (i-1)))

> permute :: Int -> [Int] -> [Int]
> permute seed as = do 
>   let bs = lcg2 seed (len as)
>   permute2 as bs []

> permute2 :: [Int] -> [Int] -> [Int] -> [Int]
> permute2 as []     cs = cs 
> permute2 as (b:bs) cs = do
>   let n          = len as
>   let (ds, e:es) = splitAt (fromIntegral (b-1)) as
>   permute2 (ds++es) bs (e:cs) 

> randPath :: Int -> Int -> IO ()
> randPath seed n = do 
>   let rnds = permute seed [1..16]
>   putStr (convert 0 n rnds)

> data Move where
>   Diag :: Limit  -> Move
>   Orth :: Limit  -> Move
>   Hipp :: Coord  -> Limit -> Move
>   Mult :: [Move] -> Move

> data Limit where
>   Num   :: Int        -> Limit
>   Range :: (Int, Int) -> Limit
>   List  :: [Int]      -> Limit
>   None  ::               Limit

> type Coord = (Int, Int)


> convert :: Int -> Int -> [Int] -> String
> convert i n [] = "\n"
> convert i n (r:rnds) = if i%n==n-1 then ((intToChar r):"\n") ++ (convert (i+1) n rnds) else (intToChar r):(convert (i+1) n rnds)

> len :: [a] -> Int
> len []     = 0
> len (a:as) = 1+(len as)

> rep :: Int -> a -> [a]
> rep 0 b = []
> rep a b = b:(rep (a-1) b)

> amanybs :: Int -> a -> [a]
> amanybs 0 b = []
> amanybs a b = (b:amanybs (a-1) b)

> (%) :: Int -> Int -> Int
> a % b = a `mod` b

> (//) :: Int -> Int -> Int
> a // b = a `div` b

> (/%/) :: Int -> Int -> (Int, Int)
> a /%/ b = a `divMod` b

> (~) :: Int -> Int -> Int
> a ~ 0 = a
> 0 ~ b = b
> a ~ b = do
>   let (da,ma) = a /%/ 2
>   let (db,mb) = b /%/ 2
>   if ma == mb 
>     then    2*(da ~ db)
>     else 1+(2*(da ~ db))


> intToChar :: Int -> Char
> intToChar 1  = '0'
> intToChar 2  = '1'
> intToChar 3  = '2'
> intToChar 4  = '3'
> intToChar 5  = '4'
> intToChar 6  = '5'
> intToChar 7  = '6'
> intToChar 8  = '7'
> intToChar 9  = '8'
> intToChar 10 = '9'
> intToChar 11 = 'A'
> intToChar 12 = 'B'
> intToChar 13 = 'C'
> intToChar 14 = 'D'
> intToChar 15 = 'E'
> intToChar 16 = 'F'
> intToChar x  = intToChar (x%16)


> data Visible where 
>   Is  :: Bool
>   For :: [Bool]

> data Player where
>   P     :: Int -> Player
>   Game  :: Player
>   Prev  :: Player
>   Curr  :: Player
>   Next  :: Player
>   All   :: Player
>   Any   :: Player
>   Rand  :: Player
>   Other :: Player

> data Graph where
>   Dictionary ::     [[Int]] -> Graph
>   IncMatrix  ::    [[Bool]] -> Graph
>   EdgeList   :: [(Int,Int)] -> Graph
>   Triangle   ::     [[Int]] -> Graph
 
> data Boundary where 
>   Rigid :: Boundary
>   Abyss :: Boundary
>   Torus :: Boundary
 
> data Geometry where 
>   Lin ::   Int -> Visible -> Boundary -> Geometry
>   Squ ::   Int -> Visible -> Boundary -> Geometry 
>   Tri ::   Int -> Visible -> Boundary -> Geometry
>   Hex ::   Int -> Visible -> Boundary -> Geometry
>   Grp :: Graph -> Visible -> Boundary -> Geometry

> type Board = Geometry -> [Cell]


> data Unit where
>   Core ::        Player -> Unit
>   Null ::                  Unit
>   Node ::           DNA -> Unit
>   Pile ::        [Node] -> Unit

> type DNA = Player -> Color -> Char



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
>   N :: Cell
>   O :: Unit :: Cell
      

Lin 3 -> OOO | Squ 3 -> OOO | Tri 3 ->    O   | Hex 3 ->    OOO
             |          OOO |            OOO  |            OOOO
             |          OOO |           OOOOO |           OOOOO
             |              |                 |           OOOO
             |              |                 |           OOO	
			
> data Move where
>   Sing :: Vector -> Sym -> Limit -> Move
>   List :: [Move] -> Move

> data Limit where
>   Num   :: Int        -> Limit
>   Range :: (Int, Int) -> Limit
>   Any   :: [Int]      -> Limit
>   No    ::               Limit

> data Sym where
>   None :: Sym 
>   Line :: Sym 
>   Quad :: Sym 
>   Octo :: Sym 

> type Vector = (Int, Int)
			
> data Knot where
>   Permute :: [Int] -> [Cell]
>   Random  :: [(Int,Cell)]
			 


> type Contiguity = Bool

> type Capture = Bool
> type EdgeLoop = Bool

> data Group where
>   Line  :: Contiguity -> Limit -> Group
>   Shape :: [Omino] -> Group
>   Region :: Group
>   Loop   :: Capture -> Edges -> Group
>   Connect :: [Cell] -> [Cell]

o GROUP CAN BE [BROKEN LINE, 
                UNBROKEN LINE, 
				LINE OF SIGHT, 
				CONTIGREGION, 
				SHAPE(TURTLE STEP NOTATION, W HIPPOGONALITY AND X Y Z TYPE NOTATION FORMULATED FOR ENTROPY), 
				LOOP(CAPTURE,SIDES,EDGEALIGNMENT), 
				CONNECT {X, Y}]







  










BOARD (CAN HAVE ONE OR SEVERAL)

O EACH PLAYER AUTO HAS A SQUARE, HEX, OR CIRCLE WITH THEIR CHARACTER ON IT. THIS IS USED TO REPRESENT TURN ORDER, CAN BE ANY WHOLE NUMBER. CAN BE REFERRED TO AS P1, P2, ALL, ANY, ME, NEXT, PREV, RAND OR OTHER.

O [GRID, HEX, GRAPH]
	MAY DELETE INDIV. NODES MANUALLY, BUT UP TOP MAY ONLY MAKE A SQUARE OR RHOMBUS W GRIDS.

	O [UNBOUND, BOUND, 2-TORUS, 4-TORUS]
	
	O [LIST OF PLAYERS] EACH SITE CAN BE SET VISIBLE TO ALL, NONE, OR A LIST OF PLAYERS

INIT

o CONFIGURE UNITS ON BOARD MANUALLY. 
  STACKS POSSIBLE, HAND AND BOARD LOOK SAME.

O UNITS CAN HAVE ONE OF 12 COLORS, AND ONE OF 48 CHARACTERS. BOOLEANS CAN CHECK BOTH.

o CAN SET RANDOM PERMUTATION OF GIVEN ELEMENTS, 
  IN STACK (CARDS) OR PARTITION ACROSS BOARD (LIKE CHESS 960)

o CAN DEFINE RANDOM DISTRIBUTIONS FOR DICE
  SET INIT LISTS/VARIABLES IN THIS PART
  LISTS CAN'T BE CREATED MIDGAME, SO MAYBE DYNAMIC VARIABLES, SO YOU CAN UPDATE A LIST BY UPDATING A VARIABLE WITHIN IT?
o GROUP CAN BE [BROKEN LINE, 
                UNBROKEN LINE, 
				LINE OF SIGHT, 
				CONTIGREGION, 
				SHAPE(TURTLE STEP NOTATION, W HIPPOGONALITY AND X Y Z TYPE NOTATION FORMULATED FOR ENTROPY), 
				LOOP(CAPTURE,SIDES,EDGEALIGNMENT), 
				CONNECT {X, Y}]
o KOMI/PIE
o LISTS ARE LITERALLY BOARDSPACE, CAN HOLD CHARS=STRING OR INTS, VERT INDENT IS A COMMA.
o NO NEW UNITS MADE IN GAME
o AUTODICT "TERRITORY" PLAYERS TO LISTS OF UNITS ON BOARD


FROM:
	[LIST OF BOARDSPACES, 
	"MYBOARDSPACE", 
	"YOURBOARDSPACE", 
	LAST TO [PX], 
	A COORDINATE, 
	A LIST OF COORDINATES.] 
	
	[PARTIC UNIT, OPTIONAL]

TO:
	LIST OF BOARDSPACES
	"EMPTY" 
	"MYBOARDSPACE" 
	"YOURBOARDSPACE" 
	A COORDINATE 
	OR A LIST OF COORDINATES
	(TURTLE STEP)
	
	[PARTIC UNIT, OPTIONAL] 
	[CONDITIONAL BOOLEAN (GROUP SANCTITY)]




PLAY

o PHASE {

	[IF[BOOL], ELSE[BOOL], LOOP[ALL,ANY,N]]

		[PASS]
		
		[MOVE[FROM, TO]]
	
		[REMOVE[FROM]]

		[LET [STR] = [INT,STR,LIST]]

		[DEF [a -> a]]
		
		[VISIBILITY TOGGLE]
		
		[SWITCH[REGION]]
		
		[TRANSFORM[FROM, TO]]


	[PASS, MOVE[FROM,TO], ADD[BY CHOICE OR BY ALGORITHM], REMOVE, LOOP[ALL,ANY,N], LET [STR] =, VISIBILITYTOGGLE]
	[MOVE + MOVE = SWITCH, CAN BE OVERLOADED TO RANDOMLY PERMUTE]
	[REMOVE + ADD = TRANSFORM]
	[HET STACK INHERENT]
	[ALL TO A UNIT OR GROUP]
	[BOOL, GROUP] GROUP SANCTITY
}

WIN

FORM N GROUP(S)
UNIT IN TERRITORY
REMOVE ALL UNITS OF TYPE T
POINT TALLY (NEEDS (PCOUNT) VARIABLES WHICH IT DETERMINES MAX OF)
EMPIRICAL
LARGEST GROUP CASCADING




ESSENTIAL:

	BOOLOP   IF/ELSE THROUGHOUT, & | ! < > =, 
	INTOP    +,-,*,//,%,LOG2,PROXTOFRIEND/ENEMY/ANY[LOC,LOC]
	LISTOP	 SUM, SIZE, SORT, IN, UNION, INTERSECTION, DISJUNCTION, PERMUTATION, RAND, MAX, MIN, UNITCOUNT, GROUPCOUNT










