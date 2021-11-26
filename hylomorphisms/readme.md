# Hylomorphisms

One of many great articles from Bartosz Milewski is the [Stalking a Hylomorphism in the Wild](https://bartoszmilewski.com/2017/12/29/stalking-a-hylomorphism-in-the-wild/). My conclusions are that hylomorphisms

  * Can be used to simplify the implementation of many kinds of recursive search algorithms
  * Can be considered a functional design pattern 
  * Does not require a deep understanding of category theory
  * May be limited to lazy languages

In this article I will apply the same technique to [another problem](https://adventofcode.com/2020/day/20) from Advent of code. 

## Hylo what?

A common pattern in functional programming is to use `fold` to collapse a sequence into an aggregate. For example, a sum can be expressed as a fold in Haskell as

```
sum :: [Int] -> Int
sum = foldr (+) 0
```

Functions that matches the type signature `f a -> a` for a functor `f` and a type `a` are called *algebras*. In the case of sum, `f` corresponds the list functor `[]` and `a` to `Int`. In Haskell we can define

```
type Algebra f a = f a -> a
```

In category theory every concept comes with a dual where the arrows are reversed. The new concept is named "co-" plus the original one. Lets try that

```
type Coalgebra f a = a -> f a
```

Indeed there is such a thing. For lists, *co-algebras* can be expressed using `unfoldr`. Instead of collapsing a list into an aggregate, `unfoldr` builds up a list from a seed. For example the infinite list of fibonacci numbers can be expressed as

```
fib = unfoldr (\(p,c) -> Just (c, (c, p+c))) (1,1)
```

The intuition behind a *hylomorphism* is that it builds up a structure using a coalgebra and a seed
and then tears it down using an algebra and an aggregate. It can be defined as

```
hylo :: Functor f => Algebra f a -> Coalgebra f b -> b -> a
hylo f g = f . fmap (hylo f g) . g
```

In a lazy language like Haskell the full structure will never be built which is important since the it might be to large to fit into memory.

## Putting theory into practice

The given problem is a kind of puzzle. Each piece, henceforth called tile, has four sides. Each tile side is associated with a bit sequence. The bit sequences of two adjacent sides must match. It is allowed to rotate and/or flip a tile to make it match. A tile is also associated with an integer id. The solution is given by multiplying the ids of the corner tiles of the finished puzzle.

The following data types are used to represent tiles and tile sides
```
type Side = [Bool]

data Tile = Tile { _tileId :: Int, _tileSides :: [Side] }
```

Layed out tiles are called a board and is represented as

```
type Index = (Int,Int)

data Board = Board { _indexToTile :: Map Index Tile, _holes :: [Index] }
```

At this point it is time to think about the co-algebra and the seed. The puzzle tiles are given as input data. That indicates that tiles are needed as a seed. Having only a set of tiles is not enough though. To solve the puzzle, tiles are added to the board; one by one. That means that a (partly solved) puzzle board is also required. The reasoning leads up to the following definition.

```
type Pool = [Tile]

data Seed = Seed {  _pool :: Pool, _board :: Board }
```

The search tree that the co-algebra should unfold is either
  * a finished board where all the tiles have been put together
  * a list of seeds based on different puzzle choices

This can be encoded as

```
data SearchTreeF a = Leaf Board | NodeF [a] deriving Functor
```

Remember that the co-algebra requires the search tree data structure needs to be a functor. The functor instance can be written by hand or, in many cases, just simply derived as above.

The co-algebra expresses one step of the recursive search algorithm; it takes the seed and produces a structure filled with new seeds that in turn can be unfolded.

The base case is simple; if there are no more tiles left to place onto the board the puzzle is finished.

```
buildSearchTree :: Coalgebra SearchTreeF Seed
buildSearchTree (Seed [] board) = Leaf board
```

If there are tiles; pick one from the pool and add it, possible rotated or flipped, to one of the free positions on the board. The tile may only be added to a free position if it matches the neighbors.

```
buildSearchTree (Seed pool board) =
    NodeF [ Seed (removeTileFromPool t pool) (addTileToBoard t' hole board) |
                t     <- pool,
                t'    <- rotationsAndFlips t,
                hole  <- _holes board,
                matches t' hole board
          ]
```

To calculate the final answer the corner tiles of the puzzle are required. The algebra thus needs to collapse the search tree into corner tiles of the finished boards. Note that there might be many boards if the puzzle solutions is not unique.

The base case again is quite straight forward. When given a board, just extract the corner tiles.

```
getCorners (Leaf board) =
    let
        indexToTile = _indexToTile board
        indices = Map.keysSet indexToTile
        (xCoords, yCoords) = (Set.map fst indices, Set.map snd indices)
        cornerTiles = do
            minX <- Set.lookupMin xCoords
            maxX <- Set.lookupMax xCoords
            minY <- Set.lookupMin yCoords
            maxY <- Set.lookupMax yCoords
            mapM (`Map.lookup` indexToTile) [(minX,minY),(minX,maxY),(maxX,minY),(maxX,maxY)]
    in
        case cornerTiles of
            Just tiles -> [tiles]
            Nothing -> []
```

For internal nodes, it is even simpler; just merge the list of solutions into one big list.

```
getCorners (NodeF solutions) = concat solutions
```

Having defined both a co-algebra and an algebra the hylomorphism can fuse them together to 
create the actual solver.

```
solve :: [Tile] -> Int
solve tiles =
    let corners = head $ hylo getCorners buildSearchTree (Seed tiles emptyBoard)
    in foldr ((*) . _tileId) 1 corners
```
