# Hylomorphisms - morphine for programmers?

One of many great articles from Bartosz Milewski is the [Stalking a Hylomorphism in the Wild](https://bartoszmilewski.com/2017/12/29/stalking-a-hylomorphism-in-the-wild/) where he uses a concept from category theory called *hylomorphisms* to make an elegant solution to [a problem from advent of code](https://adventofcode.com/2017). My conclusions are that hylomorphisms

  * can be used to simplify the implementation of many kinds of recursive search algorithms
  * can be considered a functional design pattern 
  * does not require a deep understanding of category theory
  * may be limited to lazy languages

In this article I will apply the same technique to [another problem from advent of code](https://adventofcode.com/2020/day/20). 

## Hylo what?

A common pattern in functional programming is to use `fold` to collapse a sequence into an aggregate. For example, a sum can be expressed as a fold in Haskell as

```haskell
sum :: [Int] -> Int
sum = foldr (+) 0
```

Functions that matches the type signature `f a -> a` together with a functor `f` and a type `a` form an *algebra*. For `sum`, `f` corresponds to the list functor `[]` and `a` to `Int`. In Haskell we can define

```haskell
type Algebra f a = f a -> a
```

In category theory most concepts come with a dual where the arrows are reversed. The new concept is usually named "co-" plus the original one. Lets try that

```haskell
type Coalgebra f a = a -> f a
```

Indeed there is such a thing. *Co-algebras* are closely related to `unfoldr` for lists. Instead of collapsing a list into an aggregate, `unfoldr` builds up a list from a seed. For example the infinite list of fibonacci numbers can be expressed as

```haskell
fib = unfoldr (\(p,c) -> Just (p, (c, p+c))) (1,1)
```

The intuition behind a *hylomorphism* is that it builds up a structure using a co-algebra and a seed
and then tears it down using an algebra and an aggregate. It can be defined as

```haskell
hylo :: Functor f => Algebra f a -> Coalgebra f b -> b -> a
hylo f g = f . fmap (hylo f g) . g
```

In a lazy language like Haskell the full structure will never be built which is important since it might be to large to fit into memory.

## Putting theory into practice

[The given problem](https://adventofcode.com/2020/day/20) is a kind of puzzle. Each piece, henceforth called tile, has four sides. Each tile side is associated with a bit sequence. The bit sequences of two adjacent sides must match. It is allowed to rotate and/or flip a tile to make it match. A tile is also associated with an integer id. The solution is given by multiplying the ids of the corner tiles of the finished puzzle.

The following data types are used to represent tiles and tile sides

```haskell
type Side = [Bool]

data Tile = Tile { _tileId :: Int, _tileSides :: [Side] }
```

Tiles are arranged onto a board. The mental model of a board is a two-dimensional array where each position is either empty or populated with a tile. Arrays are inconvenient to expand and it is also inefficient to enumerate candidate positions for new tiles. For these reasons the following data structure is used instead

```haskell
type Index = (Int,Int)

data Board = Board { _indexToTile :: Map Index Tile, _holes :: [Index] }
```

At this point it is time to think about the co-algebra and the seed. The puzzle tiles are given as input data. That indicates that tiles are needed as a seed. Having only a set of tiles is not enough though. To solve the puzzle, tiles are added to the board; one by one. That means that a board is also required. The reasoning leads up to the following definition

```haskell
type Pool = [Tile]

data Seed = Seed {  _pool :: Pool, _board :: Board }
```

The search tree that the co-algebra should unfold is either
  * a finished board
  * a list of seeds for alternative continuations

This can be encoded as

```haskell
data SearchTreeF a = Leaf Board | Node [a] deriving Functor
```

Remember that `hylo` require that the data structure is a functor? The functor instance can many times be automatically derived as above.

The co-algebra expresses one step of the recursive search algorithm; it takes the seed and produces a structure filled with new seeds that in turn can be unfolded. 

In this case the base case is simple. If there are no more unplaced tiles available the puzzle is finished.

```haskell
buildSearchTree :: Coalgebra SearchTreeF Seed
buildSearchTree (Seed [] board) = Leaf board
```

If there are more tiles; pick one from the pool and add it, possible rotated or flipped, to one of the free positions on the board. The tile may only be added to a free position if it matches the neighbors.

```haskell
buildSearchTree (Seed pool board) =
    Node [ Seed (removeTileFromPool t pool) (addTileToBoard t' hole board) |
                t     <- pool,
                t'    <- rotationsAndFlips t,
                hole  <- _holes board,
                matches t' hole board
         ]
```

To calculate the final answer the corner tiles of the puzzle are required. The algebra thus needs to collapse the search tree into corner tiles of the finished boards.

The base case again is quite straight forward. Calculate the extreme coordinates and return the corresponding tiles. If the board is not rectangular an empty list is returned.

```haskell
getCorners :: Algebra SearchTreeF [[Tile]]
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

It is even simpler for internal nodes; just merge the list of solutions into one big list.

```haskell
getCorners (Node solutions) = concat solutions
```

Having defined both a co-algebra and an algebra the hylomorphism can fuse them together to 
create the actual solver.

```haskell
solve :: [Tile] -> Int
solve tiles =
    let corners = head $ hylo getCorners buildSearchTree (Seed tiles emptyBoard)
    in foldr ((*) . _tileId) 1 corners
```

Having extracted the corners it is trivial to calculate the final answer by multiplying the actual tile ids.

The full source code is available [here](./main.hs).