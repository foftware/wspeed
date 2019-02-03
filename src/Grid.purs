module Grid where

import Control.Comonad.Cofree (Cofree, (:<), tail)
import Control.Comonad ((=<=))
import Data.Unfoldable (replicate)
import Data.Foldable (class Foldable, fold, intercalate)
import Data.Functor (class Functor, map)
import Data.Map (Map, empty)
import Data.Monoid (mempty)
import Prelude (class Eq, class Ord, Unit, ($), (<<<), compare, eq, unit)


data RowF a 
    = Gap Int a
    | Word String a
    | End 

instance foldableRowF :: Foldable RowF where
    foldMap f = case _ of
        Gap _ a -> f a
        Word _ a -> f a
        End -> mempty

    foldr f acc = case _ of
        Gap _ a -> f a acc
        Word _ a -> f a acc
        End -> acc

    foldl f acc = case _ of
        Gap _ a -> f acc a
        Word _ a -> f acc a
        End -> acc

instance functorRowF :: Functor RowF where
    map f = case _ of
        Gap n a -> Gap n (f a)
        Word s a -> Word s (f a)
        End -> End

type RowData a = Cofree RowF a

-- Get rid of this somehow... 
data RowMeta = RowMeta Int (Map String Int)

-- Get rid of this as well
data Row a = Row RowMeta (RowData a)

instance eqRow :: Eq a => Eq (Row a) where
    eq (Row (RowMeta i1 _) _) (Row (RowMeta i2 _) _) = eq i1 i2

instance ordRow :: Ord a => Ord (Row a) where
    compare (Row (RowMeta i1 _) _) (Row (RowMeta i2 _) _) = compare i1 i2

instance functorRow :: Functor Row where
    map f (Row meta d) = Row meta $ map f d

type Grid a = Array (Row a)

row :: Row Unit
row = 
    let rowData = unit :< Gap 8 (unit :< Word "hello word" (unit :< Gap 25 (unit :< End)))
    in Row (RowMeta 1 empty) rowData

displayData :: forall a. RowData a -> String
displayData rowA = case tail rowA of
    Gap n _ -> spaces n
    Word s _ -> s
    End -> ""
  where
    spaces n = fold (replicate n " " :: Array String)

displayRow :: forall a. Row a -> String
displayRow (Row _ d) = displayRowData d
  where
    displayRowData = fold =<= displayData

grid :: Grid Unit
grid = [ row, row, row, row ]

displayGrid :: forall a. Grid a -> String
displayGrid = intercalate newline <<< map displayRow
  where
    newline = "\n"
