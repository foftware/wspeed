module Grid where

import Control.Comonad.Cofree (Cofree, (:<))
import Data.Map (Map, empty)
import Data.Set (Set, fromFoldable)
import Prelude (class Eq, class Ord, compare, eq)


data RowF a 
    = Gap a
    | Word String a
    | End 

type RowData a = Cofree RowF a

data RowMeta = RowMeta Int (Map String Int)

data Row a = Row RowMeta (RowData a)

instance eqRow :: Eq a => Eq (Row a) where
    eq (Row (RowMeta i1 _) _) (Row (RowMeta i2 _) _) = eq i1 i2

instance ordRow :: Ord a => Ord (Row a) where
    compare (Row (RowMeta i1 _) _) (Row (RowMeta i2 _) _) = compare i1 i2

type Grid a = Set (Row a)

grid :: Grid Int
grid = fromFoldable 
    [ row1 
    ]
  where
    row1 = 
        let rowData = 0 :< End
        in Row (RowMeta 1 empty) rowData
