module NormalForm where


-- data Fiction = Fiction deriving Show
-- data NonFiction = NonFiction deriving Show
--
-- data BookType =
--     FictionBook Fiction
--   | NonFictionBook NonFiction
--   deriving Show

type AuthorName = String
-- non normal form
-- data Author = Author (AuthorName, BookType)

-- same as Author type but in normal form
data AuthorNormal =
    Fiction AuthorName
  | NonFiction AuthorName
  deriving (Eq, Show)


-- data FlowerType =
--     Gardenia
--   | Daisy
--   | Rose
--   | Lilac
--   deriving Show

type Gardener = String

-- non normal form
-- data Garden =
--   Garden Gardener FlowerType
--   deriving Show

data Garden =
    Gardenia Gardener
  | Daisy Gardener
  | Rose Gardener
  | Lilac Gardener
  deriving Show
