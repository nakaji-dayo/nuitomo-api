module Util where

import           Control.Lens
import qualified Data.ByteString.Char8      as B8
import qualified Data.ByteString.Lazy.Char8 as LB8
import qualified Data.Map                   as M
import           Data.Monoid
import           Data.Text                  (Text, append, pack)

-- |
-- last of empty list
-- >>> lastMaybe [1, 2, 3]
-- Just 3
lastMaybe :: [a] -> Maybe a
lastMaybe [] = Nothing
lastMaybe xs = Just $ last xs

-- |
-- map fst of tuple
-- >>> mapFst (*3) (1,2)
-- (3,2)
mapFst :: (t -> a) -> (t, b) -> (a, b)
mapFst f (a, b) = (f a, b)

-- |
-- map snd of tuple
-- >>> mapSnd (*3) (1,2)
-- (1,6)
mapSnd :: (t -> b) -> (a, t) -> (a, b)
mapSnd f (a, b) = (a, f b)

fst3 :: (a, b, c) -> a
fst3 (a, _, _) = a

snd3 :: (a, b, c) -> b
snd3 (_, a, _) = a

third3 :: (a, b, c) -> c
third3 (_, _, a) = a

fst4 :: (a, b, c, d) -> a
fst4 (a, _, _, _) = a

snd4 :: (a, b, c, d) -> b
snd4 (_, a, _, _) = a

third4 :: (a, b, c, d) -> c
third4 (_, _, a, _) = a

-- |
-- construct Data.Map from List.
-- the value of duplicated key is to be list.
-- >>> createMapWithListValue [(1, 2), (2, 3), (1, 4)]
-- fromList [(1,[2,4]),(2,[3])]
createMapWithListValue :: Ord a => [(a, b)] -> M.Map a [b]
createMapWithListValue = M.fromListWith (flip (++)) . map (mapSnd pure)


mapView :: Getting b s b -> Optic' (->) (Const (First [b])) [s] [b]
mapView = to . fmap . view

tshow :: Show a => a -> Text
tshow = pack . show

lbshow:: Show a => a -> LB8.ByteString
lbshow = LB8.pack . show

bshow:: Show a => a -> B8.ByteString
bshow = B8.pack . show

(++%) :: Text -> Text -> Text
(++%) = append

-- sbl = sb { quoteExp = fmap (AppE (VarE 'BSL.fromStrict)) . quoteExp sb}

whenJust :: (Applicative m) =>
            Maybe a -> (a -> m ()) -> m ()
whenJust = flip $ maybe (pure ())