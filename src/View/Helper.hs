module View.Helper (
  module View.Helper
) where

-- | Monadic version of fmap
fmapMaybeM :: (Monad m) => (a -> m b) -> Maybe a -> m (Maybe b)
fmapMaybeM _ Nothing  = return Nothing
fmapMaybeM f (Just x) = Just <$> f x

-- getUserAge :: (MonadView m) => User.User -> m Int
-- getUserAge User.User {User.birthday = x} = do
--   now <- getCurrentTime
--   return $ ageAt now x