module MITrans where

class (Monad m) => MonadMInput m where
  getArtLim :: m (Artist, Limit)
  showLink :: String -> m ()

newtype MusicInput = MInput

newtype MInputT m a = 