module Handler.Task where

import           App
import           Auth               (AuthUser (..))
import           Data.Type.Map      as TM
import           Handler.Middleware
import           Service.Task
import           Type               as T
import           View

getTasksR :: AuthUser -> AppM [TaskResponse]
getTasksR au = do
  me <- authUserM au
  xs <- getTasks me
  loaded <- snd <$> loadTasksRelation xs TM.Empty
  runViewM $ mapM (renderTask loaded) xs