module Service.TaskSpec where

import           Entity
import           Mock
import           Query        as Q
import           Service.Task
import           Test.Hspec

spec :: Spec
spec =
  describe "getTasks" $
    it "success" $ do
      let tasks = [Task 99 "test" 0]
      ts <- runTestM $
        runMockT [QueryM selectTasks 0 :-> tasks] $ getTasks (User 0)
      ts `shouldBe` tasks