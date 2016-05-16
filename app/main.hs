import           ClassyPrelude
import           Data.Text (Text)
import           Yesod

data App = App

mkYesod "App" [parseRoutes|
/add/#Int/#Int    AddR GET
/sub/#Int/#Int    SubR GET
/mult/#Int/#Int   MultR GET
/div/#Int/#Int    DivR GET
|]

instance Yesod App 

getAddR :: Int -> Int -> Handler TypedContent
getAddR x y = selectRep $ do
    provideRep $ return
        [shamlet|
            #{sum}
        |]
    provideJson sum
  where
    sum = x + y 

getSubR :: Int -> Int -> Handler TypedContent
getSubR x y = selectRep $ do
    provideRep $ return
        [shamlet|
            #{sum}
        |]
    provideJson sum
  where
    sum = x - y 

getMultR :: Int -> Int -> Handler TypedContent
getMultR x y = selectRep $ do
    provideRep $ return
        [shamlet|
            #{sum}
        |]
    provideJson sum
  where
    sum = x * y 

getDivR :: Int -> Int -> Handler TypedContent
getDivR x y = selectRep $ do
    provideRep $ return
        [shamlet|
            #{sum}
        |]
    provideJson sum
  where
    divide x y = (fromIntegral x) / (fromIntegral y)
    divide :: Int -> Int -> Float
    sum = divide x y
     

main :: IO ()
main = warp 3000 App
