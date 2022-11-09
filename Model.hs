module Model (TextModel
             ,createModel
             ,nextDistribution) where
import Control.Monad
import Data.List
import Control.Arrow
import Data.Maybe
import qualified Data.Map as Map
import Data.Map (Map)
import NGram


-- The type for our Markov process text model.
type TextModel = Map NGram (Map Char Weight , Weight)

-- The empty model with no n-grams.
emptyModel :: TextModel
emptyModel = Map.empty


-- Update a model with a new n-gram followed by a character.
increaseWeight :: NGram -> Char -> TextModel -> TextModel
increaseWeight ngram next model = do
    let values = fromMaybe (Map.empty, 0) (Map.lookup ngram model)
        charm = fst values
        weights = fromMaybe 0 $ Map.lookup next charm
        charm2 = Map.insertWith (+) next 1 charm
        total = snd values
    Map.insert ngram (charm2, total+1) model

-- The distribution of next n-grams after a given one.

nextDistribution :: TextModel -> NGram -> Maybe ([(NGram,Weight)],Weight)
nextDistribution model current = 
    case Map.lookup current model of
        Nothing -> Nothing
        Just values -> Just ((map (\(x,y) -> (updateGram current x, y)) (Map.toList (fst values))), (snd values))
    

-- Create an n-gram model from a string.

createModel :: Integer -> String -> TextModel
createModel n s = foldl' (\model (ngram,next) -> increaseWeight ngram next model) Map.empty (gramsWithNext n s)









