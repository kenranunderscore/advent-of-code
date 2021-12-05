import qualified Day01Spec
import Test.Hspec

main :: IO ()
main = do
  hspec spec

spec :: Spec
spec = do
  Day01Spec.spec
