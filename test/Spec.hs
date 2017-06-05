import Test.Hspec

import qualified Mikan as Mn
import Mikan (Lex(L, A))

main :: IO ()
main = hspec lexerTest

lexerTest :: Spec
lexerTest = describe "Lexical Analysis: " $ do
  it ("a b c = " ++ show (L [A "a", A "b", A "c"])) $
    Mn.lexer "a b c" `shouldBe`  L [A "a", A "b", A "c"]
  it ("a (b c) = " ++ show (L [A "a", L [A "b",A "c"]])) $
    Mn.lexer "a (b c)" `shouldBe` L [A "a", L [A "b",A "c"]]
  it ("(a b) c = " ++ show  (L [L [A "a", A "b"], A "c"]))$
    Mn.lexer "(a b) c" `shouldBe` L [L [A "a", A "b"], A "c"]
  it "a b c = (a) (b) (c)" $
    Mn.lexer "(a) (b) (c)" `shouldBe` Mn.lexer "a b c"
