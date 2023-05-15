import Data.Ratio
import Data.List (permutations)
import Control.Monad (liftM2)
import System.IO (stderr, hPutStrLn)

main :: IO ()
main = do
  xs <- map read . words <$> getLine
  case xs of
    [a, b, c, d] -> do
      let ans1 = solve a b c d
      let ans2 = [ ans | [a', b', c', d'] <- permutations [a, b, c, d], ans <- solve a' b' c' d' ]
      let ans = if null ans1 then ans2 else ans1
      mapM_ print $ ans
    _ -> hPutStrLn stderr "invalid input"

solve :: Int -> Int -> Int -> Int -> [Expr]
solve a b c d = [ expr | op1 <- operators,
                         op2 <- operators,
                         op3 <- operators,
                         expr <- expressions a op1 b op2 c op3 d,
                         eval expr == Just 10 ]

type N = Maybe (Ratio Int)
data Op = Op String (N -> N -> N)

instance Show Op where
  show (Op op _) = op

operators :: [Op]
operators = [
  Op "+" (liftM2 (+)),
  Op "-" (liftM2 (-)),
  Op "*" (liftM2 (*)),
  Op "/" (\a b -> if b == Just 0 then Nothing else liftM2 (/) a b) ]

data Expr = Num Int | BinOp Op Expr Expr
instance Show Expr where
  show (Num n) = show n
  show (BinOp op lhs rhs) = "(" ++ show lhs ++ " " ++ show op ++ " " ++ show rhs ++ ")"

expressions :: Int -> Op -> Int -> Op -> Int -> Op -> Int -> [ Expr ]
expressions a op1 b op2 c op3 d = [
  ((Num a `op1'` Num b) `op2'` Num c) `op3'` Num d,
  (Num a `op1'` (Num b `op2'` Num c)) `op3'` Num d,
  (Num a `op1'` Num b) `op2'` (Num c `op3'` Num d),
  Num a `op1'` ((Num b `op2'` Num c) `op3'` Num d),
  Num a `op1'` (Num b `op2'` (Num c `op3'` Num d)) ]
  where (op1', op2', op3') = (BinOp op1, BinOp op2, BinOp op3)

eval :: Expr -> N
eval (Num n) = Just $ n % 1
eval (BinOp (Op _ op) lhs rhs) = eval lhs `op` eval rhs
