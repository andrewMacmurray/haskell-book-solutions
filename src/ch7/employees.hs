module Reports where

data Employee =
    Coder
  | Manager
  | Veep
  | CEO
  deriving (Eq, Ord, Show)

reportBoss :: Employee -> Employee -> IO ()
reportBoss e e' =
  putStrLn $ show e ++ " is the boss of " ++ show e'

codersRuleCEOsDrool :: Employee -> Employee -> Ordering
codersRuleCEOsDrool e e' =
  case (e, e') of
    (Coder, Coder) -> EQ
    (Coder, _)     -> GT
    (_, Coder)     -> LT
    (_, _)         -> compare e e'

employeeRank :: (Employee -> Employee -> Ordering)
  -> Employee
  -> Employee
  -> IO ()
employeeRank f e e' =
  case f e e' of
    GT -> reportBoss e e'
    EQ -> putStrLn "neither is in charge of one another"
    LT -> (flip reportBoss) e e'
