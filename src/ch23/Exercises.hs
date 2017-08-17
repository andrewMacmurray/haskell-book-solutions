module Ch23.Exercises where


newtype State s a =
  State { runState :: s -> (a, s) }


get :: State s s
get = State (\s -> (s, s))


put :: s -> State s ()
put s = State (\_ -> ((), s))


exec :: State s a -> s -> s
exec (State sa) s = snd (sa s)


eval :: State s a -> s -> a
eval (State sa) s = fst (sa s)


modify :: (s -> s) -> State s ()
modify f = State (\s -> ((), f s))
