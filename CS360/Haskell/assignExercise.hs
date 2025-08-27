type Assoc k v = [(k,v)]
type Var = Char
type Env = Assoc Var Int
find :: Eq k => k -> Assoc k v -> v
find k t = head [v | (k',v) <- t, k == k']

insert :: Char -> Int -> Env -> Env
insert k v [] = [(k,v)]
insert k v t  = (k,v):t
-- Exercise:  Modify to change value of existing binding rather than insert new binding
insert k v t = do a <- insert k v
                  b <- t
                  a:b

data ArithExpr = Const Int
               | Var Var
               | Add ArithExpr ArithExpr
               | Mult ArithExpr ArithExpr
               deriving Show

eval :: ArithExpr -> Env -> Int
eval (Const a) _ = a
eval (Var x) t = find x t
eval (Add e1 e2) t = (eval e1 t) + (eval e2 t)
eval (Mult e1 e2) t = (eval e1 t) * (eval e2 t)

-- The state monad

type State = Env

newtype ST a = S (State -> (a,State))

app :: ST a -> State -> (a,State)
app (S st) x = st x

instance Functor ST where
   -- fmap :: (a -> b) -> ST a -> ST b
   fmap g st = S (\s -> let (x,s') = app st s in (g x, s'))

instance Applicative ST where
   -- pure :: a -> ST a
   pure x = S (\s -> (x,s))
   
   -- (<*>) :: ST (a -> b) -> ST a -> ST b
   stf <*> stx = S (\s ->
       let (f,s')  = app stf s
           (x,s'') = app stx s' in (f x, s''))

instance Monad ST where
   -- (>>=) :: ST a -> (a -> ST b) -> ST b
   st >>= f = S (\s -> let (x,s') = app st s in app (f x) s')


data Op = ASSIGN Var ArithExpr
type Code = [Op]

updateST :: Var -> Int -> ST ()
updateST key value = S (\env -> ((), insert key value env))

evalST :: ArithExpr -> ST Int
evalST expr = S (\env -> (eval expr env, env))

exec :: Code -> ST ()
-- Exercise:  implement with do notation. 

env :: Env
env = [('a',2),('b',3)]

code :: Code
code = [ASSIGN 'x' (Const 1), ASSIGN 'y' (Add (Var 'x') (Const 1)), ASSIGN 'z' (Add (Var 'x') (Var 'y')),ASSIGN 'x' (Const 0)]

{-

ghci> app (exec code) []
((),[('x',0),('z',3),('y',2),('x',1)])

-}