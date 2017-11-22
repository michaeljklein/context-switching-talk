
```
% % Ok, so we have a nice background on Functors, Applicatives, Monads, Applicatives, and how they apply to parsing.
% % Now what? I'll begin by defining a single parser "by hand" (without all these nice classes) and show
% % what can be done with it.
```

```
% -- charP p = Parser go
% --   where
% --     go [] = Left []
% --     go (c:cs) | p c       = Right (cs, c )
% --               | otherwise = Left  (c : cs)
% --
% -- Accept any Char the predicate passes for.
% --
% -- Here's how this works: charP takes a test or \"predicate\".
% -- When the parser is run, if the test passes for the first character in the string
% -- then return the character and the rest of the string as leftover.
% -- If the test fails or there is no first character, it fails.
```

```
% char :: Char -> Parse Char
% char = charP . (==)
% %
% % this is just
% % charP (== givenCharacter)
```

```
% digit :: Parse Char
% digit = charP isDigit
```

```
% string :: String -> Parse String
% string = mapM char
```

```
% % mapM maps the function and combines all the monadic values inside.
% % Look at what happens when we just use map:
% %
% % map char ['a', 'b', 'c'] = [char 'a', char 'b', char 'c']
% %
% % This isn't too helpful since now we have to somehow run all the char
% % parsers and collect the results. mapM does exactly this.
```

```
% (<<) :: Monad m => m a -> m b -> m a
% (<<) = liftM2 const

% liftM2 f x y = f <\$> x <*> y
% % One more function, this one is also free from return and (>>=):

% parseA >> parseB
% -- parses A then B, but tosses A

% parseA << parseB
% -- parses A then B, but tosses B
```

```
% % This is like (>>), but backwards.
% % liftM2 :: (a -> b -> c) -> Parser a -> Parser b -> Parser c
% % liftM2 func parser1 parser2 = run parser1, run parser2, return their results combined with func
% %
% % So @liftM2 const@ runs both parsers, but @const a b = a@
% % so it tosses the result of the second parser.

% space :: Parse Char
% space = charP isSpace

% % Isn't this pretty?
% inParens :: Parse a -> Parse a
% inParens p = char '(' >> p << char ')'

% -- |
% -- @
% -- word = some \$ charP isAlpha
% -- @
% --
% word :: Parse String
% word = some \$ charP isAlpha


% -- Let's say we wanted to parse class instances of the form:
% --
% -- @
% -- \"Class (Name Type)\"
% -- @
% --
% -- Where \"Name\" is fixed. Then we only need the following two definitions.
% --
% -- Example use:
% --
% -- @
% -- parse classType \"Spoken  (Name Kid)\"      == Right (\"Spoken\", \"Kid\")
% -- parse classType \"Monad   (Name Function)\" == Right (\"Monad\",  \"Function\")
% -- parse classType \"Word1   (Name Word2)\"    == Right (\"Word1\",  \"Word2\")
% -- @


% -- |
% -- @
% -- name = string \"Name " >> word
% -- @
% --
% -- In words: the input must start with \"Name \", but once we know it's there, just toss it and parse a word.
% name :: Parse String
% name = string "Name " >> word

% -- name' = "Name " >> word

% -- |
% -- @
% -- classType = do
% --   className <- word            -- get a word and call it 'className'
% --   many space                   -- get a bunch of spaces, but toss them, they're in the way
% --   typeName  <- inParens name   -- get a name that's in parentheses and call it 'typeName'
% --   return (className, typeName) -- return what you've gotten in a nice tuple
% -- @
% --
% classType :: Parse (String, String)
% classType = do
%   className <- word           -- get a word and call it 'className'
%   many space                  -- get a bunch of spaces, but toss them, they're in the way
%   typeName  <- inParens name  -- get a name that's in parentheses and call it 'typeName'
%   return (className, typeName)     -- return what you've gotten in a nice tuple




% -- Ok, so that wasn't bad. However, it's also not too hard to parse something like that in the first place.
% -- How about parsing a lisp that only supports addition of natural numbers? Huh, that could be neat.
% --
% -- Here are some examples that our result should be able to evaluate:
% --
% -- @
% -- 1                   -> 1
% -- (+ 2 2)             -> 4
% -- (+ (+ 1 2) (+ 3 4)) -> 10
% -- @
% --
% -- For completion sake, we define: @(+ x) -> x@, and make @(+)@ or @()@ a parse error.
% --
% -- First, let's define our expression type (This is a GADT, but that's for another time. It could easily be defined as the following:
% -- @data Expr a = Lit a | Sum [Expr a]@,
% -- but I prefer the GADT style.)

% -- |
% -- @
% -- data Expr a where
% --   Lit ::       a  -> Expr a
% --   Sum :: [Expr a] -> Expr a
% -- @
% data Expr a where
%   Lit ::       a  -> Expr a
%   Sum :: [Expr a] -> Expr a
%   deriving (Show)

% -- |
% -- @
% parseLit :: (Num a, Read a) => Parse (Expr a)
% parseLit = do
%   some space
%   digits <- some digit
%   return . Lit . read \$ digits


% -- parseSum = do
% --   char '('
% --   char \'+\'
% --   subExprs <- some (parseLit <|> parseSum)
% --   char ')'
% --   return \$ Sum subExprs
% -- @
% --
% parseSum :: (Num a, Read a) => Parse (Expr a)

% parseLit2 :: (Num a, Read a) => Parse (Expr a)
% parseLit2 = fmap (Lit . read) \$ some space >> some digit

% -- parseLit3 :: (Num a, Read a) => Parse (Expr a)
% -- parseLit3 = fmap (Lit . read) \$ some " " >> some digit


% parseSum2 :: (Num a, Read a) => Parse (Expr a)
% parseSum2 = fmap Sum \$ string "(+" >> some (parseLit <|> parseSum) << char ')'

% parseSum3 :: (Num a, Read a) => Parse (Expr a)
% parseSum3 = fmap Sum . inParens \$ char '+' >> some (parseLit <|> parseSum)


% -- Even though I don't consider it best practice and it's probably optimized away anyway by Haskell's laziness,
% -- you can do the following to evaluate as you parse:
% parseLit' :: (Num a, Read a) => Parse a
% parseLit' = many space >> fmap read (some digit)

% parseSum' :: (Num a, Read a) => Parse a
% parseSum' = do
%   many space
%   string "(+"
%   subExprs <- some (parseLit' <|> parseSum')
%   char ')'
%   return . sum \$ subExprs

% -- parseSum4 :: (Num a, Read a) => Parse (Expr a)
% -- parseSum4 = fmap Sum \$ "(+" >> some (parseLit <|> parseSum) << ")"

% -- parseSum5 :: (Num a, Read a) => Parse (Expr a)
% -- parseSum5 = do
% --   _ <- "(+"
% --   subExprs <- some (parseLit <|> parseSum)
% --   _ <- ")"
% --   return \$ Sum subExprs

% -- |
% -- @
% -- parseExpr = parseLit <|> parseSum
% -- @
% parseExpr :: (Num a, Read a) => Parse (Expr a)
% parseExpr = parseLit <|> parseSum

% -- Well, that was easy. Lets make a quick evaluator:

% -- |
% -- @
% -- eval (Lit x ) = x
% -- eval (Sum xs) = sum (map eval xs)
% -- @
% --
% eval :: (Num a, Read a) => Expr a -> a
% eval (Lit x ) = x
% eval (Sum xs) = sum (map eval xs)

% -- |
% -- @
% -- evalString = parse \$ fmap eval parseExpr
% -- @
% --
% -- Ok, so that was easy too and it even works with numbers as big as your computer can handle. I'm gonna go off and extend this until it gets hard.
% --
% evalString :: String -> Either String Integer
% evalString = parse \$ fmap eval parseExpr


% data Tree where
%   Branch :: [Tree] -> Tree
%   Leaf   :: Tree


% parseLeaf = string "()" >> return Leaf

% parseBranch = do
%   char '('
%   subTrees <- some parseTree
%   char ')'
%   return . Branch \$ subTrees

% parseTree' = Branch <\$> inParens (some \$ parseLeaf <|> parseTree')

% parseTree = Branch <\$> some (parseLeaf <|> parseBranch)

% data Parens = Parens [Parens] deriving (Eq, Ord, Show)

% parseParens :: Parse Parens
% parseParens = Parens <\$> inParens (many parseParens)
```

