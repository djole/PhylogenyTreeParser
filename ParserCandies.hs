module ParserCandies where

import Control.Monad


--- Type definitions and instantiation of the Monad and MonadPlus
--
newtype Parser a = Parser (String -> [(a, String)])

instance Monad Parser where
    return ax = Parser (\cs -> [(ax, cs)])
    p >>= f = Parser (\cs -> concat [parse (f ax) cs' |
                               (ax, cs') <- parse p cs])

instance MonadPlus Parser where
    mzero = Parser (\_ -> [])
    mplus p f = Parser (\cs -> parse p cs ++ parse f cs)



-- The method that applies the parser to the string, i.e. the Monad unwrapper
parse (Parser pars) = pars

-- The method that applies the parser to the string, takes first result of
-- the parsing, and returns the parsing result
parseAndPeel p cs = a where (a, _) = head $ parse p cs

-- Apply two parsers and return the result of first parser, if sucsessful.
-- If not, return the result of the second parser.
(+++) :: Parser a -> Parser a -> Parser a
p +++ q = Parser (\cs -> case parse (mplus p q) cs of
                                [] -> []
                                (x:xs) -> [x])

-- Parse the first character of the string
item :: Parser Char
item = Parser (\cs -> case cs of
                        "" -> []
                        (c:cs) -> [(c,cs)])

-- If the first character satisfies the predicate, parse it. Fail otherwise.
sat :: (Char -> Bool) -> Parser Char
sat predicate = do{c <- item; if predicate c then return c else mzero}

char :: Char -> Parser Char
char predCh = sat (predCh==)

string :: String -> Parser String
string "" = return ""
string (queryC:queryCs) = do {char queryC; string queryCs; return (queryC:queryCs)}

-- Apply the parser any amount of times
many :: Parser a -> Parser [a]
many p = many1 p +++ return []

-- Apply the parser p at least once
many1 :: Parser a -> Parser [a]
many1 p = do {a <- p; as <- many p; return (a:as)}

-- Apply parser p, then parser sep and discard the result of sep any amount of
-- times.
sepby :: Parser a -> Parser b -> Parser [a]
p `sepby` sep = (p `sepby1` sep) +++ return []

-- The same as previous, only apply at least once
sepby1 :: Parser a -> Parser b -> Parser [a]
p `sepby1` sep = do
                    a <- p
                    as <- many (do{sep; p})
                    return (a:as)

-- Apply the parser p separated with parser op that extracts a binary operator
-- that is then applied to two results of p parser, any number of time.
chainl :: Parser a -> Parser (a -> a -> a) -> a -> Parser a
chainl p op ax = (p `chainl1` op) +++ return ax

-- Same as previous, only it has to be applied at least once
chainl1 :: Parser a -> Parser (a -> a -> a) -> Parser a
chainl1 p op = do {a <- p; rest a}
                where rest a = (do
                                    f <- op
                                    b <- p
                                    rest (f a b))
                                +++ return a
