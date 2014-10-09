module ParsersAndCandies where

import Control.Monad


newtype Parser a = Parser (String -> [(a, String)])

parse (Parser pars) = pars

parseAndPeel p cs = a where (a, _) = head $ parse p cs

instance Monad Parser where
    return ax = Parser (\cs -> [(ax, cs)])
    p >>= f = Parser (\cs -> concat [parse (f ax) cs' |
                               (ax, cs') <- parse p cs])


instance MonadPlus Parser where
    mzero = Parser (\_ -> [])
    mplus p f = Parser (\cs -> parse p cs ++ parse f cs)

(+++) :: Parser a -> Parser a -> Parser a
p +++ q = Parser (\cs -> case parse (mplus p q) cs of
                                [] -> []
                                (x:xs) -> [x])

item :: Parser Char
item = Parser (\cs -> case cs of
                        "" -> []
                        (c:cs) -> [(c,cs)])

sat :: (Char -> Bool) -> Parser Char
sat predicate = do{c <- item; if predicate c then return c else mzero}

char :: Char -> Parser Char
char predCh = sat (predCh==)

string :: String -> Parser String
string "" = return ""
string (queryC:queryCs) = do {char queryC; string queryCs; return (queryC:queryCs)}

many :: Parser a -> Parser [a]
many p = many1 p +++ return []

many1 :: Parser a -> Parser [a]
many1 p = do {a <- p; as <- many p; return (a:as)}

sepby :: Parser a -> Parser b -> Parser [a]
p `sepby` sep = (p `sepby1` sep) +++ return []

sepby1 :: Parser a -> Parser b -> Parser [a]
p `sepby1` sep = do
                    a <- p
                    as <- many (do{sep; p})
                    return (a:as)
chainl :: Parser a -> Parser (a -> a -> a) -> a -> Parser a
chainl p op ax = (p `chainl1` op) +++ return ax

chainl1 :: Parser a -> Parser (a -> a -> a) -> Parser a
chainl1 p op = do {a <- p; rest a}
                where rest a = (do
                                    f <- op
                                    b <- p
                                    rest (f a b))
                                +++ return a
