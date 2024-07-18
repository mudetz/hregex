module Parser where

import Control.Applicative
import Data.Char

-- Type Definitions

data RegexValue
  = Quantifier Int (Maybe Int)
  | CharRange Char Char
  | Brackets Bool [RegexValue]
  | Alternation RegexValue RegexValue
  | Group [RegexValue]
  | Wildcard
  | Start
  | End
  | Literal Char
  deriving (Eq, Show)

newtype Parser a = Parser
  { runParser :: String -> Maybe (String, a)
  }

-- Class Proofs
instance Functor Parser where
  fmap f (Parser p) =
    Parser $ \input -> do
      (input', x) <- p input
      return (input', f x)

instance Applicative Parser where
  pure a = Parser $ \input -> Just (input, a)
  (Parser p1) <*> (Parser p2) =
    Parser $ \input -> do
      (input',  f) <- p1 input
      (input'', a) <- p2 input'
      return (input'', f a)

instance Alternative Parser where
  empty = Parser $ const Nothing
  (Parser p1) <|> (Parser p2) = Parser f
    where f input = p1 input <|> p2 input

-- Primitive Parsers
charP :: Char -> Parser Char
charP c = condP (== c)

anyCharP :: Parser Char
anyCharP = (charP '\\' *> condP (const True)) <|> condP (\c -> not $ elem c "[")

stringP :: String -> Parser String
stringP = sequenceA . map charP

condP :: (Char -> Bool) -> Parser Char
condP p = Parser f
  where
    f (x:xs)
      | p x = return (xs, x)
      | otherwise = Nothing
    f [] = Nothing

exceptCharP :: Char -> Parser Char
exceptCharP c = (charP '\\' *> condP (const True)) <|> condP (/= c)

spanP :: (Char -> Bool) -> Parser String
spanP f = many $ condP f

whitespaceP :: Parser String
whitespaceP = spanP isSpace

intP :: Parser Int
intP = fmap read $ (:) <$> (condP $ flip elem "123456789") <*> spanP isDigit

sepByP :: Parser a -> Parser b -> Parser [b]
sepByP sepP elemP = (:) <$> elemP <*> many (sepP *> elemP) <|> pure []

-- Regex Parsers
-- TODO: Force non-quantifier expressions before quantifiers
optionalQuantifierP :: Parser RegexValue
optionalQuantifierP = (const $ Quantifier 0 (Just 1)) <$> charP '?'

starQuantifierP :: Parser RegexValue
starQuantifierP = (const $ Quantifier 0 Nothing) <$> charP '*'

plusQuantifierP :: Parser RegexValue
plusQuantifierP = (const $ Quantifier 1 Nothing) <$> charP '+'

bracesExactQuantifierP :: Parser RegexValue
bracesExactQuantifierP = (\x -> Quantifier x (Just x))<$>
  (charP '{' *> whitespaceP *> intP <* whitespaceP <* charP '}')

bracesRangeQuantifierP :: Parser RegexValue
bracesRangeQuantifierP = charP '{' *> whitespaceP *> intRange <* whitespaceP <* charP '}'
  where intRange = (\a _ b -> Quantifier a (Just b))
               <$> intP <*> (whitespaceP *> charP ',' <* whitespaceP) <*> intP

bracesLeftUnboundRangeQuantifierP :: Parser RegexValue
bracesLeftUnboundRangeQuantifierP = charP '{' *> whitespaceP *> intRange <* whitespaceP <* charP '}'
  where intRange = (\_ b -> Quantifier 0 (Just b))
               <$> (whitespaceP *> charP ',' <* whitespaceP) <*> intP

bracesRightUnboundRangeQuantifierP :: Parser RegexValue
bracesRightUnboundRangeQuantifierP = charP '{' *> whitespaceP *> intRange <* whitespaceP <* charP '}'
  where intRange = (\a _ -> Quantifier a Nothing)
               <$> intP <*> (whitespaceP *> charP ',' <* whitespaceP)

bracesQuantifierP :: Parser RegexValue
bracesQuantifierP = bracesExactQuantifierP
                <|> bracesRangeQuantifierP
                <|> bracesLeftUnboundRangeQuantifierP
                <|> bracesRightUnboundRangeQuantifierP

quantifierP :: Parser RegexValue
quantifierP = optionalQuantifierP
          <|> starQuantifierP
          <|> plusQuantifierP
          <|> bracesQuantifierP

-- NOTE: This need to be fixed: This supports invalid ranges, e.g: [z-a]
bracketCharRangeP :: Parser RegexValue
bracketCharRangeP = (\a _ b -> CharRange a b) <$> anyCharP <*> charP '-' <*> anyCharP

bracketLiteral :: Parser RegexValue
bracketLiteral = fmap Literal $ exceptCharP ']'

inclusiveBracketP :: Parser RegexValue
inclusiveBracketP = (\xs -> Brackets False xs) <$> (charP '[' *> elements <* charP ']')
  where
    elements = many element
    element  = bracketCharRangeP <|> bracketLiteral

exclusiveBracketP :: Parser RegexValue
exclusiveBracketP = (\xs -> Brackets True xs) <$> (charP '[' *> charP '^' *> elements <* charP ']')
  where
    elements = many element
    element  = bracketCharRangeP <|> bracketLiteral

bracketP :: Parser RegexValue
bracketP = exclusiveBracketP <|> inclusiveBracketP

groupP :: Parser RegexValue
groupP = fmap Group $ (charP '(' *> elements <* charP ')')
  where
    elements = many
             $ noLiteralP
           <|> (fmap Literal $ exceptCharP ')')

-- TODO: Allow N alternation
-- INFO: Not done as it causes an infinite loop + stack overflow
alternationP :: Parser RegexValue
alternationP = (\a _ b -> Alternation a b) <$> nonAlternationP <*> charP '|' <*> nonAlternationP
  where nonAlternationP = quantifierP
                     <|> bracketP
                     <|> groupP
                     <|> wildcardP
                     <|> literalP

literalP :: Parser RegexValue
literalP = fmap Literal $ exceptCharP '|'

wildcardP :: Parser RegexValue
wildcardP = const Wildcard <$> charP '.'

-- Regex Parser Composition
noLiteralP :: Parser RegexValue
noLiteralP = alternationP
         <|> quantifierP
         <|> bracketP
         <|> groupP
         <|> wildcardP

regexP :: Parser [RegexValue]
regexP = many
       $ noLiteralP
     <|> literalP

parseRegex :: String -> Parser RegexValue
parseRegex = undefined
