-- | 
-- Module: Language
-- Description: Common patterns parser functions of the game language. 
module Parser.Text.Language (module Parser.Text.Language, module Extensions.Parsec) where

import Extensions.Parsec

-- | Parse many elements in a section: @\<SectionName \\n {many parser} \\n />@.
--
-- ==== __Examples__
-- 
-- @
--  sectionMany \"TokenName\" (spaces *> string "bb" <* spaces) will parse:
--  <TokenName
--      bb bb bb 
--      bb
--  />
-- and returns [\"bb\", \"bb\", \"bb\", \"bb\"]
-- @
--
sectionMany :: String -> CharParser () a -> CharParser () [a]
sectionMany name parser = beginToken name *> parser `manyTill` tryEndToken

-- | Parse many elements in a section with key: @\<SectionName \n {key parser} {many parser} />@.
--
-- ==== __Examples__
-- 
-- @
--  sectionManyWithKey \"TokenName\" (spaces *> string "bb" <* spaces) (string \"key=1\n") will parse:
--  <TokenName
--      key=1
--      bb bb bb 
--      bb
--  />
-- and returns (\"key=1\n\", ["bb", \"bb", \"bb", \"bb"])
-- @
--
sectionManyWithKey :: String -> CharParser () a -> CharParser () key -> CharParser () (key, [a])
sectionManyWithKey name prsr prsrKey = (,) <$> (beginToken name *> prsrKey) <*> (prsr `manyTill` tryEndToken)

-- | Parse element in a section with key: @\<SectionName \n {key parser} {parser} />@.
--
-- ==== __Examples__
-- 
-- @
--  sectionWithKey \"TokenName\" (spaces *> string "bb" <* spaces) (string \"key=1\n") will parse:
--  <TokenName
--      key=1
--      bb
--  />
-- and returns ("key=1\n", \"bb")
-- @
--
sectionWithKey :: String -> CharParser () a -> CharParser () key -> CharParser () (key, a)
sectionWithKey name parser parserKey = (,) <$> (beginToken name *> parserKey) <*> (parser <* endToken)

-- | Parse element in a section \<SectionName \n {parser} />.
--
-- ==== __Examples__
-- 
-- @
--  section \"TokenName\" (spaces *> string "bb" <* spaces) will parse:
--  <TokenName
--      bb
--  />
-- and returns \"bb"
-- @
--
section :: String -> CharParser () a -> CharParser () a
section name parser = beginToken name *> parser <* endToken

-- | Parse element in an inline section @SectionName \\n {parser} />@.
--
-- ==== __Examples__
-- 
-- @
--  section \"TokenName\" (spaces *> string "bb" <* spaces) will parse:
--  <TokenName
--      bb
--  />
-- and returns \"bb"
-- @
--
inlineSection :: String -> CharParser () [String]
inlineSection name = skipOptionName name *> sepBy quotaString spacebs1 <* spacebsAndEol'

-- | Parse a begin token: @\<TokenName@, even if surrounded by spaces. It must end with the end-of-line string. It discards result.
beginToken :: String -> CharParser () ()
beginToken name = spaces *> string' ("<" ++ name) *> spacebsAndEol'

-- | Parse the end token: @/>@. It discards the spaces before it.
endToken :: CharParser () String
endToken = spaces *> string "/>"

-- | tryEndToken = 'try' 'endToken'
tryEndToken :: CharParser () String
tryEndToken = try endToken

-- | Parse an option name and discards results. OptionName = .
--
-- ==== __Examples__
-- 
-- @
--  skipOptionName \"TokenName\" will parse:
--  TokenName    =    
-- @
--
skipOptionName :: String -> CharParser () ()
skipOptionName name = do 
    spaces
    string' name
    spacebs'
    _ <- char '='
    spacebs'

-- | Parse an option value. OptionName = Value. It must end with the end-of-line string.
--
-- ==== __Examples__
-- 
-- @
--  readOption \"TokenName\" will parse:
--  TokenName    = \"Value"
--  and returns \"Value"   
-- @
--
readOption :: String -> CharParser () String
readOption name = skipOptionName name *> quotaString <* spacebsAndEol'

-- | Parse a string between quotas. Quatas in the string are not allowed.
--
-- ==== __Examples__
-- 
-- @
--  quotaString will parse:
--  \"Value S"
--  and returns \"Value S"   
-- @
--
quotaString :: CharParser () String
quotaString = between (char '"') (char '"') (many (noneOf "\""))

-- | The same as with 'quataString' but discards preceding spaces and the stream must end with the eol string.
readValueName ::CharParser () String
readValueName = spaces *> quotaString <* spacebsAndEol'

-- | Read a number. For non-number returns 0.
readInt :: CharParser () Int
readInt = read <$> many alphaNum

-- | the same as with 'readOption' but parses number instead.
readOptionInt :: String -> CharParser () Int
readOptionInt name = skipOptionName name *> readInt <* spacebsAndEol'

tryOptionInt :: String -> CharParser () Int
tryOptionInt = try . readOptionInt

-- | Try parsing @b@ and transform it to @c@ and then put it to tupple @(a, c)@.
tryToTV :: (b -> c) -> a -> CharParser () b -> CharParser () (a, c)
tryToTV v t r = (,)t . v <$> try r

-- | Try parsing choice of 'tryToTV's.
choiceToTV :: [(a, CharParser () c)] -> CharParser () (a, c)
choiceToTV [] = error "Can't be empty!"
choiceToTV xs = choice $ (uncurry (tryToTV id)) <$> xs

string' :: String -> CharParser () ()
string' str = string str *> return ()