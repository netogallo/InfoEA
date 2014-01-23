module GraphParser where

import Text.ParserCombinators.UU
import Text.ParserCombinators.UU.Utils
import Text.ParserCombinators.UU.BasicInstances as BI
import System.IO

type State = (Str Char [Char] LineCol) 

pI = (*> pure ())

lineColState input = Str{
  input=input,
  msgs=[],
  pos=LineCol 1 1,
  deleteOk=True} 

pNil :: P State ()
pNil = pI pInteger <|> (pI pComma) <|> (pI $ pSym '.') <|> (pI $ pSym ' ')

pBlanks = pMany $ pSym ' '

vertexParser :: P State (Int,[Int])
vertexParser = (\i vx -> (i,vx)) <$>
               pInteger
               <* pBlanks
               <* ((pParens (pMany pNil)) *> pBlanks *> (pIntegerRaw :: P State Int))
               <* pBlanks
               <*> pMany (pIntegerRaw <* (pMany $ pSym ' '))

graphParser = (\x y -> (x,y)) <$> pMany (pSpaces *> vertexParser <* pSym '\n') <*> pEnd

parseGraph = parse graphParser . lineColState

readGraph f = withFile f ReadMode $ \h -> do
  (g,e) <- hGetContents h >>= return . parseGraph
  case length g of
    0 -> return g
    _ -> return $ id g
