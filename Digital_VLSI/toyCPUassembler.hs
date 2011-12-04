import Control.Applicative
import Numeric

data Mnemonic = ADDA
             | MOVAB
             | INA
             | MOVA
             | MOVBA
             | ADDB
             | INB
             | MOVB
             | OUTB
             | OUT
             | JMP
             | JNC
  deriving (Eq, Show, Read)

parseProgram = map lineParse
  where
    lineParse :: [String] -> (Mnemonic, Int)
    lineParse (x:y:_) = (read x, read y)

assemble :: Mnemonic -> Int
assemble ADDA  = 0
assemble MOVAB = 1
assemble INA   = 2
assemble MOVA  = 3
assemble MOVBA = 4
assemble ADDB  = 5
assemble INB   = 6
assemble MOVB  = 7
assemble OUTB  = 8
assemble OUT   = 9
assemble JMP   = 10
assemble JNC   = 11

formatProgram (op, i) = "8'h" ++ (showBin $ opi*0x10 + i)
  where
    opi = assemble op
    showBin x = showHex x ""

main = do
    pList <- (parseProgram.(map words).lines) <$> getContents
    let out = zip [0..] (map formatProgram pList)
    mapM_ (\(i, op) ->
        putStrLn $ "q[4'h" ++ (showHex i "") ++ "] <= " ++ op ++ ";"
        ) out
