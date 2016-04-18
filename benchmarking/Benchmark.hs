import Distribution.Compat.ReadP
import Distribution.Package (Dependency)
import Distribution.Text (Text(..), simpleParse)

comments :: ReadP r ()
comments = do
    _ <- string "--"
    _ <- manyTill (satisfy (const True)) (char '\n')
    return ()

parser :: ReadP r [Dependency]
parser = do
    skipMany comments
    _ <- string "constraints:"
    many1 (skipSpaces *> parse <* optional (char ','))

newtype CabalConfig = CabalConfig [Dependency]
  deriving (Eq, Read, Show)

instance Text CabalConfig where
  disp = undefined
  parse = CabalConfig <$> parser

main :: IO ()
main = do
    cnf <- readFile $! "cabal.config"
    case simpleParse cnf :: Maybe CabalConfig of
         Nothing -> error "wat"
         Just cc -> print cc
