module Config.Parser(
    parseConfig
) where

import Control.Applicative ( some )
import qualified Data.Map.Strict as M

import Text.Parsec.String ( Parser, parseFromFile )
import Text.Parsec.Char
import Text.Parsec
import Config.Types


-- parse parseIniLine "" "cluster=8080, 8081, 8082, 8083"
parseIniLine :: Parser (String, [Integer])
parseIniLine = do
  spaces
  name <- string "port" <|> string "neighbours"
  skipMany1 space
  _ <- char '='
  skipMany1 space
  val <- case name of
      "port" -> do
        initPort <- parseCommaSeparatedPort
        return [initPort]
      "neighbours" -> parseCommaSeparatedPorts
      _ -> return []
  skipMany (oneOf "\n")
  return (name, val)

parseIniLines :: Parser [(String, [Integer])]
parseIniLines = some parseIniLine

parseCommaSeparatedPort :: Parser Integer
parseCommaSeparatedPort = do
  spaces
  initPort <- many1 digit
  skipMany (oneOf ", ")
  return (read initPort)

parseCommaSeparatedPorts :: Parser [Integer]
parseCommaSeparatedPorts = some parseCommaSeparatedPort

parseConfig' :: Parser InitConfig
parseConfig' = do
  configLines <- parseIniLines
  let configMap = M.fromList configLines
      initPort = head $ M.findWithDefault [0] "port" configMap
      initNeighbours = filter (/= initPort) $ M.findWithDefault [] "neighbours" configMap in
    return $ initConfig initPort initNeighbours

parseConfig :: IO (Either ParseError InitConfig)
parseConfig = parseFromFile parseConfig' "config.ini"
