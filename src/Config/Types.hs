module Config.Types(
    InitConfig(InitConfig)
  , initConfig
) where

data InitConfig = InitConfig {
    port :: Integer
  , neighbours :: [Integer]
} deriving Show

initConfig :: Integer -> [Integer] -> InitConfig
initConfig = InitConfig
