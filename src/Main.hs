module Main where

    import System.Environment

    import JavaEngine

    main :: IO Int
    main = do
    	args <- getArgs
    	bootup args