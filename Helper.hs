module Helper where

end :: String -> IO a
end e = ioError(userError e)