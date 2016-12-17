import           Control.Applicative
import           Network.HTTP
import           Text.HTML.TagSoup

openURL url = getResponseBody =<< simpleHTTP (getRequest url)

findLinks url = do
  tags <- parseTags <$> openURL url
  return $ filter (\x -> x ~== "<a>" || x ~== "<A>") tags

main :: IO ()
main = putChar 'a'
