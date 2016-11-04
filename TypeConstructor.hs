data MyMaybe a = MyNothing | MyJust a -- <- Type Constructor

-- "type" keyword makes aliases
type Chapters = [(FilePath, String)]

chapters :: Chapters
chapters = [ ("/list.html",  "Список")
           , ("/tuple.html", "Кортеж")
           , ("/hof.html",   "ФВП")
           ]

lookupChapterNameBy :: FilePath -> Chapters -> MyMaybe String
lookupChapterNameBy _ [] = MyNothing
lookupChapterNameBy path ((realPath, name) : others)
  | path == realPath = MyJust name
  | otherwise        = lookupChapterNameBy path others

main :: IO ()
main = putStrLn $
  case name of
    MyNothing -> "No such chapter, sorry..."
    MyJust n -> "This is chapter name: " ++ n
  where
    name = lookupChapterNameBy "/tuple.html" chapters
