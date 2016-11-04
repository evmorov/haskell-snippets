newtype Project = Project String
newtype Limit = Limit Int

getBuildsInfo :: Project -> Limit -> String
getBuildsInfo (Project projectName) (Limit limit) =
  "Information about build " ++ projectName ++ " with limit " ++ show limit

main :: IO ()
main = print $ getBuildsInfo (Project "ohaskell.guide") (Limit 4)
