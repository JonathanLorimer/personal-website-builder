{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE OverloadedLabels      #-}
{-# LANGUAGE OverloadedStrings     #-}
import           Control.Lens
import           Control.Monad
import           Data.Aeson                 as A
import           Data.Aeson.Lens
import qualified Data.HashMap.Lazy          as HML
import           Data.List                  (sortBy)
import qualified Data.Text                  as T
import           Development.Shake
import           Development.Shake.Classes
import           Development.Shake.FilePath
import           Development.Shake.Forward
import           GHC.Generics               (Generic)
import           Slick

---Config-----------------------------------------------------------------------

siteMeta :: SiteMeta
siteMeta =
    SiteMeta { siteAuthor = "jonathanlorimer"
             , baseUrl = "https://example.com"
             , siteTitle = "Jonathan Lorimer"
             , githubUser = "jonathanlorimer"
             , linkedInUser = "jonathan-lorimer-dev"
             , twitterUser = "jonathanlorime1"
             }

outputFolder :: FilePath
outputFolder = "docs/"

--Data models-------------------------------------------------------------------

withSiteMeta :: Value -> Value
withSiteMeta (Object obj) = Object $ HML.union obj siteMetaObj
  where
    Object siteMetaObj = toJSON siteMeta
withSiteMeta _ = error "only add site meta to objects"

-- mergeObjects :: [Value] -> Value
-- mergeObjects = Object . HML.unions . map (\(Object x) -> x)

data SiteMeta =
    SiteMeta { siteAuthor   :: String
             , baseUrl      :: String
             , siteTitle    :: String
             , githubUser   :: String
             , linkedInUser :: String
             , twitterUser  :: String
             }
    deriving (Generic, Eq, Ord, Show, ToJSON)

-- | Data for the index page
data PostsInfo =
  PostsInfo
    { posts :: [Post]
    } deriving (Generic, Show, FromJSON, ToJSON)

-- | Data for a blog post
data Post =
    Post { title      :: String
         , author     :: String
         , content    :: String
         , url        :: String
         , date       :: String
         , datePretty :: String
         , tags       :: [Tag]
         , image      :: Maybe String
         } deriving (Generic, Eq, Ord, Show, FromJSON, ToJSON, Binary)

data Tag = Tag { tag :: String }
    deriving (Generic, Eq, Ord, Show, FromJSON, ToJSON, Binary)

-- | Data for CV
data Bio = Bio { email    :: String
               , location :: String
               , content  :: String
               } deriving (Generic, Eq, Show, FromJSON, ToJSON, Binary)

data Technology =
  Technology { technology :: String } deriving (Generic, Eq, Show, FromJSON, ToJSON, Binary)


data Experience =
  Experience { company      :: String
             , location     :: String
             , title        :: String
             , startDate    :: String
             , endDate      :: Maybe String
             , technologies :: [Technology]
             , content      :: String
             } deriving (Generic, Eq, Show, FromJSON, Binary)

instance ToJSON Experience where
  toJSON = genericToJSON defaultOptions { omitNothingFields = True }

instance Ord Experience where
  Experience { startDate = sd1 } `compare` Experience { startDate = sd2 } =
    sd1 `compare` sd2

data Education =
  Education { schoolName :: String
            , startDate  :: String
            , endDate    :: String
            , credential :: String
            } deriving (Generic, Eq, Show, FromJSON, ToJSON, Binary)

data AboutMe = AboutMe { bio        ::  Bio
                       , experience :: [Experience]
                       , education  :: [Education]
                       } deriving (Generic, Eq, Show, FromJSON, ToJSON)

buildExperience :: FilePath -> Action Experience
buildExperience srcPath = cacheAction ("build" :: T.Text, srcPath) $ do
  liftIO . putStrLn $ "Rebuilding aboutme/experience: " <> srcPath
  experienceContent <- readFile' srcPath
  -- load post content and metadata as JSON blob
  experienceData <- markdownToHTML . T.pack $ experienceContent
  convert experienceData

buildBio :: FilePath -> Action Bio
buildBio srcPath = cacheAction ("build" :: T.Text, srcPath) $ do
  liftIO . putStrLn $ "Rebuilding aboutme/bio: " <> srcPath
  bioContent <- readFile' srcPath
  -- load post content and metadata as JSON blob
  bioData <- markdownToHTML . T.pack $ bioContent
  convert bioData

buildEducation :: FilePath -> Action Education
buildEducation srcPath = cacheAction ("build" :: T.Text, srcPath) $ do
  liftIO . putStrLn $ "Rebuilding aboutme/education: " <> srcPath
  eduContent <- readFile' srcPath
  -- load post content and metadata as JSON blob
  eduData <- markdownToHTML . T.pack $ eduContent
  convert eduData

buildAboutMe :: Action ()
buildAboutMe  = do

  -- Get Paths
  [bioPath] <- getDirectoryFiles "." ["site/aboutme//bio.md"]
  experiencePaths <- getDirectoryFiles "." ["site/aboutme/experience//*.md"]
  educationPaths <- getDirectoryFiles "." ["site/aboutme/education//*.md"]

  -- Build Data Structures
  bioData <- buildBio bioPath
  expsData <- forP experiencePaths buildExperience
  edusData <- forP educationPaths buildEducation
  let aboutMeData = AboutMe { bio = bioData
                            , experience = sortBy (flip compare) expsData
                            , education = edusData
                            }
  -- Compile HTML
  aboutMeT <- compileTemplate' "site/templates/aboutme.html"
  let cvHTML = T.unpack $ substitute aboutMeT $ withSiteMeta $ toJSON aboutMeData
  writeFile' (outputFolder </> "aboutme.html") cvHTML

buildIndex :: Action ()
buildIndex = do
  indexT <- compileTemplate' "site/templates/index.html"
  let indexHTML = T.unpack $ substitute indexT $ toJSON siteMeta
  writeFile' (outputFolder </> "index.html") indexHTML

-- | given a list of posts this will build a table of contents
buildTableOfContents :: [Post] -> Action ()
buildTableOfContents posts' = do
  postsT <- compileTemplate' "site/templates/posts.html"
  let postsInfo = PostsInfo { posts = sortBy (\x y -> compare (date y) (date x)) posts' }
      postsHTML = T.unpack $ substitute postsT (withSiteMeta $ toJSON postsInfo)
  writeFile' (outputFolder </> "posts.html") postsHTML

-- | Find and build all posts
buildPosts :: Action [Post]
buildPosts = do
  pPaths <- getDirectoryFiles "." ["site/posts//*.md"]
  forP pPaths buildPost

-- | Load a post, process metadata, write it to output, then return the post object
-- Detects changes to either post content or template
buildPost :: FilePath -> Action Post
buildPost srcPath = cacheAction ("build" :: T.Text, srcPath) $ do
  liftIO . putStrLn $ "Rebuilding post: " <> srcPath
  postContent <- readFile' srcPath
  -- load post content and metadata as JSON blob
  postData <- markdownToHTML . T.pack $ postContent
  let postUrl = T.pack . dropDirectory1 $ srcPath -<.> "html"
      withPostUrl = _Object . at "url" ?~ String postUrl
  -- Add additional metadata we've been able to compute
  let fullPostData = withSiteMeta . withPostUrl $ postData
  template <- compileTemplate' "site/templates/post.html"
  writeFile' (outputFolder </> T.unpack postUrl) . T.unpack $ substitute template fullPostData
  convert fullPostData

-- | Copy all static files from the listed folders to their destination
copyStaticFiles :: Action ()
copyStaticFiles = do
    filepaths <- getDirectoryFiles "./site/"
      ["images//*"
      , "css//*"
      , "js//*"
      , "fonts//*"
      ]
    void $ forP filepaths $ \filepath ->
        copyFileChanged ("site" </> filepath) (outputFolder </> filepath)

-- | Specific build rules for the Shake system
--   defines workflow to build the website
buildRules :: Action ()
buildRules = do
  allPosts <- buildPosts
  buildAboutMe
  buildIndex
  buildTableOfContents allPosts
  copyStaticFiles

main :: IO ()
main = do
  let shOpts = shakeOptions { shakeVerbosity = Chatty, shakeLintInside = ["\\"] }
  shakeArgsForward shOpts buildRules
