--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections     #-}
import           Control.Applicative
import           Data.Function
import           Data.List
import           Data.Monoid
import           Hakyll
import           System.FilePath
import           System.Locale


--------------------------------------------------------------------------------
main :: IO ()
main = hakyll $ do
    match "images/*" $ do
        route   idRoute
        compile copyFileCompiler

    match "css/*" $ do
        route   idRoute
        compile compressCssCompiler


    postTags <- buildTags "posts/*" $ fromCapture "posts/tags/*.html"

    -- Generate tag pages.
    tagsRules postTags $ tagPageRules "templates/tag.html"


    match "posts/*" $ do
        route $ setExtension "html"
        let ctx = tagsField "tags" postTags <> postCtx
        compile $ pandocCompiler
            >>= saveSnapshot "content"
            >>= withItemBody (return . demoteHeaders)
            >>= saveSnapshot "demoted"
            >>= loadAndApplyTemplate "templates/post.html"    ctx
            >>= saveSnapshot "post-view"
            >>= loadAndApplyTemplate "templates/default.html" ctx
            >>= relativizeUrls


    match "index.html" $ do
        route idRoute
        compile $ do
            posts <- recentFirst =<< loadAllSnapshots "posts/*" "demoted"
            let ctx = listField "posts" postCtx (return posts)
                   <> defaultCtx
            getResourceBody
                >>= applyAsTemplate ctx
                >>= loadAndApplyTemplate "templates/default.html" ctx
                >>= relativizeUrls

    create ["archive.html"] $ do
        route idRoute
        compile $ do
            posts <- recentFirst =<< loadAll "posts/*"
            let ctx = listField "posts" postCtx (return posts)
                   <> constField "title" "Archives"
                   <> defaultCtx
            makeItem ""
                >>= loadAndApplyTemplate "templates/archive.html" ctx
                >>= loadAndApplyTemplate "templates/default.html" ctx
                >>= relativizeUrls

    match "templates/*" $ compile templateCompiler


tagPageRules :: Identifier -> String -> Pattern -> Rules ()
tagPageRules template tag pattern = do
    route idRoute
    compile $ do
      let title = "Posts tagged \"" <> tag <> "\""
      posts <- recentFirst =<< loadAll pattern
      let ctx = constField "title" title
             <> listField "posts" postCtx (return posts)
             <> defaultCtx
      makeItem ""
          >>= loadAndApplyTemplate template ctx
          >>= loadAndApplyTemplate "templates/default.html" ctx
          >>= relativizeUrls


--------------------------------------------------------------------------------
postCtx :: Context String
postCtx =
    dateField "date" "%e %B %Y" `mappend`
    defaultCtx

defaultCtx :: Context String
defaultCtx = navTagsField "tags-nav"
          <> navPostListField "posts-nav"
          <> defaultContext


--------------------------------------------------------------------------------

navTagsField :: String -> Context String
navTagsField = flip field mkNavTags
  where
    mkNavTags _ =
        buildTags "posts/*" (fromCapture "posts/tags/*.html") >>= renderNavTags

-- | Renders a tags list for displaying in the navbar.
renderNavTags :: Tags -> Compiler String
renderNavTags = renderTags mkTag unlines
  where
    mkTag tag url _ _ _ =
        "<a href=\"" <> escapeHtml url <>"\">" <> escapeHtml tag <>"</a>"



-- The post list in the navbar is tough because we can't simply use loadAll
-- like we do with other post lists. Doing so would cause dependency loops.
---- Navbar Post List ----------------------------------------------------------

navPostListField :: String -> Context String
navPostListField  = flip field mkPostList
  where
    mkPostList _ = unlines <$> (mapM mkEntry =<< loadPostList)
    mkEntry (url, ident) = do
      title <- getMetadataField' ident "title"
      return ("<a href=\"/" <> escapeHtml url <>"\">" <> escapeHtml title <>"</a>")

loadPostList :: Compiler [(String, Identifier)]
loadPostList =
    map (\i -> (fixExt i, i)) <$> (sortByTime =<< getMatches "posts/*")
  where
    tupleTime ident = (ident,) <$> getItemUTC defaultTimeLocale ident
    sortByTime :: [Identifier] -> Compiler [Identifier]
    sortByTime ps = map fst <$> sortBy (compare `on` snd) <$> mapM tupleTime ps
    fixExt :: Identifier -> String
    fixExt i = replaceExtension (toFilePath i) "html"
