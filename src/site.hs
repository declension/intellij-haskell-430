--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
import           Data.Monoid ((<>))
import           Control.Monad.Trans (liftIO)
import           Hakyll
import           System.FilePath
import           Data.Maybe (fromMaybe,isJust)
import qualified Data.Text as T
import qualified Data.Set as S
import           Data.Char (toLower)
import           Data.Time.Clock               (UTCTime (..),getCurrentTime)
import           Data.Time.Format              (formatTime)
import           Data.Time.Locale.Compat       (TimeLocale, defaultTimeLocale)
import           Text.Pandoc.Extensions
import           Text.Pandoc.Options (def, readerExtensions,ReaderOptions,writerHighlightStyle)
import           Text.Pandoc.Highlighting (kate)
import           Hakyll.Web.Sass (sassCompiler,renderSass)
import           Control.Monad (liftM,filterM)
--------------------------------------------------------------------------------

main :: IO ()
main = getCurrentTime >>= hakyll . rulesAt

rulesAt buildTime = do
    let pageCtx = defaultContext

    match "posts/*" $ do
        route directoryify
        compile $ do
            tpl <- loadBody "templates/post.html"
            customPandocCompiler
                >>= applyTemplate tpl pageCtx
                >>= loadAndApplyTemplate "templates/default.html" pageCtx 
                >>= relativizeUrls

    match "templates/**" $ compile templateBodyCompiler

--------------------------------------------------------------------------------


customPandocCompiler :: Compiler (Item String)
customPandocCompiler = pandocCompilerWith pandocReaderOptions (def {writerHighlightStyle = Just kate})

pandocReaderOptions :: ReaderOptions
pandocReaderOptions = def { readerExtensions = disableExtension Ext_implicit_figures $ extraReaderExtensions <> pandocExtensions }
        where extraReaderExtensions = extensionsFromList [Ext_auto_identifiers, Ext_ascii_identifiers, Ext_emoji, Ext_backtick_code_blocks]

dropPrefix :: String -> Routes
dropPrefix prefix = gsubRoute prefix $ const ""

directoryify :: Routes
directoryify = customRoute $
    (\(p, e) -> p </> "index" <.> "html") . splitExtension . toFilePath


dropIndexHtml :: String -> Context a
dropIndexHtml key = mapContext transform (urlField key) where
    transform url = case splitFileName url of
                        (p, "index.html") -> takeDirectory p ++ "/"
                        _                 -> url

loadPosts :: Compiler [Item String]
loadPosts = loadAll "posts/*.md"
            >>= recentFirst

