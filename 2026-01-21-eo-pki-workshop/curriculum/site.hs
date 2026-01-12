{-# LANGUAGE OverloadedStrings #-}

import Text.Pandoc.Definition
  ( Pandoc(..), Block(Header, Plain), Inline(..) )
import Text.Pandoc.Walk (query, walk)
import Hakyll


siteTitle :: String
siteTitle = "Practical PKI"


main :: IO ()
main = hakyll $ do
  {-
  match "images/**" $ do
    route idRoute
    compile copyFileCompiler
  -}

  match "css/*" $ do
    route idRoute
    compile compressCssCompiler

  match "index.md" $ do
    route $ setExtension "html"
    compile $ do
      let homeContext =
            constField "title" "Start"
            <> context
      pandocCompiler
        >>= loadAndApplyTemplate "templates/module.html" homeContext
        >>= loadAndApplyTemplate "templates/default.html" homeContext
        >>= relativizeUrls

  match "modules/*" $ version "recent" $ do
    compile $
      pandocCompilerWithTransformM
        defaultHakyllReaderOptions
        defaultHakyllWriterOptions
        (\pandoc -> do
          let
            h1 = maybe [Str "no title"] id . firstHeader $ pandoc
            render f = fmap writePandoc . makeItem . Pandoc mempty . pure . Plain . f
          _ <- render removeFormatting h1 >>= saveSnapshot "title"
          _ <- render id h1 >>= saveSnapshot "fancyTitle"
          pure $ addSectionLinks pandoc
        )

  match "modules/*" $ do
    route $ setExtension "html"
    compile $ do
      ident <- getUnderlying
      loadBody (setVersion (Just "recent") ident)
        >>= makeItem
        >>= loadAndApplyTemplate "templates/module.html" context
        >>= loadAndApplyTemplate "templates/default.html" context
        >>= relativizeUrls

  match "templates/*" $ compile templateCompiler


context :: Context String
context =
  dateField "date" "%Y-%m-%d"
  <> snapshotField "title" "moduleTitle"
  <> constField "siteTitle" siteTitle
  <> urlFieldNoVersion "url0"
  <> defaultContext


-- | Get field content from snapshot (at item version "recent")
snapshotField
  :: String           -- ^ Key to use
  -> Snapshot         -- ^ Snapshot to load
  -> Context String   -- ^ Resulting context
snapshotField key snap = field key $ \item ->
  loadSnapshotBody (setVersion (Just "recent") (itemIdentifier item)) snap


-- | Set a url field that looks for url of non-versioned identifier
urlFieldNoVersion :: String -> Context a
urlFieldNoVersion key = field key $ \i -> do
  let ident = setVersion Nothing (itemIdentifier i)
      empty' = fail $ "No route url found for item " <> show ident
  fmap (maybe empty' toUrl) $ getRoute ident


firstHeader :: Pandoc -> Maybe [Inline]
firstHeader (Pandoc _ xs) = go xs
  where
  go [] = Nothing
  go (Header _ _ ys : _) = Just ys
  go (_ : t) = go t


-- yield "plain" terminal inline content; discard formatting
removeFormatting :: [Inline] -> [Inline]
removeFormatting = query f
  where
  f inl = case inl of
    Str s -> [Str s]
    Code _ s -> [Str s]
    Space -> [Space]
    SoftBreak -> [Space]
    LineBreak -> [LineBreak]
    Math _ s -> [Str s]
    RawInline _ s -> [Str s]
    _ -> []


addSectionLinks :: Pandoc -> Pandoc
addSectionLinks = walk f where
  f (Header n attr@(idAttr, _, _) inlines) | n > 1 =
      let link = Link ("", ["section"], []) [Str "§"] ("#" <> idAttr, "")
      in Header n attr (inlines <> [Space, link])
  f x = x
