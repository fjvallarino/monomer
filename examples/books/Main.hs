{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Control.Lens
import Data.Default
import Data.Maybe
import Data.Text (Text)
import TextShow

import qualified Data.Text as T
import qualified Network.Wreq as W

import BookTypes
import Monomer

import qualified Monomer.Lens as L

type BooksWenv = WidgetEnv BooksModel BooksEvt
type BooksNode = WidgetNode BooksModel BooksEvt

bookImage :: Maybe Int -> Text -> WidgetNode s BooksEvt
bookImage imgId size = maybe filler coverImg imgId where
  baseUrl = "http://covers.openlibrary.org/b/id/<id>-<size>.jpg"
  imgUrl i = T.replace "<size>" size $ T.replace "<id>" (showt i) baseUrl
  coverImg i = image_ (imgUrl i) [fitHeight, alignRight]

bookRow :: BooksWenv -> Book -> BooksNode
bookRow wenv b = row where
  rowBgColor = wenv ^. L.theme . L.userColorMap . at "rowBgColor" . non def
  publishYear = maybe "" showt (b ^. year)

  rowContent b = hstack [
      vstack [
        label_ (b ^. title) [resizeFactor 1]
          `styleBasic` [textFont "Medium", textSize 16],
        spacer,
        label_ (T.intercalate ", " (b ^. authors)) [resizeFactor 1]
          `styleBasic` [textSize 14]
      ],
      filler,
      vstack [
        label publishYear `styleBasic` [width 50, textSize 14],
        spacer
      ],
      bookImage (b ^. cover) "S" `styleBasic` [width 35]
    ]

  row = box_ cfg content `styleBasic` [padding 10, paddingT 0] where
    cfg = [expandContent, onClick (BooksShowDetails b)]
    content = rowContent b
      `styleBasic` [height 80, padding 20, radius 5]
      `styleHover` [bgColor rowBgColor, cursorIcon CursorHand]

bookDetail :: Book -> WidgetNode s BooksEvt
bookDetail b = content `styleBasic` [minWidth 500, paddingH 20] where
  hasCover = isJust (b ^. cover)
  publishYear = maybe "" showt (b ^. year)

  shortLabel value = label value `styleBasic` [textFont "Medium", textTop]
  longLabel value = label_ value [multiline, ellipsis, trimSpaces]

  content = hstack . concat $ [[
    vstack [
      longLabel (b ^. title)
        `styleBasic` [textSize 20, textFont "Medium"],
      spacer,
      longLabel (T.intercalate ", " (b ^. authors))
        `styleBasic` [textSize 16],
      spacer,
      label publishYear
        `styleBasic` [textSize 14]
    ]],
    [filler | hasCover],
    [bookImage (b ^. cover) "M" `styleBasic` [width 200] | hasCover]
    ]

buildUI
  :: WidgetEnv BooksModel BooksEvt
  -> BooksModel
  -> WidgetNode BooksModel BooksEvt
buildUI wenv model = widgetTree where
  sectionBgColor = wenv ^. L.theme . L.sectionColor

  bookOverlay = alert BooksCloseDetails content where
    content = maybe spacer bookDetail (model ^. selected)

  searchOverlay = box content `styleBasic` [bgColor (darkGray & L.a .~ 0.8)] where
    content = label "Searching" `styleBasic` [textSize 20, textColor black]

  searchForm = keystroke [("Enter", BooksSearch)] $ vstack [
      hstack [
        label "Query:",
        spacer,
        textField query `key` "query",
        spacer,
        mainButton "Search" BooksSearch
      ] `styleBasic` [bgColor sectionBgColor, padding 25]
    ]

  countLabel = label caption `styleBasic` [padding 10] where
    caption = "Books (" <> showt (length $ model ^. books) <> ")"

  booksChanged old new = old ^. books /= new ^. books

  widgetTree = zstack [
      vstack [
        searchForm,
        countLabel,
        box_ [mergeRequired booksChanged] $
          vscroll (vstack (bookRow wenv <$> model ^. books)) `key` "mainScroll"
      ],
      bookOverlay `visible` isJust (model ^. selected),
      searchOverlay `visible` model ^. searching
    ]

handleEvent
  :: WidgetEnv BooksModel BooksEvt
  -> WidgetNode BooksModel BooksEvt
  -> BooksModel
  -> BooksEvt
  -> [EventResponse BooksModel BooksEvt BooksModel ()]
handleEvent wenv node model evt = case evt of
  BooksInit -> [setFocusOnKey wenv "query"]
  BooksSearch -> [
    Model $ model & searching .~ True,
    Task $ searchBooks (model ^. query)
    ]
  BooksSearchResult resp -> [
    Message "mainScroll" ScrollReset,
    Model $ model
      & searching .~ False
      & books .~ resp ^. docs
    ]
  BooksSearchError msg -> []
  BooksShowDetails book -> [Model $ model & selected ?~ book]
  BooksCloseDetails -> [Model $ model & selected .~ Nothing]

searchBooks :: Text -> IO BooksEvt
searchBooks query = do
  print ("Searching", query)
  resp <- W.asJSON =<< W.get url
  return $ case resp ^? W.responseBody . _Just of
    Just resp -> BooksSearchResult resp
    Nothing -> BooksSearchError "Failed"
  where
    url = "http://openlibrary.org/search.json?q=" <> T.unpack query

main :: IO ()
main = do
  startApp initModel handleEvent buildUI config
  where
    config = [
      appWindowTitle "Book search",
      appTheme customLightTheme,
      appFontDef "Regular" "./assets/fonts/Roboto-Regular.ttf",
      appFontDef "Medium" "./assets/fonts/Roboto-Medium.ttf",
      appInitEvent BooksInit
      ]
    initBook = Book "This is my book" ["Author1", "Author 2"] (Just 2000) (Just 1234)
    initModel = BooksModel "pedro paramo" False (Just initBook) [initBook]

customLightTheme :: Theme
customLightTheme = lightTheme
  & L.userColorMap . at "rowBgColor" ?~ rgbHex "#ECECEC"

customDarkTheme :: Theme
customDarkTheme = darkTheme
  & L.userColorMap . at "rowBgColor" ?~ rgbHex "#656565"
