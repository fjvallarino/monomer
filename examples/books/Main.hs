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

buildUI
  :: WidgetEnv BooksModel BooksEvt
  -> BooksModel
  -> WidgetNode BooksModel BooksEvt
buildUI wenv model = widgetTree where
  sectionBgColor = wenv ^. L.theme . L.sectionColor
  rowBgColor = wenv ^. L.theme . L.userColorMap . at "rowBgColor" . non def
  bookImage imgId size = maybe filler coverImg imgId where
    baseUrl = "http://covers.openlibrary.org/b/id/<id>-<size>.jpg"
    imgUrl i = T.replace "<size>" size $ T.replace "<id>" (showt i) baseUrl
    coverImg i = image_ (imgUrl i) [fitHeight, alignRight]
  bookRow b = box_ cfg content `style` [padding 10, paddingT 0] where
    cfg = [expandContent, onClick (BooksShowDetails b)]
    content = bookRowContent b
      `style` [bgColor rowBgColor, height 80, padding 20, radius 5]
      `hover` [bgColor gray, cursorIcon CursorHand]
  bookRowContent b = hstack [
      vstack [
        hstack [
          label "Title: " `style` [textFont "Bold"],
          label_ (b ^. title) [resizeFactor 1]
        ],
        spacer,
        hstack [
          label "Authors: " `style` [textFont "Bold"],
          label_ (T.intercalate ", " (b ^. authors)) [resizeFactor 1]
        ]
      ],
      filler,
      vstack [
        hstack [
          label "Year: " `style` [textFont "Bold"],
          label $ maybe "" showt (b ^. year)
        ]
      ] `style` [width 100],
      bookImage (b ^. cover) "S" `style` [width 35]
    ]
  bookDetail b = content `style` [minWidth 500, paddingH 20] where
    hasCover = isJust (b ^. cover)
    shortLabel value = label value `style` [textFont "Bold", textTop]
    longLabel value = label_ value [multiLine, ellipsis, trimSpaces]
    content = hstack . concat $ [[
      vstack [
        hstack [
          shortLabel "Title: ",
          longLabel (b ^. title)
        ],
        spacer,
        hstack [
          shortLabel "Authors: ",
          longLabel (T.intercalate ", " (b ^. authors))
        ],
        spacer,
        hstack [
          shortLabel "Year: ",
          label $ maybe "" showt (b ^. year)
        ]
      ]],
      [filler | hasCover],
      [bookImage (b ^. cover) "M" `style` [width 200] | hasCover]
      ]
  bookOverlay = alert BooksCloseDetails content where
    content = maybe spacer bookDetail (model ^. selected)
  searchOverlay = box content `style` [bgColor (darkGray & L.a .~ 0.8)] where
    content = label "Searching" `style` [textSize 20, textColor black]
  searchForm = keystroke [("Enter", BooksSearch)] $ vstack [
      hstack [
        label "Query:",
        spacer,
        textField query `key` "query",
        spacer,
        mainButton "Search" BooksSearch
      ] `style` [bgColor sectionBgColor, padding 25]
    ]
  countLabel = label caption `style` [padding 10] where
    caption = "Books (" <> showt (length $ model ^. books) <> ")"
  widgetTree = zstack [
      vstack [
        searchForm,
        countLabel,
        vscroll (vstack (bookRow <$> model ^. books)) `key` "mainScroll"
      ],
      bookOverlay `visible` isJust (model ^. selected),
      searchOverlay `visible` model ^. searching
    ]

{-
searchBgColor :: Color
searchBgColor = rgbHex "#404040"

rowBgColor :: Color
rowBgColor = rgbHex "#212121"
-}

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
      appTheme lightTheme,
      appFontDef "Regular" "./assets/fonts/Roboto-Regular.ttf",
      appFontDef "Bold" "./assets/fonts/Roboto-Bold.ttf",
      appInitEvent BooksInit
      ]
    initBook = Book "This is my book" ["Author1", "Author 2"] (Just 2000) (Just 1234)
    initModel = BooksModel "pedro paramo" False (Just initBook) [initBook]
    customLightTheme = lightTheme
      & L.userColorMap . at "rowBgColor" ?~ rgbHex "#636363"
    customDarkTheme = darkTheme
      & L.userColorMap . at "rowBgColor" ?~ rgbHex "#212121"
