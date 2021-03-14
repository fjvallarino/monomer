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
  bookImage imgId size = maybe spacer coverImg imgId where
    baseUrl = "http://covers.openlibrary.org/b/id/<id>-<size>.jpg"
    imgUrl i = T.replace "<size>" size $ T.replace "<id>" (showt i) baseUrl
    coverImg i = image_ (imgUrl i) [fitHeight]
  bookRow b = box_ [expandContent, onClick (BooksShowDetails b)]
    (bookRowContent b)
    `hover` [bgColor gray, cursorIcon CursorHand]
  bookRowContent b = hstack [
      vstack [
        hstack [
          label "Title: " `style` [textFont "Bold"],
          label_ (b ^. title) [resizeFactor 1]
        ],
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
      bookImage (b ^. cover) "S" `style` [width 50]
    ] `style` [height 50, padding 5]
  bookDetail b = content where
    hasCover = isJust (b ^. cover)
    shortLabel value = label value `style` [width 80, textFont "Bold", textTop]
    longLabel value = label_ value [multiLine, ellipsis, trimSpaces]
    content = hstack . concat $ [[
      vstack [
        hstack [
          shortLabel "Title: ",
          longLabel (b ^. title)
        ],
        hstack [
          shortLabel "Authors: ",
          longLabel (T.intercalate ", " (b ^. authors))
        ],
        hstack [
          shortLabel "Year: ",
          label $ maybe "" showt (b ^. year)
        ]
      ]],
      [filler | hasCover],
      [bookImage (b ^. cover) "M" `style` [width 200] | hasCover]
      ]
  bookOverlay = alert content BooksCloseDetails where
    content = maybe spacer bookDetail (model ^. selected)
  searchOverlay = box content `style` [bgColor (darkGray & L.a .~ 0.8)] where
    content = label "Searching" `style` [textSize 20, textColor black]
  searchForm = keystroke [("Enter", BooksSearch)] $ vstack [
      hstack [
        label "Query: ",
        textField query `key` "query"
      ],
      spacer,
      hstack [
        button "Search" BooksSearch,
        filler
      ]
    ] `style` [padding 5]
  widgetTree = zstack [
      vstack [
        searchForm,
        vscroll (vstack (bookRow <$> model ^. books)) `key` "mainScroll"
      ],
      bookOverlay `visible` isJust (model ^. selected),
      searchOverlay `visible` model ^. searching
    ]

handleEvent
  :: WidgetEnv BooksModel BooksEvt
  -> WidgetNode BooksModel BooksEvt
  -> BooksModel
  -> BooksEvt
  -> [EventResponse BooksModel BooksEvt ()]
handleEvent wenv node model evt = case evt of
  BooksInit -> [setFocus wenv "query"]
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
  simpleApp initModel handleEvent buildUI config
  where
    config = [
      appWindowTitle "Book search",
      appTheme darkTheme,
      appFontDef "Regular" "./assets/fonts/Roboto-Regular.ttf",
      appFontDef "Bold" "./assets/fonts/Roboto-Bold.ttf",
      appInitEvent BooksInit
      ]
    initModel = BooksModel "borges-bioy" False Nothing []

setFocus :: WidgetEnv s e -> Text -> EventResponse s e ep
setFocus wenv key = Request (SetFocus widgetId) where
  widgetId = fromMaybe def (globalKeyWidgetId wenv key)
