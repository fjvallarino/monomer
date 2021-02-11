{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Control.Lens
import Data.Maybe
import Data.Text (Text)
import TextShow

import qualified Data.Text as T
import qualified Network.Wreq as W

import BookTypes
import Monomer

buildUI
  :: WidgetEnv BooksModel BooksEvt
  -> BooksModel
  -> WidgetNode BooksModel BooksEvt
buildUI wenv model = widgetTree where
  bookThumb i = maybe spacer coverImg i where
    baseUrl = "http://covers.openlibrary.org/b/id/<id>-S.jpg"
    imgUrl i = T.replace "<id>" (showt i) baseUrl
    coverImg i = image_ (imgUrl i) [fitHeight]
  book b = hstack [
      vstack [
        hstack [
          label_ "Title: " [resizeFactor 0] `style` [textFont "Bold"],
          label (b ^. title)
        ],
        hstack [
          label_ "Authors: " [resizeFactor 0] `style` [textFont "Bold"],
          label (T.intercalate ", " (b ^. authors))
        ]
      ],
      filler,
      vstack [
        hstack [
          label_ "Year: " [resizeFactor 0] `style` [textFont "Bold"],
          label $ maybe "" showt (b ^. year)
        ]
      ] `style` [width 100],
      bookThumb (b ^. cover) `style` [width 50]
    ] `style` [height 50, padding 5, paddingT 0]
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
  widgetTree = vstack [
      searchForm,
      vscroll $ vstack (book <$> model ^. books)
    ]

handleEvent
  :: WidgetEnv BooksModel BooksEvt
  -> WidgetNode BooksModel BooksEvt
  -> BooksModel
  -> BooksEvt
  -> [EventResponse BooksModel BooksEvt ()]
handleEvent wenv node model evt = case evt of
  BooksInit -> [setFocus wenv "query"]
  BooksSearch -> [ Task $ searchBooks (model ^. query)]
  BooksSearchResult resp -> [Model $ model & books .~ resp ^. docs]
  BooksSearchError msg -> []

searchBooks :: Text -> IO (Maybe BooksEvt)
searchBooks query = do
  print ("Searching", query)
  resp <- W.asJSON =<< W.get url
  return . Just $ case resp ^? W.responseBody . _Just of
    Just resp -> BooksSearchResult resp
    Nothing -> BooksSearchError "Failed"
  where
    url = "http://openlibrary.org/search.json?q=" <> T.unpack query

main :: IO ()
main = do
  simpleApp (BooksModel "" []) handleEvent buildUI config
  where
    config = [
      appWindowTitle "Todo list",
      appTheme darkTheme,
      appFontDef "Regular" "./assets/fonts/Roboto-Regular.ttf",
      appFontDef "Bold" "./assets/fonts/Roboto-Bold.ttf",
      appInitEvent BooksInit
      ]

setFocus :: WidgetEnv s e -> Text -> EventResponse s e ep
setFocus wenv key = Request (SetFocus path) where
  path = fromMaybe rootPath (globalKeyPath wenv key)
