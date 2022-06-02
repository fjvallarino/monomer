
module Monomer.Common.OSDialogs where

import Data.Text
import Data.Maybe
import Graphics.UI.TinyFileDialogs

--OS specific File Dialogs
osSaveFileDialog :: Text   -> Text -> [Text] -> Text -> IO (Maybe Text)	
osSaveFileDialog   = saveFileDialog

osSelectFileDialog :: Text -> Text -> [Text] -> Text -> Bool	-> IO (Maybe [Text])	
osSelectFileDialog = openFileDialog

osSelectFolderDialog :: Text -> Text -> IO (Maybe Text)	
osSelectFolderDialog = selectFolderDialog