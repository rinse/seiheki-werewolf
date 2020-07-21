{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}

module Servant.HTML where

import qualified Data.ByteString.Lazy as BL
import           Network.HTTP.Media   ((//), (/:))
import           Servant.API


data HTML

instance Accept HTML where
    contentType _ = "text" // "html" /: ("charset", "utf-8")

instance MimeRender HTML BL.ByteString where
    mimeRender _ bs = bs
