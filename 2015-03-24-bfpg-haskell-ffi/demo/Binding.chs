{-# LANGUAGE ForeignFunctionInterface #-}

module Binding
  (
    makeThing
  , getSerial
  , getName
  ) where

import Foreign
import Foreign.C

#include "thing.h"

foreign import ccall "thing_get_serial"
  thing_get_serial :: Thing -> CInt

foreign import ccall "thing_get_name"
  thing_get_name :: Thing -> Ptr CChar

foreign import ccall "thing_new"
  thing_new :: CInt -> Ptr CChar -> Ptr Thing -> IO CInt

type Thing = Ptr ()

makeThing :: Int -> String -> IO (Maybe Thing)
makeThing n s =
  withCString s $ \name ->
    alloca $ \ptr -> do
      result <- thing_new (fromIntegral n) name ptr
      if result == 0
        then fmap Just $ peek ptr
        else return Nothing

getName :: Thing -> IO String
getName = peekCString . thing_get_name

getSerial :: Num a => Thing -> a
getSerial = fromIntegral . thing_get_serial

foreign import ccall "&thing_free"
  thing_free :: FinalizerPtr a
