{-# LANGUAGE CPP, ForeignFunctionInterface #-}

module Regex where

import Foreign
import Foreign.C.Types

#include <pcre.h>

newtype PCREOption = PCREOption { unPCREOption :: CInt }
  deriving (Eq, Show)

#{enum PCREOption, PCREOption
   , caseless = PCRE_CASELESS
   , dollor_endonly = PCRE_DOLLAR_ENDONLY
   , dotall = PCRE_DOTALL
   }
