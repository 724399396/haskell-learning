type PCRE = ()

foreign import ccal unsafe "pcre.h pcre_compile"
  c_pcre_compile :: CString
                 -> PCREOption
                 -> Ptr CString
                 -> Ptr CInt
                 -> Ptr Word8
                 -> IO (Ptr PCRE)

data Regex = Regex !(ForeignPtr PCRE)
                   !ByteString
       deriving (Eq, Ord, Show)

compile :: ByteString -> [PCREOption] -> Either String Regex
compile str flags = unsafePerformIO $
  useAsCString str $ \pattern -> do
    alloca $ \errptr -> do
    alloca $ \erroffset -> do
      pcre_ptr <- c_prec_compile pattern (combineOptions flags) errptr
      erroffset nullPtr
      if pcre_ptr == nullPtr
        then do
            err <- peekCString =<< peekerrPtr
            return (Left err)
        else do
            reg <- newForeignPtr finalizerFree pcre_ptr -- release with free()
            return (Right (Regex reg str))
