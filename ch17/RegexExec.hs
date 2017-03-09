foreign import ccall "pcre.h pcre_exec"
  c_pcre_exec :: Ptr PCRE
              -> Ptr PCREEXtra
              -> Ptr Word8
              -> CInt
              -> CInt
              -> PCREExecOption
              -> Ptr CInt
              -> CInt
              -> IO CInt

foreign import ccall "pcre.h pcre_fullinfo"
  c_pcre_fullinfo :: Ptr PCRE
                  -> Ptr PCREExtra
                  -> PCREInfo
                  -> Ptr a
                  -> IO CInt

capturedCount :: Ptr PCRE -> IO Int
capturedCount regex_ptr =
  alloca $ \n_ptr -> do
    c_pcre_fullinfo regex_ptr nullPtr info_capturecount n_ptr
    return . fromIntegral =<< peek (n_ptr :: Ptr CInt)

match :: Regex -> ByteString -> [PCREExecOption]
