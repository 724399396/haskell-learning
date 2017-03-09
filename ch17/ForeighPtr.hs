newForeignPtr :: FinalizerPtr a -> Ptr a -> IO (ForeighPtr a)

useAsCString :: ByteString -> (CString -> IO a) -> IO a

alloca :: Storable a => (Ptr a -> IO b) -> IO B
