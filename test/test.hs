import qualified Binaryen
import qualified Binaryen.Module as Module
import Control.Monad ((<=<))
import Data.Coerce (coerce)
import Foreign (finalizerFree, newForeignPtr, withForeignPtr)
import Foreign.C (peekCString)
import Foreign.ForeignPtr (ForeignPtr)
import Foreign.Ptr (FunPtr)

main :: IO ()
main = do
  print =<< Binaryen.getOptimizeLevel

  testDisposal

testDisposal :: IO ()
testDisposal = do
  modFP <- newModuleForeignPtr Module.dispose =<< Module.create
  strPtr <- withModuleForeignPtr modFP Module.allocateAndWriteText
  strFP <- newForeignPtr finalizerFree strPtr
  withForeignPtr strFP (print <=< peekCString)

newModuleForeignPtr ::
  FunPtr (Module.Module -> IO ()) ->
  Module.Module ->
  IO (ForeignPtr Module.Module)
newModuleForeignPtr finalizer m = newForeignPtr (coerce finalizer) (coerce m)

withModuleForeignPtr ::
  ForeignPtr Module.Module ->
  (Module.Module -> IO a) ->
  IO a
withModuleForeignPtr m f = withForeignPtr (coerce m) (coerce f)
