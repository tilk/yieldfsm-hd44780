
import Prelude
import Development.Shake
import Development.Shake.FilePath
import Clash.Main(defaultMain)

buildPath :: String
buildPath = "_build"

helloworldTop :: String
helloworldTop = "HelloWorld"

verilog :: String -> FilePath
verilog topEntity = buildPath </> helloworldTop <.> topEntity

main :: IO ()
main = shakeArgs shakeOptions { shakeFiles = "_build" } $ do
    want ["compile"]

    phony "clean" $ do
        putInfo "Cleaning build directory"
        removeFilesAfter buildPath ["//*"]

    phony "compile" $ need [verilog "topEntity" </> "topEntity" <.> "v"]

    verilog "*" </> "*" <.> "v" %> \out -> do
        let outdir = takeFileName $ takeDirectory out
        putInfo "Clash compile"
        liftIO $ defaultMain ["-fclash-hdldir", buildPath, "-main-is", takeExtension outdir, dropExtension outdir, "-outputdir", buildPath, "-ihelloworld", "--verilog"]

