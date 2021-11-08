
import Prelude
import Development.Shake
import Development.Shake.FilePath
import Clash.Main(defaultMain)

sourcePath :: FilePath
sourcePath = "helloworld"

buildPath :: FilePath
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
        let modName = dropExtension outdir
        let topName = takeExtension outdir
        need [sourcePath </> modName <.> "hs"]
        putInfo $ "Clash compile " ++ outdir
        liftIO $ defaultMain ["-fclash-hdldir", buildPath, "-main-is", topName, modName, "-outputdir", buildPath, "-i" ++ sourcePath, "--verilog"]
