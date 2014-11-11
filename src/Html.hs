module Html where

import Numeric
import Text.XHtml
import Data.List
import Data.Char
import System.IO.Unsafe

import Paths_lhc_bench

data CellValue
    = TimeCell Double
    | MemoryCell Integer
    | CounterCell Integer
    | RatioCell Double

instance HTML CellValue where
    toHtml (TimeCell v) = toHtml (ppRatio v ++ "s")
    toHtml (CounterCell i) = toHtml (showCounter i)
    toHtml (MemoryCell m) = toHtml (showMemory m)
    toHtml (RatioCell r) = toHtml (showFFloat (Just 1) (r*100) "%")

showCounter :: Integer -> String
showCounter i = go
  where
    reduce size suffix =
        showFFloat (Just 1) (fromIntegral i / size) (' ':suffix)
    go | i < 10^3  = show i
       | i < 10^6  = reduce (10^3) "K"
       | i < 10^9  = reduce (10^6) "M"
       | otherwise = reduce (10^9) "G"

showMemory :: Integer -> String
showMemory i = go
  where
    reduce size suffix =
        showFFloat (Just 1) (fromIntegral i / size) (' ':suffix)
    go | i < 2^10  = show i
       | i < 2^20  = reduce (2^10) "KiB"
       | i < 2^30  = reduce (2^20) "MiB"
       | otherwise = reduce (2^30) "GiB"

data Cell
    = SuccessCell CellValue Double
    | CompileErrorCell String
    | RuntimeErrorCell String
    | RunDisabledCell
    | DiffErrorCell

instance HTML Cell where
    toHtml (SuccessCell a r) = 
        toHtml a +++ toHtml ("(" ++ ppRatio r ++ ")")
    toHtml (CompileErrorCell _) = toHtml "CompileError"
    toHtml (RuntimeErrorCell _) = toHtml "RuntimeError"
    toHtml RunDisabledCell = toHtml "--"
    toHtml DiffErrorCell = toHtml "DiffError"

data Category = Category String [(String, [Cell])]
data Table = Table [(String, [String])] [Category]

geoMean :: [Double] -> Double
geoMean [] = 0
geoMean lst = product lst ** recip (fromIntegral (length lst))

findMeans :: [(String, [Cell])] -> [(Double, Int)]
findMeans = map worker . transpose . map snd
  where
    worker cells =
        let ratios = [ ratio | SuccessCell _ ratio <- cells ]
        in (geoMean ratios, length ratios)

ppRatio :: Double -> String
ppRatio r = showFFloat (Just 1) r ""

cellClass :: Cell -> String
cellClass (SuccessCell _ n)
    | n < 1.1 = "Best"
    | otherwise = ""
cellClass CompileErrorCell{} = "CompileError"
cellClass RuntimeErrorCell{} = "RuntimeError"
cellClass RunDisabledCell = ""
cellClass DiffErrorCell = "DiffError"

trimName :: String -> String
trimName = map toLower . filter (not . isSpace)

tableToHtml :: (String, Table) -> Html
tableToHtml (tableName, Table tHeader categories) =
    (table (concatHtml $ 
        map mkCategories categories))
        ! [ identifier (trimName tableName) ]
  where
    mkHeader (flavor, sub) = 
        td (thediv (toHtml flavor) ! [theclass "Implementation eCell"]
            ) ! [colspan (length sub)]
    mkLabels (flavor, labels) =
        concatHtml
            [ td (thediv (toHtml label) ! [theclass "Version eCell"])
            | label <- labels ]
    mkRow (prog, cells) =
        tr (
        td (toHtml prog) +++
        concatHtml
            [ td (thediv (toHtml cell) ! [theclass $ cellClass cell ++ " Result eCell"])
            | cell <- cells
            ])
    mkMeans total (mean, count) =
        td (toHtml $ ppRatio mean ++ " (" ++ show count ++ "/" ++ show total ++")")
    mkCategories (Category category rows) = let means = findMeans rows in
        tr (
            td (thediv (toHtml category)
                ! [theclass "Category eCell"]
                )
                ! [rowspan 2 ] +++
            concatHtml (map mkHeader tHeader)) +++
        tr (concatHtml (map mkLabels tHeader)) +++
        concatHtml (map mkRow rows) +++
        tr (td (toHtml "mean:") +++ concatHtml (map (mkMeans (length rows)) means))

{-
cat        Flavor1           Flavor2
        label1 label2    label3 label4
prog1     x      y        z       f
prog2     z      y        z       f


-}

cssStyle :: String
cssStyle = unsafePerformIO $ do
    readFile =<< getDataFileName "results.css"

javascript :: String
javascript = unsafePerformIO $ do
    readFile =<< getDataFileName "results.js"

mkAnalysis tables =
    thehtml $
        benchHeader +++
        body (
            select (concatHtml
                [option (toHtml name) ! [value $ trimName name]
                | (name,_) <- tables ]) ! [identifier "selector"] +++
            thediv (concatHtml (map tableToHtml tables))
                ! [ identifier "Scores" ]
            )

benchHeader =
    style (toHtml cssStyle) +++
    tag "script" noHtml ! [ src jquery, thetype "text/javascript" ] +++
    tag "script" (primHtml javascript) ! [thetype "text/javascript" ]
  where
    jquery = "https://code.jquery.com/jquery-1.11.1.min.js"
