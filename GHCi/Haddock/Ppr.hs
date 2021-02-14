{-# LANGUAGE BangPatterns #-}
module GHCi.Haddock.Ppr
  ( pprHaddock
  ) where

import           Control.Monad
import           Data.Bitraversable
import           Data.Foldable
import qualified Data.Map.Strict as Map
import           Data.Maybe
import           Data.Sequence (Seq ((:<|), (:|>)), (<|), (><), (|>))
import qualified Data.Sequence as Seq
import           Prelude hiding ((<>))
import qualified Prelude as P

import           Outputable
import           PprColour
import qualified Documentation.Haddock.Markup as H
import qualified Documentation.Haddock.Parser as H
import qualified Documentation.Haddock.Types as H

pprHaddock :: String -> SDoc
pprHaddock = pprDocH . H.toRegular . H._doc . H.parseParas Nothing

pprDocH :: H.DocH mod String -> SDoc
pprDocH = builderToSDoc . ($ IgnoreLeadingWS) . H.markup builderMarkup

-- | A single line of output
data Line =
  Line
    { lineDoc :: !SDoc
    , lineLen :: !Int -- need line length for building tables
    }

joinLines :: Line -> Line -> Line
joinLines a b =
  Line { lineDoc = lineDoc a <> lineDoc b
       , lineLen = lineLen a + lineLen b
       }

-- | A grouping of lines. Blocks are separated by a blank line when printed
type Block = Seq Line

-- | The collection of blocks that make up the whole output
type Blocks = Seq Block

-- | A 'Monoid' for building the terminal output from a DocH
data Builder
  = Builder Blocks BreakMode
  | EmptyBuilder

instance Semigroup Builder where
  (<>) = combineBuilders
instance Monoid Builder where
  mempty = EmptyBuilder

-- | Indicates how one 'Builder' should be combined with another
data BreakMode
  = NoBreak
  | NewLine
  | NewBlock
  deriving Show

-- | Turns a 'Builder' into the resulting output 'SDoc'
builderToSDoc :: Builder -> SDoc
builderToSDoc EmptyBuilder = empty
builderToSDoc (Builder blocks _) =
  concatBlocks
    $ fmap (foldl' appendLine empty) (fmap lineDoc <$> blocks)
  where
    appendBlock = ($+$) . ($+$ text "")
    appendLine = ($+$)
    concatBlocks (first :<| rest) = foldl' appendBlock first rest
    concatBlocks _ = empty

combineBuilders :: Builder -> Builder -> Builder
combineBuilders EmptyBuilder b = b
combineBuilders b EmptyBuilder = b
combineBuilders (Builder xs xBr) (Builder ys yBr) =
  Builder (appendBlocks xBr xs ys) yBr

-- | Combine two groups of blocks according to a 'BreakMode'
--
-- - 'NoBreak' causes the inner two blocks to be merged by concating the two
--   innermost lines
-- - 'NewLine' Combines the two innermost blocks into a single block
-- - 'NewBlock' simply concatenates the two groups
appendBlocks :: BreakMode -> Blocks -> Blocks -> Blocks
appendBlocks NoBreak xs ys
  | xs'       :|> lstBlock  <- xs
  , lstBlock' :|> lstLine   <- lstBlock
  , fstBlock  :<| ys'       <- ys
  , fstLine   :<| fstBlock' <- fstBlock
  , let joinedLine     = joinLines lstLine fstLine
        joinedLstBlock = lstBlock' |> joinedLine
        leftBlocks     = xs' |> (joinedLstBlock >< fstBlock')
  = leftBlocks >< ys'
  | otherwise = xs >< ys
appendBlocks NewLine xs ys
  | xs'      :|> lstBlock <- xs
  , fstBlock :<| ys'      <- ys
  , let joinedLstBlock = lstBlock >< fstBlock
        leftBlocks     = xs' |> joinedLstBlock
  = leftBlocks >< ys'
  | otherwise = xs >< ys
appendBlocks NewBlock xs ys = xs >< ys

data StringOptions
  = IgnoreLeadingWS
  | KeepLeadingWS

-- | Set of instruction for translating 'DocH's into 'Builder's. The
-- 'StringOptions' argument is so we can use different ways of handling leading
-- whitespace for newlines depending on the context.
builderMarkup :: H.DocMarkupH mod String (StringOptions -> Builder)
builderMarkup =
  H.Markup
    { H.markupEmpty                = mempty
    , H.markupString               = fromString
    , H.markupParagraph            = \b _ -> setBreakMode NewBlock $ b IgnoreLeadingWS
    , H.markupAppend               = mappend
    , H.markupIdentifier           = pprIdentifier
    , H.markupIdentifierUnchecked  = mempty
    , H.markupModule               = pprModule
    , H.markupWarning              = id
    , H.markupEmphasis             = fmap $ mapDocs keyword
    , H.markupBold                 = fmap $ mapDocs keyword
    , H.markupMonospaced           = fmap $ mapDocs (coloured colWhiteFg)
    , H.markupUnorderedList        = pprUnorderedList
    , H.markupOrderedList          = pprOrderedList
    , H.markupDefList              = pprDefList
    , H.markupCodeBlock            = pprCodeBlock
    , H.markupHyperlink            = pprHyperlink
    , H.markupAName                = fromString
    , H.markupPic                  = mempty
    , H.markupMathInline           = fromString
    , H.markupMathDisplay          = fromString
    , H.markupProperty             = fromString
    , H.markupExample              = pprExample
    , H.markupHeader               = \(H.Header _ inner) o -> mapDocs keyword (inner o)
    , H.markupTable                = \t _ -> pprTable $ fmap ($ KeepLeadingWS) t
    }

mapLines :: (Line -> Line) -> Builder -> Builder
mapLines _ EmptyBuilder = EmptyBuilder
mapLines f (Builder blocks br) = Builder (fmap (fmap f) blocks) br

mapDocs :: (SDoc -> SDoc) -> Builder -> Builder
mapDocs f = mapLines f' where
  f' (Line d l) = Line (f d) l -- should not change length

setBreakMode :: BreakMode -> Builder -> Builder
setBreakMode _ EmptyBuilder = EmptyBuilder
setBreakMode br (Builder blocks _) = Builder blocks br

wrap :: String -> Blocks
wrap str = pure . pure $ Line (text str) (length str)

-- | Convert a string to a builder, respecting newlines
fromString :: String -> StringOptions -> Builder
fromString "" _ = mempty
fromString xs opts
  | (s, '\n':rest) <- break (== '\n') xs
  , let bldr = Builder (wrap s) NewLine
        rBldr = case opts of
                  IgnoreLeadingWS -> fromString (dropWhile (== ' ') rest) opts
                  KeepLeadingWS -> fromString rest opts
  = bldr `mappend` rBldr
  | otherwise
  = Builder (wrap xs) NoBreak

pprIdentifier :: String -> StringOptions -> Builder
pprIdentifier str opts = mapDocs (coloured $ colCustom "94")
                       $ fromString str opts

pprModule :: String -> StringOptions -> Builder
pprModule str _ = mapDocs (coloured $ colCustom "92") $ Builder (wrap str) NoBreak

pprHyperlink :: H.Hyperlink (StringOptions -> Builder) -> StringOptions -> Builder
pprHyperlink (H.Hyperlink url mbBldr) opt =
  mconcat
  [ case mbBldr of
      Just bldr -> bldr opt `mappend` Builder (wrap " ") NoBreak
      Nothing -> mempty
  , Builder (wrap $ "(" ++ url ++ ")") NoBreak
  ]

pprUnorderedList :: [StringOptions -> Builder] -> StringOptions -> Builder
pprUnorderedList items
  = setBreakMode NewBlock . foldMap unorderedListItem
  . sequence items

unorderedListItem :: Builder -> Builder
unorderedListItem = listItem "•"

pprOrderedList :: [StringOptions -> Builder] -> StringOptions -> Builder
pprOrderedList items
  = setBreakMode NewBlock . foldMap orderedListItem . zip [1..]
  . sequence items

orderedListItem :: (Int, Builder) -> Builder
orderedListItem (i, builder) = listItem label builder
  where label = show i ++ "."

pprDefList :: [(StringOptions -> Builder, StringOptions -> Builder)]
           -> StringOptions -> Builder
pprDefList defs
  = setBreakMode NewBlock . foldMap definition
  . traverse bisequence defs

definition :: (Builder, Builder) -> Builder
definition (definiendum, definiens) =
  setBreakMode NewLine $
    setBreakMode NewLine definiendum
      `mappend` mapLines indent definiens

pprCodeBlock :: (StringOptions -> Builder) -> StringOptions -> Builder
pprCodeBlock builder _
  = setBreakMode NewBlock . mapDocs (coloured colWhiteFg) $ builder KeepLeadingWS

indent :: Line -> Line
indent (Line d l) = Line (nest 2 d) (l + 2)

-- | Prefix the first line of the first block with a given label. All other
-- lines are indented.
listItem :: String -> Builder -> Builder
listItem _ EmptyBuilder = EmptyBuilder
listItem label (Builder blocks _) = Builder itemBlock NewLine
  where
    itemBlock
      | fstBlock :<| blocks'   <- blocks
      , fstLine  :<| fstBlock' <- fstBlock
      , let labeledFstBlock = addLabel fstLine <| fmap indent fstBlock'
            tailBlocks      = fmap (fmap indent) blocks'
      = labeledFstBlock <| tailBlocks
      | otherwise = blocks
    addLabel (Line d l) = Line (text label <+> d) (l + length label + 1)

pprExample :: [H.Example] -> StringOptions -> Builder
pprExample exs _ = setBreakMode NewBlock $ foldMap exampleBuilder exs

exampleBuilder :: H.Example -> Builder
exampleBuilder (H.Example expr res) =
  let sdoc = text "λ>" <+> text expr
      exprBldr = Builder (pure . pure . Line sdoc $! length expr + 3) NewLine
      resBldr  = foldMap (setBreakMode NewLine . flip fromString IgnoreLeadingWS) res
   in mapDocs (coloured colWhiteFg) $ exprBldr `mappend` resBldr

--------------------------------------------------------------------------------
-- Tables
--------------------------------------------------------------------------------

{-
Note [Rendering Tables]
~~~~~~~~~~~~~~~~~~~~~~~
Tables present the greatest complexity for pretty printing haddocks, the main
reason being that table cells can span multiple rows or columns. 'TableCell's
that span across rows only appear in the row where they first occur, so that
the next 'TableRow' won't have a cell in that column position. To account for
this, the implementation used here keeps track of a list of the row spanning
cells extending from previous rows along with their column index. This way if
the index of the column being looked at matches a spanning cell, we utilize
that cell rather than looking at the next cell in the row.
-}

type ColWidth = Int
type ColIdx = Int
type ColSpan = Int
type RowSpan = Int

-- | Identifies a cell that spans multiple rows/columns by the index of the
-- starting column, index of the ending column, and the number of rows spanned.
type CellSpan = (ColIdx, ColIdx, RowSpan)

-- | Used to collect the position and width of a cell to aide in calculating
-- the overall width of each column.
data CellSize =
  CellSize
    { endCol :: !ColIdx
    , startCol :: !ColIdx
    , cellWidth :: !Int
    } deriving Show

pprTable :: H.Table Builder -> Builder
pprTable tbl@(H.Table header body) =
  Builder (Seq.singleton . Seq.fromList $ (`Line` tableWidth) <$> lns) NewBlock
  where
    colWidths = columnWidths tbl
    lns =
      doRows
        ( (Just <$> header) `zip` reverse (True : replicate (length header - 1) False)
       ++ (Just <$> body) `zip` repeat False
        )

    doRows rows = resultLines []
      where
        (_, resultLines) =
          foldl' go ([], id)
            $ zip ((Nothing, False) : rows) (rows ++ [(Nothing, False)])

        go :: ([(CellSpan, Builder)], [SDoc] -> [SDoc])
           -> ((Maybe (H.TableRow Builder), Bool), (Maybe (H.TableRow Builder), Bool))
           -> ([(CellSpan, Builder)], [SDoc] -> [SDoc])
        go (spans, ls) ((mbPrevR, isHeader), (mbNextR, _)) =
          let (spans', divSDoc) =
                pprDivider isHeader colWidths mbPrevR mbNextR spans
              (spans'', newLines)
                | Just r <- mbNextR
                = pprRow colWidths r spans'
                | otherwise = (spans', [])
           in (spans'', ls . ((divSDoc : newLines) ++))

    tableWidth = sum colWidths + length colWidths + 1

-- | Calculate the width of each column in the table.
columnWidths :: H.Table Builder -> [ColWidth]
columnWidths (H.Table header body) = colLength (header ++ body)
  where
    colLength :: [H.TableRow Builder] -> [ColWidth]
    colLength = scanColWidths 0 1 . snd . foldl' folder ([], [])

    folder :: ([(ColIdx, (RowSpan, ColSpan))], [[CellSize]])
           -> H.TableRow Builder
           -> ([(ColIdx, (RowSpan, ColSpan))], [[CellSize]])
    folder (spans, csizes) row = (: csizes) <$> measureRow spans row

    -- Produce a list of cell sizes for a row
    measureRow :: [(ColIdx, (RowSpan, ColSpan))] -> H.TableRow Builder
               -> ([(ColIdx, (RowSpan, ColSpan))], [CellSize])
    measureRow spanCells (H.TableRow cs) = go 0 spanCells cs where
      go :: ColIdx
         -> [(ColIdx, (RowSpan, ColSpan))]
         -> [H.TableCell Builder]
         -> ([(ColIdx, (RowSpan, ColSpan))], [CellSize])
      go ci ((si, (rowSpan, colSpan)) : spans) cells
        | ci == si
        , let (spans', cells') = go (ci + colSpan) spans cells
              spans'' | rowSpan > 1 = (si, (rowSpan - 1, colSpan)) : spans'
                      | otherwise = spans'
        = (spans'', cells')
      go ci spans (tc : tailCells) =
        let c = CellSize
                  { endCol = ci + H.tableCellColspan tc
                  , startCol = ci
                  , cellWidth =
                      case H.tableCellContents tc of
                        EmptyBuilder -> 0
                        Builder blocks _ ->
                          foldl' max 0 . fmap lineLen $ join blocks
                  }
            (spans', cells') = go (ci + H.tableCellColspan tc) spans tailCells
            spans'' | H.tableCellRowspan tc > 1
                    = (ci, (H.tableCellRowspan tc - 1, H.tableCellColspan tc)) : spans'
                    | otherwise = spans'
         in (spans'', c : cells')
      go _ _ _ = ([], [])

    -- Consume the rows of cell sizes from left to right to find the column
    -- width for each column.
    scanColWidths :: ColWidth -> ColIdx -> [[CellSize]] -> [ColWidth]
    scanColWidths _ _ [] = []
    scanColWidths prevColWidth i rows = findMaxColumn
                                      . fmap (filter (not . null))
                                      . unzip $ map popColumn rows
      where
        -- Given the column widths for each row, determine the max width, then
        -- recurse over the remaining columns.
        findMaxColumn :: ([Maybe ColWidth], [[CellSize]]) -> [ColWidth]
        findMaxColumn (widths, next) =
          let colWidth = foldl' max 0 (catMaybes widths)
           in colWidth : scanColWidths colWidth (i + 1) next

        -- Finds the width of the i'th column in a row if there is a cell
        -- starting at that column index. Also returns the remaing cells in the
        -- row, having subtracted the previous column width if the cell started
        -- at a previous column.
        popColumn :: [CellSize] -> (Maybe ColWidth, [CellSize])
        popColumn [] = (Nothing, [])
        popColumn (c:cs)
          | endCol c == i
          , let w | startCol c == i - 1 = cellWidth c
                  | otherwise = cellWidth c - prevColWidth - 1 -- minus 1 for the border
          = (Just w, cs)
          | otherwise =
            let newWidth | startCol c == i - 1 = cellWidth c
                         | otherwise = cellWidth c - prevColWidth - 1
                c' = c { cellWidth = newWidth }
             in (Nothing, c' : cs)

-- | Render the cell contents within a row
pprRow :: [ColWidth]
       -> H.TableRow Builder
       -> [(CellSpan, Builder)]
       -> ([(CellSpan, Builder)], [SDoc])
pprRow colWidths row ss =
  let (resCells, resSpans, lineSDocs) =
        foldr (\_ (cells, spans, lns) ->
                let (cells', spans', line) = go 0 cells spans colWidths
                 in (cells', spans', (tblFrame (char '│') <> line) : lns)
              )
              (H.tableRowCells row, ss, [])
              [1 .. rowLines]
   in (nextSpans 0 resCells resSpans, reverse lineSDocs)

  where
    go :: ColIdx
       -> [H.TableCell Builder]
       -> [(CellSpan, Builder)]
       -> [ColWidth]
       -> ([H.TableCell Builder], [(CellSpan, Builder)], SDoc)
    go i cells (((si, se, sd), b) : spans) ws
      | i == si
      , let (widths, nxtWidths) = splitAt (se - si) ws
            width = sum widths + length widths - 1
            (mbLn, b') = popLine b
            ln = pad width mbLn
            (cells', spans', sdoc) = go se cells spans nxtWidths
      = ( cells'
        , ((si, se, sd), b') : spans'
        , ln <> (tblFrame (char '│') <> sdoc)
        )
    go i (c : cells) spans ws =
      let colSpan = H.tableCellColspan c
          (widths, nxtWidths) = splitAt colSpan ws
          width = sum widths + length widths - 1
          (mbLine, bldr) = popLine (H.tableCellContents c)
          ln = pad width mbLine
          c' = c { H.tableCellContents = bldr }
          (cells', spans', sdoc) = go (i + colSpan) cells spans nxtWidths
       in ( c' : cells'
          , spans'
          , ln <> (tblFrame (char '│') <> sdoc)
          )
    go _ _ _ _ = ([], [], empty)

    pad :: ColWidth -> Maybe Line -> SDoc
    pad width (Just (Line d l)) = d <> spaces
      where spaces = text $ replicate (width - l) ' '
    pad width _ = text $ replicate width ' '

    rowLines = numRowLines row ss

    -- determine the cell span info to be passed to the next row
    nextSpans i cells (((si, se, sd), b) : spans)
      | i == si
      , let spans' = nextSpans se cells spans
      = ((si, se, sd - 1), b) : spans'
    nextSpans i (c : cells) spans =
      let rowSpan = H.tableCellRowspan c
          colSpan = H.tableCellColspan c
          spans' = nextSpans (i + colSpan) cells spans
          newSpans | rowSpan > 1
                   = ((i, i + colSpan, rowSpan - 1), H.tableCellContents c) : spans'
                   | otherwise = spans'
       in newSpans
    nextSpans _ _ _ = []

-- | Determines how many lines are in a row
numRowLines :: H.TableRow Builder
            -> [(CellSpan, Builder)]
            -> Int
numRowLines (H.TableRow cells) spans =
  let spanBldr = snd <$> filter (\((_, _, sd), _) -> sd == 1) spans
      cellBldr =
        H.tableCellContents <$> filter (\c -> H.tableCellRowspan c == 1) cells
      bldrLines EmptyBuilder = 0
      bldrLines (Builder blocks _) = length (join blocks) + length blocks - 1 -- account for empty lines between blocks
   in foldl' max 0 (bldrLines <$> spanBldr ++ cellBldr)

-- | Removes the first line from the first block of a builder and returns it
-- along with the new builder. If the first block is empty, it is removed and
-- a blank line is returned.
popLine :: Builder -> (Maybe Line, Builder)
popLine EmptyBuilder = (Nothing, EmptyBuilder)
popLine (Builder blocks br)
  | fstBlock :<| blocks'   <- blocks
  , fstLine  :<| fstBlock' <- fstBlock
  = (Just fstLine, Builder (fstBlock' <| blocks') br)
  | fstBlock :<| blocks' <- blocks
  , null fstBlock
  = (Just (Line (text "") 0), Builder blocks' br)
  | otherwise = (Nothing, EmptyBuilder)

-- | Forms the border along the top of the table
topBorder :: H.TableRow Builder
          -> [ColWidth]
          -> SDoc
topBorder (H.TableRow cs) = go 0 cs where
  go :: ColIdx -> [H.TableCell Builder] -> [ColWidth] -> SDoc
  go !i (c : cells) widths =
    let colSpan = H.tableCellColspan c
        (ws, widths') = splitAt colSpan widths
        width = sum ws + length ws - 1
        frame = tblFrame . text $ replicate width '─'
        divider | i == 0 = tblFrame $ char '┌'
                | otherwise = tblFrame $ char '┬'
        rest = go (i + 1) cells widths'
     in divider <> frame <> rest
  go _ _ _ = tblFrame $ char '┐'

-- | Forms the border along the bottom of the table
bottomBorder :: H.TableRow Builder
             -> [ColWidth]
             -> [(CellSpan, Builder)]
             -> SDoc
bottomBorder (H.TableRow cs) = go 0 cs where
  go i cells widths (((si, se, _), _) : spans)
    | i == si
    , let (ws, widths') = splitAt (se - si) widths
          width = sum ws + length ws - 1
          divider | i == 0 = tblFrame $ char '└'
                  | otherwise = tblFrame $ char '┴'
          frame = tblFrame . text $ replicate width '─'
          rest = go se cells widths' spans
    = divider <> frame <> rest
  go i (c : cells) widths spans =
    let colSpan = H.tableCellColspan c
        (ws, widths') = splitAt colSpan widths
        width = sum ws + length ws - 1
        divider | i == 0 = tblFrame $ char '└'
                | otherwise = tblFrame $ char '┴'
        frame = tblFrame . text $ replicate width '─'
        rest = go (i + colSpan) cells widths' spans
     in divider <> frame <> rest
  go _ _ _ _ = tblFrame $ char '┘'

-- | Renders the table frame characters dividing the rows of cells or forming
-- the top and bottom of the table.
pprDivider :: Bool -- True <=> divides header from body
           -> [ColWidth]
           -> Maybe (H.TableRow Builder) -- previous row
           -> Maybe (H.TableRow Builder) -- next row
           -> [(CellSpan, Builder)]
           -> ([(CellSpan, Builder)], SDoc)
pprDivider isHeader colWidths (Just prevRow) (Just nextRow) spans =
  let dividers = buildDividers prevRow nextRow spans
   in pprDivider' isHeader colWidths dividers
pprDivider _ colWidths Nothing (Just nextRow) _ =
  ([], topBorder nextRow colWidths)
pprDivider _ colWidths (Just prevRow) Nothing spans =
  ([], bottomBorder prevRow colWidths spans)
pprDivider _ _ Nothing Nothing _ = ([], empty)

pprDivider' :: Bool -- True <=> divides header from body
            -> [ColWidth]
            -> [Divider]
            -> ([(CellSpan, Builder)], SDoc)
pprDivider' isHeader = go False where
  go True _ [] = ([], tblFrame $ if isHeader then char '╡' else char '┤')
  go False _ [] = ([], tblFrame $ char '│')
  go splitOnLeft widths (divider:divs) =
    case divider of
      DivAbove n ->
        let (width, widths') = getCellWidth widths n
            d | isHeader = char '╧'
              | otherwise = char '┴'
            (spans, rest) = go True widths' divs
         in (spans, buildFrame d width <> rest)
      DivBelow n ->
        let (width, widths') = getCellWidth widths n
            d | isHeader = char '╤'
              | otherwise = char '┬'
            (spans, rest) = go True widths' divs
         in (spans, buildFrame d width <> rest)
      DivBoth n ->
        let (width, widths') = getCellWidth widths n
            d | splitOnLeft = if isHeader then char '╪' else char '┼'
              | otherwise = if isHeader then char '╞' else char '├'
            (spans, rest) = go True widths' divs
         in (spans, buildFrame d width <> rest)
      DivNeither ((si, se, sd), b) ->
        let (width, widths') = getCellWidth widths (se - si)
            d | splitOnLeft = tblFrame $ char '┤'
              | otherwise = tblFrame $ char '│'
            (mbLn, b') = popLine b
            ln = pad width mbLn
            (spans, rest) = go False widths' divs
         in ( ((si, se, sd), b') : spans
            , d <> ln <> rest
            )
  getCellWidth ws n = let (w, ws') = splitAt n ws
                       in (sum w + length w - 1, ws')
  buildFrame d w = tblFrame
                 $ d <> text (replicate w $ if isHeader then '═' else '─')

  pad :: Int -> Maybe Line -> SDoc
  pad width (Just (Line d l)) = d <> spaces
    where spaces = text $ replicate (width - l) ' '
  pad width _ = text $ replicate width ' '

-- | Used when rendering horizontal dividers in the table
data Divider
  = DivAbove ColSpan -- ^ Cell division in previous row
  | DivBelow ColSpan -- ^ Cell division in next row
  | DivBoth ColSpan -- ^ Cell division in both rows
  | DivNeither (CellSpan, Builder) -- ^ A cell spans across the divider

buildDividers :: H.TableRow Builder
              -> H.TableRow Builder
              -> [(CellSpan, Builder)]
              -> [Divider]
buildDividers prevRow nextRow spans =
  let above = dividerTop prevRow spans
      below = dividerBottom nextRow spans
      neither = dividerNeither spans
      am = Map.fromList [ (i, DivAbove a) | (i, a) <- above ]
      bm = Map.fromList [ (i, DivBelow b) | (i, b) <- below ]
      nm = Map.fromList [ (si, DivNeither n) | n@((si,_,_),_) <- neither ]
      combine (DivAbove a) (DivBelow b) = DivBoth $ min a b
      combine x _ = x
      abm = Map.unionWith combine am bm

   in Map.elems $ Map.union nm abm

dividerTop :: H.TableRow Builder
           -> [(CellSpan, Builder)]
           -> [(ColIdx, ColSpan)]
dividerTop (H.TableRow cells) = go 0 (filter notSpan cells) where
  notSpan c = H.tableCellRowspan c == 1 -- span cells are covered already
  go i cs (((si, se, sd), _) : spans)
    | i == si
    , let rest = go se cs spans
    = if sd == 0 then (si, se - si) : rest else rest
  go i (c : cs) spans =
    let colSpan = H.tableCellColspan c
        rowSpan = H.tableCellRowspan c
        endI = i + colSpan
        rest = go endI cs spans
     in if rowSpan > 1
           then rest
           else (i, colSpan) : rest
  go _ _ _ = []

dividerBottom :: H.TableRow Builder
              -> [(CellSpan, Builder)]
              -> [(ColIdx, ColSpan)]
dividerBottom (H.TableRow cells) = go 0 cells where
  go i cs (((si, se, sd), _) : spans)
    | i == si
    , sd > 0
    = go se cs spans
  go i (c : cs) spans =
    let colSpan = H.tableCellColspan c
        endI = i + colSpan
        rest = go endI cs spans
     in (i, colSpan) : rest
  go _ _ _ = []

dividerNeither :: [(CellSpan, Builder)]
               -> [(CellSpan, Builder)]
dividerNeither = filter neitherSpan where
  neitherSpan ((_, _, sd), _) = sd > 0

-- | Styling for table frame characters
tblFrame :: SDoc -> SDoc
tblFrame = coloured colCyanFg

