> module Chart (gplot, gplotFile) where

> import Diagrams.Backend.Cairo
> import Diagrams.Coordinates
> import Diagrams.Prelude
> import Graphics.SVGFonts.ReadFont

Each series is a label and a list of points (x-y pairs).  Each series
will be drawn as a separate line, with its own combination of colour,
dashing pattern and shape.

> type Points = [(Double, Double)]
>
> dataSeries :: [(String,Points)]
> dataSeries =
>   [ ("upward",   zip [0.0, 1.0 .. 10.0] [0.0, 1.0 .. 10.0])
>   , ("downward", zip [0.0, 1.0 .. 10.0] [10.0, 9.0 .. 0.0])
>   , ("cycle",    zip [0.0, 1.0 .. 10.0] (cycle [3,4,5]))
>   , ("arbitrary", [(2,4), (4,2), (5,4), (10,5)])
>   , ("sin",      map (\x -> (x, 8+sin x)) [0.0, 0.5 .. 10.0])
>   ]
>
> type DC = Diagram Cairo R2

The final diagram is the chart with the legend next to it.
 
> example :: DC
> example = pad 1.1 . centerXY $
>     (centerY (chart (map snd dataSeries) plotStyles)
>      ||| strutX 1
>      ||| centerY (legend plotStyles (map fst dataSeries)))

> example1 :: DC
> example1 = centerXY $ (chart (map snd dataSeries) plotStyles)

> gplot :: Points -> DC
> gplot ps = centerXY $ chart [ps] plotStyles

> gplotFile :: FilePath -> Points -> IO ()
> gplotFile fname ps = renderCairo fname (mkSizeSpec (Just 800) (Just 400)) $ gplot ps

The size of the chart, in logical units.

> h,w :: Double
> h = 4
> w = 8

The chart is made of the data series, the outer box, and the
horizontal and vertical axes markings.

"dataToFrac" converts points from the "data" space [0..10] into the
[0..1] range.

> chart :: [Points] -> [(DC, DC -> DC)] -> DC
> chart series styles = mconcat [plotMany styles series dataToFrac, box]
>   where maxx = maximum $ map fst $ concat series
>         minx = minimum $ map fst $ concat series
>         maxy = maximum $ map snd $ concat series
>         miny = minimum $ map snd $ concat series
>         xrange = maxx-minx
>         yrange = maxy-miny
>         dataToFrac (x,y) = ((x-minx)/xrange, (y-miny)/yrange)

Plot a single data series.  A "shape" is drawn at every data point,
and straight lines are drawn between points.

> plot :: ((Double,Double) -> (Double,Double)) -> DC -> (DC -> DC) -> [(Double,Double)] -> DC
> plot dataToFrac shape lineStyle ps =
>     let scalify (x,y) = (x*w,y*h)
>         ps' = map (p2 . scalify . dataToFrac) ps
>     in mconcat [ shape # moveTo p | p <- ps' ]

Plot many data series using the given list of styles.

> plotMany :: [(DC, DC -> DC)] -> [[(Double, Double)]] -> ((Double, Double) -> (Double, Double)) -> DC
> plotMany styles seriesList dataToFrac =
>     mconcat $ zipWith (uncurry (plot dataToFrac)) (styles ++ plotStyles) seriesList

A string of text, converted to a path and filled.

> text' :: String -> DC
> text' s = (stroke $ textSVG' (TextOpts s lin2 INSIDE_H KERN False 0.4 0.4)) # fc black # lw 0

The chart's legend.  Each label is drawn next to a little example of
how the line looks in the chart.

> legend :: [(DC, DC -> DC)] -> [String] -> DC
> legend styles labels = centerXY $
>     vcat' with {sep=0.15} $
>       map (\(l,s) -> littleLine s ||| strutX 0.4 ||| text' l # alignL)
>         (zip labels (styles ++ plotStyles))
>   where littleLine (d,l) = (stroke $ fromVertices [ 0&0, 1&0 ]) # l
>                            <> d # moveTo (0.5&0)

The outer box is just a rectangle.

> box :: DC
> box = strokeLoop . closeLine . fromVertices $ [ 0&0, 0&h, w&h, w&0 ]

Each tick on the vertical axis has a text part, a solid line on the
left, a solid line on the right, and a long dashed line from left to
right.

> vertticks :: [(Double,String)] -> DC
> vertticks pairs =
>     let textBits = mconcat [ text' t # alignR # moveTo ((-0.2)&(y*h)) | (y,t) <- pairs ]
>         tickBits =    mconcat [ fromVertices [ 0&(y*h), 0.1    &(y*h) ] | (y,_) <- pairs ]
>                    <> mconcat [ fromVertices [ w&(y*h), (w-0.1)&(y*h) ] | (y,_) <- pairs ]
>                    <> mconcat [ fromVertices [ 0&(y*h), w&(y*h)       ] # lc gray # dashing [ 0.1, 0.1 ] 0 | (y,_) <- pairs ]
>     in textBits <> tickBits

(Similar for the horizontal axis.)

> horizticks :: [(Double,String)] -> DC
> horizticks pairs =
>     let textBits = mconcat [ text' t # moveTo ((x*w)&(-0.3)) | (x,t) <- pairs ]
>         tickBits =    mconcat [ fromVertices [ (x*w)&0, (x*w)&0.1     ] | (x,_) <- pairs ]
>                    <> mconcat [ fromVertices [ (x*w)&h, (x*w)&(h-0.1) ] | (x,_) <- pairs ]
>                    <> mconcat [ fromVertices [ (x*w)&0, (x*w)&h       ] # lc gray # dashing [ 0.1, 0.1 ] 0 | (x,_) <- pairs ]
>     in textBits <> tickBits

A dot style is a shape (any diagram) and a boolean indicating whether
the shape should be filled, a line style is a dashing pattern, and a
colour style is just a colour.  These three combined give a "style".

> newtype Fill = Fill Bool
> type Shape = DC
> type DotStyle = (Shape, Fill)
> type LineStyle = DC -> DC
> 
> plotStyles :: [ (Shape, LineStyle) ]
> plotStyles = zipWith3 combineStyles dotStyles colourStyles lineStyles
> 
> combineStyles :: DotStyle -> Colour Double -> LineStyle -> (Shape, LineStyle)
> combineStyles (d,Fill f) c l =
>   ( d # (if f then fcA (c `withOpacity` 0.5) else id) # lc c, lc c . l )

The dot styles.

> dotStyles :: [DotStyle]
> dotStyles = cycle $
>     let shapes = map (stroke)
>            [ circle 0.02
>            , square 0.1
>            , eqTriangle 0.1
>            , pentagon 0.1
>            , cross 0.07
>            , plus 0.07
>            , star (StarSkip 2) (pentagon 0.1)
>            ]
>     in [ (s, Fill b) | b <- [True,False], s <- shapes ]

Some custom shapes.

> cross :: Double -> Path R2
> cross x = fromVertices [ x&(-x) , ((-x)&x) ]
>           <> fromVertices [ x&x , ((-x)&(-x)) ]
> 
> plus :: Double -> Path R2
> plus x = cross x # rotate (45::Deg)

The colour styles.

> colourStyles :: [Colour Double]
> colourStyles = cycle $ [ red, green, blue, brown ]

The line styles.

> lineStyles :: [DC -> DC]
> lineStyles = cycle . map (. lw 0.03) $
>                [ id, dashing [0.1,0.1] 0, dashing [0.02,0.02] 0
>                , dashing [0.1,0.1,0.03,0.1] 0, dashing [0.1,0.1,0.02,0.02,0.02,0.1] 0 ]
