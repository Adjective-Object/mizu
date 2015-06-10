module Colours where
import Data.List (foldl', reverse)
import Data.List.Split (chunksOf)
import Numeric (readHex, showHex)

class ColourConversion a b where
    convert :: a -> b

data HexColour = Hex String
    deriving (Show, Eq, Ord)

instance ColourConversion HexColour HexColour where
    convert (Hex str)   = Hex str
instance ColourConversion RGBColour HexColour where
    convert (RGB r g b) =
            Hex $ "#"
                ++ showHex (round r) ""
                ++ showHex (round g) ""
                ++ showHex (round b) ""
instance ColourConversion LABColour HexColour where
    convert (LAB l a b) = convert (convert (LAB l a b) :: RGBColour)

data LABColour = LAB Double Double Double
    deriving Show
instance ColourConversion HexColour LABColour where
    convert (Hex str)   = convert (convert (Hex str) :: RGBColour)
instance ColourConversion LABColour LABColour where
    convert (LAB l a b) = LAB l a b
instance ColourConversion RGBColour LABColour where
    convert (RGB r g b) = convert (convert (RGB r g b) :: XYZColour)
instance ColourConversion XYZColour LABColour where
    convert (XYZ x y z) =
        let xFrac = filt $ x / 95.047
            yFrac = filt $ y / 100
            zFrac = filt $ z / 108.883

            filt x = if x > 0.008856
                then x ** ( 1/3 )
                else 7.787 * x + ( 16 / 116 )

        in LAB  (116 * yFrac - 16)
                (500 * (xFrac - yFrac))
                (200 * (yFrac - zFrac))

data RGBColour = RGB Double Double Double
    deriving Show
instance ColourConversion RGBColour RGBColour where
    convert (RGB r g b) = RGB r g b
instance ColourConversion LABColour RGBColour where
    convert (LAB l a b) = RGB l a b -- TODO
instance ColourConversion HexColour RGBColour where
    convert (Hex str)   =
        let strData      = drop 1 str
            hexStrings   = chunksOf (length strData `quot` 3) strData
            readers      = map readHex hexStrings
            first (a, _) = a
            [r, g, b]    = map (\a -> first . head a) readers
        in RGB r g b

data XYZColour = XYZ Double Double Double
    deriving Show
instance ColourConversion XYZColour XYZColour where
    convert (XYZ x y z) = XYZ x y z
instance ColourConversion RGBColour XYZColour where
    convert (RGB r g b) =
        let toXYZChan a =
                let frac = a / 255
                    mid =
                        if frac > 0.04045
                            then ((frac + 0.055) / 1.055) ** 2.4
                            else frac / 12.92
                in mid * 100

            vr = toXYZChan r
            vg = toXYZChan g
            vb = toXYZChan b
        in XYZ  (vr * 0.4124 + vg * 0.3576 + vb * 0.1805)
                (vr * 0.2126 + vg * 0.7152 + vb * 0.0722)
                (vr * 0.0193 + vg * 0.1192 + vb * 0.9505)
instance ColourConversion LABColour XYZColour where
    convert (LAB l a b) =
        let y = (l + 16 ) / 116
            x = y + a / 500
            z = y - b / 200

            unfilt a = if a ** 3 > 0.008856
                then a ** 3
                else (a - 16 / 116) / 7.787

        in XYZ  (95.047  * unfilt x)
                (100.000 * unfilt y)
                (108.883 * unfilt z)
