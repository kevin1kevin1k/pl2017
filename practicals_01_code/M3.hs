module M3 where
import Prelude ()
import MiniPrelude

f0 :: List a -> List a
f0 = (.-.|.|.--.|.|.-|-|-.+)
f1 :: List a -> List Int
f1 = (.-..|.|-.|...)
(..-.|...|-) ((-..-),(-.--)) = (-..-)
(...|-.|-..) ((-..-),(-.--)) = (-.--)
(..-.|---|.-..|-..|.-.) (..-.) (.| ) [] = (.| )
(..-.|---|.-..|-..|.-.) (..-.) (.| ) ((-..-):(-..-|...)) = (..-.) (-..-) ((..-.|---|.-..|-..|.-.) (..-.) (.| ) (-..-|...))
(--|.-|.--.) (..-.) [] = []
(--|.-|.--.) (..-.) ((-..-) : (-..-|...)) = (..-.) (-..-) : (--|.-|.--.) (..-.) (-..-|...)
(--..|..|.--.) [] _ = []
(--..|..|.--.) ((-..-):(-..-|...)) [] = []
(--..|..|.--.) ((-..-):(-..-|...)) ((-.--):(-.--|...)) = ((-..-),(-.--)) : (--..|..|.--.) (-..-|...) (-.--|...)
(..-.|..|.-..|-|.|.-.) (.--.) = (..-.|---|.-..|-..|.-.) (\(-..-) (-..-|...) -> if (.--.) (-..-) then (-..-) : (-..-|...) else (-..-|...)) []
(....|.|.-|-..) ((-..-):(-..-|...)) = (-..-)
(-|.-|..|.-..) ((-..-):(-..-|...)) = (-..-|...)
(-.-.|---|-.|-.-.|.-|-) = (..-.|---|.-..|-..|.-.) (++) []
(.-..|.|-.|--.|-|....) :: [a] -> Int
(.-..|.|-.|--.|-|....) [] = 0
(.-..|.|-.|--.|-|....) ((-..-):(-..-|...)) = 1 + (.-..|.|-.|--.|-|....) (-..-|...)
(-..|.-.|---|.--.) :: Int -> [a] -> [a]
(-..|.-.|---|.--.) 0 (-..-|...) = (-..-|...)
(-..|.-.|---|.--.) _ [] = []
(-..|.-.|---|.--.) (-.) ((-..-):(-..-|...)) = (-..|.-.|---|.--.) ((-.)-1) (-..-|...)
(.-..|.|-.|...) :: [a] -> [Int]
(.-..|.|-.|...) (-..-|...) = [(..| ) | (..| ) <- [0..(.-..|.|-.|--.|-|....) (-..-|...)]]
(.-.|.|.--.|.|.-|-|-.+) :: [a] -> [a]
(.-.|.|.--.|.|.-|-|-.+) (-..-|...) = (-.-.|---|-.|-.-.|.-|-) ((--|.-|.--.) ((-.-.|---|-.|...|-) (-..-|...)) ((.-..|.|-.|...) (-..-|...)))
(-|.-|-.-|.) :: Int -> [a] -> [a]
(-|.-|-.-|.) 0 _ = []
(-|.-|-.-|.) _ [] = []
(-|.-|-.-|.) (-.) ((-..-):(-..-|...)) = (-..-) : (-|.-|-.-|.) ((-.)-1) (-..-|...)
(-.-.|---|-.|...|-) (-..-) (-.--) = (-..-)
