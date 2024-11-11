import Graphics.Gloss

main :: IO()
main = display (InWindow "" (200, 200) (10, 10)) white (Circle 80)