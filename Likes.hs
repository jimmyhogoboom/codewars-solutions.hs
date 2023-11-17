import Text.Printf (printf)

likes ls =
  case ls of
    [] -> "no one likes this"
    [x] -> x ++ " likes this"
    [x, y] -> printf "%s and %s like this" x y
    [x, y, z] -> printf "%s, %s and %s like this" x y z
    (x : y : zs)  -> printf "%s, %s and %d others like this" x y (length zs)
