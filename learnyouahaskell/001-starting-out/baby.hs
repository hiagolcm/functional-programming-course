doubleMe x = x + x

doubleUs x y = doubleMe x + doubleMe y

doubleSmallNumber x = if x <= 100 then x * 2 else x

doubleSmallNumber' x = (if x <= 100 then x * 2 else x) + 1

rightTriangles = [(y,z,x)| x <- [1..10], y <- [1..x], z <- [1..y], y ^ 2 + z ^ 2 == x ^ 2, x + y + z == 24]