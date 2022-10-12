roots :: (Double, Double, Double) -> (Double, Double)
roots (a, b, c) = 
    let d = sqrt (b * b - 4 * a * c)
        e = 2 * a
    in ( (-b - d) / e, (-b + d) / e )




unitVec2D :: (Double, Double) -> (Double, Double)
unitVec2D (a, b) = 
    let e = sqrt(a * a + b * b)
    in (a / e, b / e)


unitVec3D :: (Double, Double, Double) -> (Double, Double, Double)
unitVec3D (a, b, c) = 
    let e = sqrt(a * a + b * b + c * c)
    in (a / e, b / e, c / e)


heron :: (Double, Double, Double) -> Double
heron (a, b, c) = 
   let p = (a + b + c) / 2
   in sqrt(p * (p - a) * (p - b) * (p - c))