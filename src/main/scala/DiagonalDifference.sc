def diagonalDifference(a: Array[Array[Int]]): Int =  {
  val dimension = a.length
  val primaryDiagonal = for{
    x <- 0 until dimension
    y <- 0 until dimension
    if x==y
  }yield a(x)(y)
  val secondaryDiagonal = for{
    x <- 0 until dimension
    y <- 0 until dimension
    constraint = dimension-x-1
    if y==constraint && constraint>=0 && constraint<dimension
  }yield a(x)(y)
  val primaryDiagonalSum=primaryDiagonal.sum
  val secondaryDiagonalSum=secondaryDiagonal.sum
  math.abs(primaryDiagonalSum-secondaryDiagonalSum)
}


diagonalDifference(Array(Array(11,2,4),
  Array(4,5,6),
  Array(10,8,-12)))