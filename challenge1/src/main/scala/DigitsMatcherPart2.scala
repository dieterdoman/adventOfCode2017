object DigitsMatcherPart2 {
  def matcher(pair: (Int, Int)): Option[Int] = {
    if (pair._1 == pair._2) {
      return Some(pair._1)
    }
    None
  }

  def captchaSolution(inputList: List[Int]): Int = {
    val stepsForward = inputList.length / 2
    val circularList = inputList ::: inputList.splitAt(stepsForward)._1
    val matchingList = circularList.splitAt(stepsForward)._2
    val pairs = circularList zip matchingList
    val output = pairs.flatMap(DigitsMatcher.matcher)
    output.sum
  }
}
