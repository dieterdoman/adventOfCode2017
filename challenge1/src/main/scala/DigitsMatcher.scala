object DigitsMatcher {
  def matcher(pair: (Int, Int)): Option[Int] = {
    if (pair._1 == pair._2) {
      return Some(pair._1)
    }
    None
  }

  def captchaSolution(inputList: List[Int]): Int = {
    if (inputList.isEmpty) {
      0
    } else {
      val circularList = inputList :+ inputList.head
      val pairs = circularList zip circularList.tail
      val output = pairs.flatMap(DigitsMatcher.matcher)
      output.sum
    }
  }
}
