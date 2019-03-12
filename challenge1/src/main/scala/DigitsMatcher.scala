object DigitsMatcher {
  def matcher(firstDigit: Int, secondDigit: Int): Option[Int] = {
    if (firstDigit == secondDigit) {
      return Some(firstDigit)
    }
    None
  }

  def captchaSolution(inputList: List[Int]): Int = {
    if (inputList.isEmpty) {
      0
    } else {
      val circularList = inputList :+ inputList.head
      val pairs = circularList zip circularList.tail
      val output = pairs.flatMap(x => DigitsMatcher.matcher(x._1, x._2))
      output.sum
    }
  }
}
