import org.scalatest.FunSuite

class DigitsMatcherTest extends FunSuite {
  test("DigitsMatcher.matcher") {
    assert(DigitsMatcher.matcher(3, 3) === Option(3))
    assert(DigitsMatcher.matcher(3, 4) === None)
  }
  test("DigitsMatcher.captchaSolution") {
    val input1 = List[Int](1, 1, 2, 2)
    val input2 = List[Int](1, 1, 1, 1)
    val input3 = List[Int](1, 2, 3, 4)
    val input4 = List[Int](9, 1, 2, 1, 2, 1, 2, 9)
    assert(DigitsMatcher.captchaSolution(List.empty) === 0)
    assert(DigitsMatcher.captchaSolution(input1) === 3)
    assert(DigitsMatcher.captchaSolution(input2) === 4)
    assert(DigitsMatcher.captchaSolution(input3) === 0)
    assert(DigitsMatcher.captchaSolution(input4) === 9)
  }
}
