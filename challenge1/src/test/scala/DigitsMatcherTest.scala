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
    val input5 = List[Int](1, 2, 1, 2)
    val input6 = List[Int](1, 2, 2, 1)
    val input7 = List[Int](1, 2, 3, 4, 2, 5)
    val input8 = List[Int](1, 2, 3, 1, 2, 3)
    val input9 = List[Int](1, 2, 1, 3, 1, 4, 1, 5)
    assert(DigitsMatcher.captchaSolution(List.empty) === 0)
    assert(DigitsMatcher.captchaSolution(input1) === 3)
    assert(DigitsMatcher.captchaSolution(input2) === 4)
    assert(DigitsMatcher.captchaSolution(input3) === 0)
    assert(DigitsMatcher.captchaSolution(input4) === 9)
    assert(DigitsMatcherPart2.captchaSolution(input5) === 6)
    assert(DigitsMatcherPart2.captchaSolution(input6) === 0)
    assert(DigitsMatcherPart2.captchaSolution(input7) === 4)
    assert(DigitsMatcherPart2.captchaSolution(input8) === 12)
    assert(DigitsMatcherPart2.captchaSolution(input9) === 4)
  }
}
