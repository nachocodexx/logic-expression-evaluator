import org.scalatest.funsuite.AnyFunSuite
import io.codex.evaluator.LogicExpressionEvaluator._
import io.codex.utils.Utils.{leftParenthesisWithIndexes,rightParenthesisWithIndexes,biconditionalWithIndexes,conditionalWithIndexes}
import io.codex.converter.PostfixConverter._
class PostfixTest extends  AnyFunSuite {
  test("Logic Expression: P+Q"){
    val input ="P+Q"
    val result = evaluate(input)
    val answer = 0::1::1::1::Nil
    assert(result._1==answer)
  }
  test("Logic Expression: P*Q"){
    val input ="P*Q"
    val result = evaluate(input)
    val answer = 0::0::0::1::Nil
    assert(result._1==answer)
  }
  test("Logic Expression: ~P"){
    val input ="~P"
    val result = evaluate(input)
    val answer = 1::0::Nil
    assert(result._1==answer)
  }
  test("Logic Expression: ~P+Q"){
    val input ="~P+Q"
    val result = evaluate(input)
    val answer = 1::1::0::1::Nil
    assert(result._1==answer)
  }

  test("Logic Expression: (A*B)+C"){
    val input ="(A*B)+C"
    val result = evaluate(input)
    val answer = 0::1::0::1::0::1::1::1::Nil
    assert(result._1==answer)
  }
  test("Logic Expression: (~(A+B)*C)→D"){
    val input ="(~(A+B)*C)→D"
    val result = evaluate(input)
    val answer = 1::1::0::1::1::1::1::1::1::1::1::1::1::1::1::1::Nil
    assert(result._1==answer)
  }
  test("Logic Expression: (~(A+B)*C)↔D"){
    val input ="(~(A+B)*C)↔D".trim
    val result = evaluate(input)
    val answer = 1::0::0::1::1::0::1::0::1::0::1::0::1::0::1::0::Nil
    assert(result._1==answer)
  }


}
