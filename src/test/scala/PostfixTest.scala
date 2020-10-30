import org.scalatest.funsuite.AnyFunSuite
import io.codex.evaluator.LogicExpressionEvaluator._
import io.codex.converter.PostfixConverter._
import io.codex.utils.Utils._
class PostfixTest extends  AnyFunSuite {
  test("Process mixed expression"){
    val input = "(A→1)+0"
    val processedExpression = processMixedInput(input)
    println(processedExpression)
    assert(processedExpression=="(A→T)+F")
  }
  test("Logic expression that contains only binary digits"){
    val input ="(1+0)*0"
    val result = evaluateBinaryExpression(input)
    assert(result ==0)
  }

  test("Mixed logic expression"){
    val input = "(Z*0)+1"
    val response = evaluateMixedExpression(input)._1
    assert(response  == 1::1::Nil)
  }

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
  test("Mixed expression: (A+1)+1+1"){
    val input = "(0→A)"
    val result = evaluateMixedExpression(input)
    assert(result._1 == 1::1::Nil)
  }


}
