import org.scalatest.funsuite.AnyFunSuite
import io.codex.evaluator.LogicExpressionEvaluator._
class PostfixTest extends  AnyFunSuite {

  test("the logic expression: (p+q)*~(p*q)"){
    val result = evaluate("(p+q)*~(p*q)")
    assert( result._1 == List(0,1,1,0))
  }
  test("the logic expression: (p+q)"){
    val result = evaluate("(p+q)")
    assert( result._1== List(0,1,1,1))
  }


}
