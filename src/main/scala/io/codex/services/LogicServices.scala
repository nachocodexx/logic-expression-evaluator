package io.codex.services
import org.http4s._
import org.http4s.dsl.io._
import org.http4s.circe._
import io.circe.syntax._
import io.circe.generic.auto._
import cats.effect._
import io.codex.encoders.JsonSupport.{LogicExpressionResponse, OnlyNumbersResponse}
import io.codex.evaluator.LogicExpressionEvaluator.{evaluate, evaluateBinaryExpression, evaluateMixedExpression, hasOnlyNumbers, isContingencyStr, isContradictionStr, isMixed, isSatisfiableStr, isTautologyStr}

object LogicServices {
  def apply(): HttpRoutes[IO] =HttpRoutes.of {
    case GET -> Root / logicStatement =>
      val hasOnlyNumberResult = hasOnlyNumbers(logicStatement.toList)
      val isMixedResult = isMixed(logicStatement.toList)

      if (hasOnlyNumberResult) {
        val result = evaluateBinaryExpression(logicStatement)
        val response = OnlyNumbersResponse(logicStatement, result, "only-numbers")
        Ok(response.asJson)
      }
      else if (isMixedResult) {
        val (result, data) = evaluateMixedExpression(logicStatement)
        val response = LogicExpressionResponse(logicStatement, data, isTautologyStr(result), isContradictionStr
        (result), isContingencyStr(result), isSatisfiableStr(result), "mixed").asJson
        Ok(response)
      }
      else {
        val (result, data) = evaluate(logicStatement)
        val response = LogicExpressionResponse(logicStatement, data, isTautologyStr(result), isContradictionStr(result),
          isContingencyStr(result), isSatisfiableStr(result), "normal").asJson
        Ok(response)
      }
  }
}
