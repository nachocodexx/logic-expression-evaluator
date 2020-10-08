package io.codex.evaluator

import io.codex.calculator.PostfixCalculator.evalInput
import io.codex.converter.PostfixConverter.{infixToPostfix, isVariable}
import io.codex.encoders.JsonSupport
import io.codex.utils.Utils.{booleanToString, pad, substituteValuesInExpression, toBinary, toTruthColumns, zipVariableWithValues}
import org.slf4j.Logger
import org.slf4j.LoggerFactory.getLogger

import scala.collection.immutable.List
import scala.math.pow

class LogicExpressionEvaluator
object LogicExpressionEvaluator {
  val logger: Logger = getLogger(classOf[LogicExpressionEvaluator])
  def getExpressionList(value:String): List[Char] = value.toList
  def appendCloseParenthesis(value:List[Char]): List[Char] = value :+ ')'
  def parseInput(value:String): List[Char] = (getExpressionList _ andThen appendCloseParenthesis)(value)
  def getVariables(xs:List[Char],f:Char=>Boolean): List[Char] = xs.filter(f).distinct.sorted
  def getVariableLength(xs:List[Char]): Int =xs.length
  def getTotalRows(n:Int): Int = pow(2,n).toInt

  def isTautology(xs:List[Int]): Boolean = !xs.exists(_ != 1)
  def isTautologyStr: List[Int] => String = isTautology _ andThen booleanToString
  def isContradiction(xs:List[Int]): Boolean = !xs.contains(1)
  def isContradictionStr: List[Int] => String = isContradiction _ andThen booleanToString
  def isContingency(xs:List[Int]): Boolean = !isTautology(xs) && !isContradiction(xs)
  def isContingencyStr: List[Int] => String = isContingency _ andThen booleanToString
  def isSatisfiable(xs:List[Int]):Boolean = xs.contains(1)
  def isSatisfiableStr: List[Int] => String = isSatisfiable _ andThen booleanToString

//  def on

  def evaluate(input:String): (List[Int], List[JsonSupport.TruthColumn]) ={
    // Parse the logical statement to List[Char]
    val testExpression = parseInput(input)
    //  Getting all the variables from test expression
    val logicalVariables = getVariables(testExpression,isVariable)
    //  Getting total of the variables
    val logicalVariablesLength = getVariableLength(logicalVariables)
    //  Total of rows using this formula (2^n -1)
    val totalOfRows  = getTotalRows(logicalVariablesLength)
    // Range from 0 to (2^n - 1)
    val maximum = 0 until totalOfRows
    // Getting the binary numbers from 0 to (2^n - 1)
    val binaryNumbers  = maximum.toList.map(toBinary(_)).map(pad(_,logicalVariablesLength))

//    println(test)
    //  Infix to postfix using Shunting-yard algorithm
    val postfixExpression = infixToPostfix(testExpression,'('::Nil)
    // Substitute the binary numbers in the postfix expression
    val allPossibleExpressions = substituteValuesInExpression(postfixExpression,logicalVariablesLength,logicalVariables,binaryNumbers)
    // Evaluate the postfix expression
    val response = allPossibleExpressions.map(evalInput).map(_.runA(Nil).value)

    //
    val columns = zipVariableWithValues(logicalVariables,binaryNumbers) :+ (input,response)
    val data= toTruthColumns(columns)

//    response
//    val isTautologyStr= (isTautology _ an
//    val isContradictionStr= (isContradiction _ andThen booleanToString)(response)
//    val isContingencyStr= (isContingency _ andThen booleanToString)(response)
//    val isSatisfiableStr= (isSatisfiable _ andThen booleanToString)(response)

//    logger.info(s"Variables: ${logicalVariables.mkString(",")}")
//    println(s"Infix expression: $input")
//    println(s"Postfix expression: $postfixExpression")
//    println(s"All possible expressions: ${allPossibleExpressions.mkString(", ")}")
//    println(s"Is it a tautology: ${isTautologyStr(response)}")
//    println(s"Is it a contradiction: $isContradictionStr")
//    println(s"Is it a contingency: $isContingencyStr")
//    println(s"Is it a satisfiableStr: $isSatisfiableStr")
//    println(s"Data: $data")

//    println(s"Result: ${response.mkString(", ")}")
    (response,data)


//    LogicExpressionResponse(input,data,isTautology = isTautologyStr(response),isContradiction=isContradictionStr,
//      isContingency
//      = isContingencyStr, isSatisfiable = isSatisfiableStr)

  }

}

