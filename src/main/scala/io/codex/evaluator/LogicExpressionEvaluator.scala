package io.codex.evaluator

import io.codex.calculator.PostfixCalculator.evalInput
import io.codex.converter.PostfixConverter.{infixToPostfix, isVariable}
import io.codex.encoders.JsonSupport
import io.codex.utils.Utils.{booleanToString, filteredBinaryNumbers, generateBinaryNumbers, processMixedInput, substituteValuesInExpression, toTruthColumns, zipVariableWithValues}
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
  def getIndependentVariables(xs:List[Char]): List[Char] = xs.filter(!isVariable(_)).filter(_.isDigit).map {
    x=> if(x=='1') 'T'else 'F'
  }.sorted.reverse
  def getVariableLength(xs:List[Char]): Int =xs.length
  def getTotalRows(n:Int): Int = pow(2,n).toInt

  def hasTrueAndFalse(xs:List[Int]): Boolean = xs.contains(1) && xs.contains(0)
  def hasOnlyNumbers(xs:List[Char]): Boolean = getVariables(xs, isVariable).count(!_.isDigit) ==0
  def isMixed(xs:List[Char]): Boolean = getVariables(xs, !isVariable(_)).exists(_.isDigit)
  def onlyFalse(xs:List[Char]): Boolean =xs.head == 'F'
  def onlyTrue(xs:List[Char]): Boolean =xs.head == 'T'

  def isTautology(xs:List[Int]): Boolean = !xs.exists(_ != 1)
  def isTautologyStr: List[Int] => String = isTautology _ andThen booleanToString
  def isContradiction(xs:List[Int]): Boolean = !xs.contains(1)
  def isContradictionStr: List[Int] => String = isContradiction _ andThen booleanToString
  def isContingency(xs:List[Int]): Boolean = !isTautology(xs) && !isContradiction(xs)
  def isContingencyStr: List[Int] => String = isContingency _ andThen booleanToString
  def isSatisfiable(xs:List[Int]):Boolean = xs.contains(1)
  def isSatisfiableStr: List[Int] => String = isSatisfiable _ andThen booleanToString

//  def on
  def evaluateBinaryExpression(input:String): Int = {
    val testExpression = parseInput(input)
    val postfixExpression = infixToPostfix(testExpression,'('::Nil)
    val response = evalInput(postfixExpression).runA(Nil).value
    response
  }

  def evaluateMixedExpression(input:String): (List[Int], List[JsonSupport.TruthColumn]) = {
    val  processedInput = processMixedInput(input)

    val testExpression = parseInput(input)
    val processedTestExpression = parseInput(processedInput)

    val variables= getVariables(processedTestExpression,isVariable)

    val onlyLetterVariables = getVariables(testExpression,isVariable)

    val logicalVariablesLength=variables.length
    val bits = variables.filter(x=>x=='F' || x=='T').map{
      x=> if(x== 'T') 1 else 0
    }

    val unfilteredBinaryNumbers = generateBinaryNumbers(logicalVariablesLength)
//    val unfilteredBinaryNumbers = generateBinaryNumbers(logicalVariablesLength)
    val binaryNumbers = filteredBinaryNumbers(bits,unfilteredBinaryNumbers )
//    val binaryNumbers = filteredBinaryNumbers(bits,unfilteredBinaryNumbers )
//    val postfixExpression = infixToPostfix(testExpression,'('::Nil)
    val postfixExpression = infixToPostfix(processedTestExpression,'('::Nil)

//
//    val getIndependentVariablesResult=getIndependentVariables(processedTestExpression)
    val getIndependentVariablesResult=getIndependentVariables(testExpression).distinct
//
    val allPossibleExpressions = substituteValuesInExpression(postfixExpression,logicalVariablesLength,variables, binaryNumbers)

    val response = allPossibleExpressions.map(evalInput).map(_.runA(Nil).value)

    val columns = zipVariableWithValues(onlyLetterVariables:++getIndependentVariablesResult,binaryNumbers) :+ (input,response)

    println(columns)

    val data= toTruthColumns(columns)

    (response,data)
  }

  def evaluate(input:String): (List[Int], List[JsonSupport.TruthColumn]) ={
    // Parse the logical statement to List[Char]
    val testExpression = parseInput(input)
    //  Getting all the variables from test expression
    val logicalVariables = getVariables(testExpression,isVariable)
    //  Getting total of the variables
    val logicalVariablesLength = getVariableLength(logicalVariables)
    //
    val binaryNumbers = generateBinaryNumbers(logicalVariablesLength)
    //  Infix to postfix using Shunting-yard algorithm
    val postfixExpression = infixToPostfix(testExpression,'('::Nil)
    // Substitute the binary numbers in the postfix expression
    val allPossibleExpressions = substituteValuesInExpression(postfixExpression,logicalVariablesLength,logicalVariables,binaryNumbers)
    // Evaluate the postfix expression
    val response = allPossibleExpressions.map(evalInput).map(_.runA(Nil).value)

    //
    val columns = zipVariableWithValues(logicalVariables,binaryNumbers) :+ (input,response)
    val data= toTruthColumns(columns)
    (response,data)

  }

}

