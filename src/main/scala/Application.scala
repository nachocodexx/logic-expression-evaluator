import scala.math.pow
import  scala.collection.immutable.List
import io.codex.converter.PostfixConverter._
import io.codex.utils.Utils._
import io.codex.calculator.PostfixCalculator._


object LogicalExpression{
  def getExpressionList(value:String): List[Char] = value.toList
  def appendCloseParenthesis(value:List[Char]): List[Char] = value :+ ')'
  def parseInput(value:String): List[Char] = (getExpressionList _ andThen appendCloseParenthesis)(value)
  def getVariables(xs:List[Char],f:Char=>Boolean): List[Char] = xs.filter(f).distinct.sorted
  def getVariableLength(xs:List[Char]): Int =xs.length
  def getTotalRows(n:Int): Int = pow(2,n).toInt
}

import LogicalExpression._
object Application extends App{
// Logical statement
//  val input = "((~B+A)*C)+D"
  val input = "(A+~A)"
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
//  Infix to postfix using Shunting-yard algorithm
  val postfixExpression = infixToPostfix(testExpression,'('::Nil)
// Substitute the binary numbers in the postfix expression
  val allPossibleExpressions = substituteValuesInExpression(postfixExpression,logicalVariablesLength,logicalVariables,binaryNumbers)
// Evaluate the postfix expression
  val response = allPossibleExpressions.map(evalInput).map(_.runA(Nil).value)
  println(response)
}
