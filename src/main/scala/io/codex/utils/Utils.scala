package io.codex.utils
import cats.effect._
import io.codex.encoders.JsonSupport.TruthColumn
import io.codex.evaluator.LogicExpressionEvaluator.{getTotalRows, onlyTrue}

import scala.annotation.tailrec
import scala.collection.immutable.List

object Utils {

  def removeNonAlphabeticCharacters(x:String): String = x.replaceAll("""[^A-Za-z]"""," ")
  def extractWordsFromString(x:String): IO[List[String]] = IO(removeNonAlphabeticCharacters(x).split(" ").toList.map(_
    .toUpperCase).map(_.trim).filter(_.length>0))

  def nCr(n:Int,r:Int): Int = (factorial(n))/(factorial(r)*factorial(n-r))
  def factorial(n:Int):Int  ={
    if(n==0 || n==1) 1
    else n*factorial(n-1)
  }

  def characterWithIndexes(xs:List[Char],fn:Char=>Boolean): List[Int] =
    xs
      .zipWithIndex
      .filter(x=>fn(x._1))
      .map(_._2)


  def toTruthColumns(xss:List[(String,List[Int])]): List[TruthColumn] = xss.map {
    case (str, value) => TruthColumn(str,value)
  }
  def zipVariableWithValues(variables:List[Char],values:List[List[Int]]): List[(String, List[Int])] = variables.zipWithIndex.map {
    case (c, i) => (c.toString,values.map(x=>x(i)))
  }
  def booleanToString(x:Boolean): String = if(x) "Yes" else "No"

  def generateBinaryNumbers(n:Int): List[List[Int]] ={
    //  Total of rows using this formula (2^n -1)
    val totalOfRows  = getTotalRows(n)
    // Range from 0 to (2^n - 1)
    val maximum = 0 until totalOfRows
    // Getting the binary numbers from 0 to (2^n - 1)
    val binaryNumbers  = maximum.toList.map(toBinary(_)).map(pad(_,n))
    binaryNumbers
  }
  def filteredBinaryNumbers(bits:List[Char], binaryNumbers:List[List[Int]]): List[List[Int]] = {
    if(bits.length>1) binaryNumbers.filter(filterMixed)
    else
      if(onlyTrue(bits)) binaryNumbers.filter(filterLastTrue)
      else binaryNumbers.filter(filterLastFalse)
  }
 def filterLastFalse: List[Int]=>Boolean = {
   case init :+x => x==0
 }
  def filterLastTrue:List[Int]=>Boolean ={
    case init:+x=>x==1
  }
  def filterMixed:List[Int]=>Boolean = {
    case init:+x:+y=>x==1 && y==0
  }

  def replaceNumberOneForTrue(input:String): String = input.replaceAll("1","T")
  def replaceNumberZeroForFalse(input:String): String = input.replaceAll("0","F")
  def processMixedInput(input:String): String = (replaceNumberOneForTrue _ andThen replaceNumberZeroForFalse)(input)

  def pad(xs:List[Int], size:Int): List[Int] = {
    val xLen= xs.length
    if(xLen == size)  xs
    else  {
      val diff = size - xLen
      val padZeros:List[Int] = List.fill(diff)(0)
      padZeros.concat(xs)
    }

  }

  @tailrec
  def toBinary(n:Int, bin: List[Int] = List.empty[Int]):List[Int] = {
    if(n ==1 ) 1::Nil
    else if (n== 0) 0::Nil
    else if(n/2 == 1) 1:: (n % 2) :: bin
    else {
      val remainder = n % 2
      val quotient = n / 2
      toBinary(quotient, remainder::bin)
    }
  }


//  @tailrec
  def substituteValuesInExpression(postfixExpression:String,
                                   logicalVariablesLength:Int, logicalVariables:List[Char],
                                   allValues:List[List[Int]]
                               ):List[String] = {
    @tailrec
    def _replaceInExpression(n:Int, values:List[Int], str:String):String ={
      if(n==0) str.replaceAll(logicalVariables.head.toString,values.head.toString)
      else {
        val value = values(n - 1)
        val character = logicalVariables(n - 1)
        _replaceInExpression(n - 1, values, str.replaceAll(character.toString, value.toString))
      }
    }
    allValues.map(_replaceInExpression(logicalVariablesLength,_,postfixExpression))
  }
}


