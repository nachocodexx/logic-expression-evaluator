package io.codex.encoders

import io.codex.sets.Setx

import scala.collection.mutable.ArrayBuffer

object JsonSupport {
  case class RelationsResponse(data:List[List[(String,String)]])
  case class FunctionItemResponse(orderPairs:List[(String,String)],
                                  isRelation:String,
                                  isFunction:String,
                                  isInjective:String,
                                  isSurjective:String,
                                  isBijective:String)

  case class FunctionResponse(data:List[FunctionItemResponse])

 case class TruthColumn(title:String,values:List[Int])
 case class LogicExpressionResponse(expression:String,
                                    data:List[TruthColumn],
                                    isTautology:String,
                                    isContradiction:String,
                                    isContingency:String, isSatisfiable:String,expressionType:String)

 case class OnlyNumbersResponse(expression:String,result:Int,expressionType:String)
// SETS
  case class Members(A:List[String], B:List[String])
  case class Cardinalities(A:Int,B:Int)
  case class PowerSets(A:List[String],B:List[String])
  case class Subsets(AB:String,BA:String)
  case class ProperSubsets(AB:String,BA:String)
  case class Difference(AB:List[String],BA:List[String])
  case class SymmetricDifference(AB:List[String],BA:List[String])
  case class Intersection(AB:List[String],areDisjoint:String)
  case class CartersianProduct(AB:List[String], BA:List[String])

 case class SetsOperationsResponse(
                                    members:Members,
                                    cardinalities: Cardinalities,
                                    powerSets: PowerSets,
                                    comparison:String,
                                    subsets: Subsets,
                                    properSubsets: ProperSubsets,
                                    difference: Difference,
                                    symmetricDifference: SymmetricDifference,
                                    union:List[String],
                                    intersection: Intersection,
                                    cartersianProduct: CartersianProduct
                                  )
}
