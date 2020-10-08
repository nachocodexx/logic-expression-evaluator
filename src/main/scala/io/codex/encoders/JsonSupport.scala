package io.codex.encoders
object JsonSupport {
 case class TruthColumn(title:String,values:List[Int])
 case class LogicExpressionResponse(expression:String,
                                    data:List[TruthColumn],
                                    isTautology:String,
                                    isContradiction:String,
                                    isContingency:String, isSatisfiable:String)
}
