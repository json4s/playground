import scala.reflect.macros.Context


class MacroHelpers[C <: Context](val c1:C) {
  
  import c1.universe._
  
  def LIT[B](x:B) = c1.Expr[B](Literal(Constant(x)))
  def CONCAT(a:c1.Expr[String],b:c1.Expr[String]) = reify{a.splice+b.splice}
  
  // For building objects that take type parameters
  def typeArgumentTree(t: c1.Type): c1.Tree = t match {
    case TypeRef(_, _, typeArgs @ _ :: _) => AppliedTypeTree(
          Ident(t.typeSymbol), typeArgs map (t=>typeArgumentTree(t)) )
    case _                                => Ident(t.typeSymbol.name)
  }
    
  def getVars(tpe:Type):List[Symbol] = tpe.members.filter(_.asTerm.isVar).toList
  def getVals(tpe:Type):List[Symbol] = tpe.members.filter(_.asTerm.isVal).toList
  
  def getNonConstructorVars(tpe:Type):List[Symbol] = {
    // Make sure that the param isn't part of the constructor
    val ctorParams = tpe.member(nme.CONSTRUCTOR).asMethod.paramss.flatten.map(_.name.toTermName.toString.trim)
    for{
      sym <- getVars(tpe)
      if !ctorParams.exists{sym.name.toTermName.toString.trim == _}
    } yield sym
  }
  
  
  
}
