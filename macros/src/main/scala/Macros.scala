import language.experimental.macros
import scala.reflect.macros.Context

import scala.collection.mutable.{ListBuffer, Stack}

import java.util.Date


import playground.ValueProvider

class ParseException(message: String, cause: Exception) extends Exception(message, cause)

object Macros {
  val defaultDateFormat = new java.text.SimpleDateFormat("EEE MMM d HH:mm:ss zzz yyyy")
  
  type ParamsTpe = ValueProvider[_]
  
  // Some helpers to make things a little more simple in the generated code
  def getInt(name:String,in:Any):Int = in match {
    case n:Long => n.asInstanceOf[Int]
    case n:Int => n
    case s:String => s.toInt
    case e => throw new ParseException(s"Error converting item '$name' to Int. Value: $e",null)
  }
  
  def getLong(name:String,in:Any):Long = in match {
    case n:Long => n
    case n:Int => n.asInstanceOf[Long]
    case s:String => s.toLong
    case e => throw new ParseException(s"Error converting item '$name' to Long. Value: $e",null)
  }
  
  def getFloat(name:String,in:Any):Float = in match {
    case n:Float => n
    case n:Double => n.asInstanceOf[Float]
    case n:Long =>   n.asInstanceOf[Float]
    case n:Int =>    n.asInstanceOf[Float]
    case s:String => s.toFloat
    case e => throw new ParseException(s"Error converting item '$name' to Float. Value: $e",null)
  }
  
  def getDouble(name:String,in:Any):Double = in match {
    case n:Double => n
    case n:Float => n.asInstanceOf[Double]
    case n:Long =>   n.asInstanceOf[Double]
    case n:Int =>    n.asInstanceOf[Double]
    case s:String => s.toDouble
    case e => throw new ParseException(s"Error converting item '$name' to Double. Value: $e",null)
  }
  
  def getString(name:String,in:Any):String = in match {
    case s:String => s
    case s => s.toString
  }
  
  def getDate(name:String, in:Any):Date = in match {
    case s:Date => s
    case s:String => defaultDateFormat.parse(s)
    case e => throw new ParseException(s"Error converting item '$name' to Date. Value: $e",null)
  }
  
  def asyncBuilder[U](params:Macros.ParamsTpe,name:String)(f:(U)=>Unit) = macro asyncimpl[U]
  def asyncimpl[U:c.WeakTypeTag](c: Context)(params: c.Expr[ParamsTpe],name:c.Expr[String])
      (f:c.Expr[(U)=>Unit]):c.Expr[Unit] = {
      import c.universe._
      
      c.Expr[Unit](Apply(f.tree,List(classbuilder[U](c)(params,name).tree)))
  }
  // The meat and potatoes of the implementation.
  def classBuilder[U](params: ParamsTpe,name:String) = macro classbuilder[U]
  def classbuilder[U: c.WeakTypeTag](c: Context)(params: c.Expr[ParamsTpe],name:c.Expr[String]):c.Expr[U] = {
    import c.universe._
    import Flag._
    
    def LIT[B](x:B) = c.Expr[B](Literal(Constant(x)))
    def CONCAT(a:c.Expr[String],b:c.Expr[String]) = reify{a.splice+b.splice}
    
    // For building objects that take type parameters
    def typeArgumentTree(t: c.Type): c.Tree = t match {
      case TypeRef(_, _, typeArgs @ _ :: _) => AppliedTypeTree(
            Ident(t.typeSymbol), typeArgs map (t=>typeArgumentTree(t)) )
      case _                                => Ident(t.typeSymbol.name)
    }
    
    // For generating new new params set down the tree one step
    def genFreshParams(name: c.Expr[String], params: c.Expr[ParamsTpe]):(TermName,c.Expr[ParamsTpe],Tree) = {
      val freshNme = newTermName(c.fresh("freshParams$"))
      val freshParams = c.Expr[ParamsTpe](Ident(freshNme))
      
      // For some reason this had to be taken out of the reify, causing stack overflow 
      // on expansion. Maybe a reify in a reify is bad news?
      val freshParamsTree = ValDef(    // Need to use a fresh name to avoid stuff like 'val nme = nme'
                    Modifiers(), 
                    freshNme,
                    TypeTree(typeOf[ParamsTpe]), 
                    reify{params.splice.forPrefix(name.splice)}.tree
                  )
      
      (freshNme,freshParams,freshParamsTree)
    }
    
    def rparseList(tpe:Type,name:c.Expr[String], params: c.Expr[ParamsTpe]):Tree = {
      val TypeRef(_,_,List(argTpe)) = tpe
      
      val (freshNme,freshParams,freshParamsTree) = genFreshParams(name,params)
      
      val listNme = newTermName("lst")
      val listTree = ValDef(
                      Modifiers(MUTABLE),
                      listNme,
                      typeArgumentTree(tpe),
                      reify{Nil}.tree
        )
        
      reify{
        c.Expr(freshParamsTree).splice
        c.Expr(listTree).splice
        var items = freshParams.splice.keyCount-1
        while(items >= 0) {
          val itemnumber = items.toString
          // Manual tree manipulation is fastest...
          c.Expr{Assign(Ident(listNme),Apply(Select(Ident(listNme),
              newTermName("$colon$colon")),
              List(buildObject(argTpe, c.Expr[String](Ident("itemnumber")),freshParams))))
          }.splice
          
          items-=1
        }
        c.Expr(Ident(listNme)).splice
      }.tree
    } // rparseList
     
    def rparseMap(tpe:Type,name:c.Expr[String], params: c.Expr[ParamsTpe])   = {
      val TypeRef(_,_,keyTpe::valTpe::Nil) = tpe 
      val (freshNme,freshParams,freshParamsTree) = genFreshParams(name,params)
      
      // Makes us capable of parsing maps that contain primatives as keys, not only strings
      val kExpr = c.Expr[String](Ident("k"))
      val keyParser = keyTpe match {
        case a if a =:= typeOf[Int] => reify{getInt(kExpr.splice,kExpr.splice)}
        case a if a =:= typeOf[Long] => reify{getLong(kExpr.splice,kExpr.splice)}
        case a if a =:= typeOf[Float] => reify{getFloat(kExpr.splice,kExpr.splice)}
        case a if a =:= typeOf[Double] => reify{getDouble(kExpr.splice,kExpr.splice)}
        case a if a =:= typeOf[String] => reify{kExpr.splice}
        case _ => c.abort(c.enclosingPosition, "Map must contain primative types as keys!")
      }
      
        // Build the tree much like the function toMap does
      val mapBuilderTree = ValDef(Modifiers(), newTermName("b"), TypeTree(),
          TypeApply(reify{scala.collection.mutable.HashMap.empty}.tree,
            List(typeArgumentTree(keyTpe), typeArgumentTree(valTpe))
          )
        )
      // code comlains of unknown type if we try to use c.Expr().splice for some reason
      val addValTree = Apply(Select(Ident("b"),newTermName("update")),
                            List(keyParser.tree,buildObject(valTpe,kExpr,freshParams))
                            )
   
      reify {
      // This is more simple to do using collections functions, but this is more 
      // performant. Check older commits for simpler version (da050fb6d1c317228cbcedeac9db91221d4565da)
        c.Expr(freshParamsTree).splice
        c.Expr(mapBuilderTree).splice // Defines val b = mapBuilder
        freshParams.splice.keySet.foreach { k => // Must use k or rename above
          c.Expr(addValTree).splice
        }
        c.Expr(Select(Ident("b"),newTermName("toMap"))).splice
      }.tree
    }
    
    def rparseDate(iname:c.Expr[String], params: c.Expr[ParamsTpe])   = reify { 
      val name = iname.splice
      //(dtf.splice).parse(getArg(name,params.splice))
      new Date
    }
    
    def rparseInt(name:c.Expr[String], params: c.Expr[ParamsTpe])    = reify {
      getInt(name.splice,params.splice(name.splice))
    }
    
    def rparseLong(name:c.Expr[String], params: c.Expr[ParamsTpe])    = reify {
      getLong(name.splice,params.splice(name.splice))
    }
    
    def rparseFloat(name:c.Expr[String], params: c.Expr[ParamsTpe])    = reify {
      getFloat(name.splice,params.splice(name.splice))
    }
    
    def rparseDouble(name:c.Expr[String], params: c.Expr[ParamsTpe])    = reify {
      getDouble(name.splice,params.splice(name.splice))
    }
    
    def rparseString(name:c.Expr[String], params: c.Expr[ParamsTpe]) = reify{
      getString(name.splice,params.splice(name.splice))
    }
    
    def rparseOption(tpe:Type,name:c.Expr[String], params: c.Expr[ParamsTpe]):Tree = {
      val TypeRef(_,_,List(argTpe)) = tpe
      reify{
        try{  // Expr is of type argTpe
          Some(c.Expr(buildObject(argTpe,name,params)).splice)
        } catch {
          case _: java.util.NoSuchElementException => None
        }
      }.tree
      
    }
    
    def getVars(tpe:Type):List[Symbol] = {
      val ctorParams = tpe.member(nme.CONSTRUCTOR).asMethod.paramss.flatten.map(_.name.toTermName.toString.trim)
      for{
        sym <- tpe.members.filter(_.asTerm.isVar).toList
        if !ctorParams.exists{sym.name.toTermName.toString.trim == _}
      } yield sym
    }
    
    // The really heavyweight function. Most of the magic happens in the last else statement
    def buildObject(tpe: Type, name:c.Expr[String],params: c.Expr[ParamsTpe]):Tree = {
      // simple types
      if      (tpe =:= typeOf[Int])    { rparseInt(name,params).tree    }
      else if (tpe =:= typeOf[Long])   { rparseLong(name,params).tree   }
      else if (tpe =:= typeOf[Float])  { rparseFloat(name,params).tree  }
      else if (tpe =:= typeOf[Double]) { rparseDouble(name,params).tree }
      else if (tpe =:= typeOf[String]) { rparseString(name,params).tree }
      else if (tpe =:= typeOf[Date])   { rparseDate(name,params).tree   }
      // The privlaged types
      else if (tpe.erasure =:= typeOf[Option[Any]]) {
        rparseOption(tpe,name,params)
      }
      else if (tpe.erasure =:= typeOf[Map[_,_]]) {
        rparseMap(tpe,name,params)
      }
      else if (tpe.erasure =:= typeOf[List[Any]]) {
        rparseList(tpe,name,params)
        
      } else { // Must be a complex object. Hopefully it can be instanced normally
      
        val TypeRef(_,sym:Symbol,tpeArgs:List[Type]) = tpe
        val ctorParams = tpe.member(nme.CONSTRUCTOR).asMethod.paramss
        val (newParamsTerm,newParamsExpr,newProviderTree) = genFreshParams(name,params)
        
        val newObjTerm = newTermName(c.fresh("newObj$"))
        val newObjTypeTree = typeArgumentTree(tpe)
        val newObjTree = ValDef(Modifiers(),newObjTerm,newObjTypeTree,
          New(newObjTypeTree,ctorParams.map{_.zipWithIndex.map { 
            case (pSym,index) =>
            // Change out the types if it has type parameters
            val pTpe = pSym.typeSignature.substituteTypes(sym.asClass.typeParams,tpeArgs)
            //val compName = CONCAT(CONCAT(name,LIT(".")),LIT(pSym.name.decoded))
            val compName = LIT(pSym.name.decoded)
            // If param has defaults, try to find the val in map, or call 
            // default evaluation from its companion object
            if (pSym.asTerm.isParamWithDefault) {
              reify {
                try {
                  c.Expr(buildObject(pTpe, compName,newParamsExpr)).splice // splice in another obj tree
                } catch {
                  case e: java.util.NoSuchElementException =>
                    // Need to use the origional symbol.companionObj to get defaults
                    // Would be better to find the generated TermNames if possible
                    c.Expr(Select(Ident(sym.companionSymbol),newTermName(
                      "$lessinit$greater$default$" + (index+1).toString))
                    ).splice
                }
              }.tree
            } else buildObject(pTpe, compName,newParamsExpr) // Required
            
          }})    // Using the New(Tree,List(List(Tree))) constructor
        ) // newObjTree ValDef
        
        // Here we generate the code for setting fields not in the constructor
        val vars = getVars(tpe)
        val setParamsBlocks = vars map { vari =>
          val varName = vari.name.toTermName.toString.trim
          reify{
            try {
            c.Expr(Assign(Select(Ident(newObjTerm),newTermName(varName)),
              buildObject(vari.typeSignature,
                LIT(varName),
                newParamsExpr
              )
            )).splice
            } catch { // Don't care if they fail
              case _ : java.util.NoSuchElementException =>
            }
          }.tree
        }
        
        Block(newProviderTree::newObjTree::setParamsBlocks,Ident(newObjTerm))
      }
    }
    
    val tpe = weakTypeOf[U]
    c.Expr[U](buildObject(tpe,name,params))
  }
}
