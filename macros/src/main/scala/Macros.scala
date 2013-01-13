import language.experimental.macros
import scala.reflect.macros.Context

import java.util.Date

import playground.ValueProvider

class ParseException(message: String, cause: Exception) extends Exception(message, cause)

object Macros {
  import PrimativeHelpers._
  
  type ParamsTpe = ValueProvider[_]
  
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
    
    // Some helper functions moved to declutter
    val helpers = new MacroHelpers[c.type](c)
    import helpers._
    
    // For generating new new params set down the tree one step
    def genFreshParams(name: c.Expr[String], params: c.Expr[ParamsTpe]):(TermName,c.Expr[ParamsTpe],Tree) = {
      val freshNme = newTermName(c.fresh("freshParams$"))
      val freshParams = c.Expr[ParamsTpe](Ident(freshNme))
      
      val freshParamsTree = ValDef( // Need to use a fresh name to avoid stuff like 'val nme = nme'
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
          // Manual tree manipulation is fastest, but more subject to scala churn
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
      
      // Capable of parsing maps that contain primatives as keys, not only strings
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
        // performant. Check older commits for simpler version 
        // Jan 9, 2013 (da050fb6d1c317228cbcedeac9db91221d4565da)
        c.Expr(freshParamsTree).splice
        c.Expr(mapBuilderTree).splice // Defines val b = mapBuilder
        freshParams.splice.keySet.foreach { k => // Must use k or rename above
          c.Expr(addValTree).splice
        }
        c.Expr(Select(Ident("b"),newTermName("toMap"))).splice
      }.tree
    }
    
    def rparseDate(iname:c.Expr[String], params: c.Expr[ParamsTpe])  = reify { 
      val name = iname.splice
      //(dtf.splice).parse(getArg(name,params.splice))
      new Date
    }
    
    def rparseInt(name:c.Expr[String], params: c.Expr[ParamsTpe])    = reify {
      getInt(name.splice,params.splice(name.splice))
    }
    
    def rparseLong(name:c.Expr[String], params: c.Expr[ParamsTpe])   = reify {
      getLong(name.splice,params.splice(name.splice))
    }
    
    def rparseFloat(name:c.Expr[String], params: c.Expr[ParamsTpe])  = reify {
      getFloat(name.splice,params.splice(name.splice))
    }
    
    def rparseDouble(name:c.Expr[String], params: c.Expr[ParamsTpe]) = reify {
      getDouble(name.splice,params.splice(name.splice))
    }
    
    def rparseString(name:c.Expr[String], params: c.Expr[ParamsTpe]) = reify{
      getString(name.splice,params.splice(name.splice))
    }
    
    def rparseOption(tpe:Type,name:c.Expr[String], params: c.Expr[ParamsTpe]):Tree = {
      val TypeRef(_,_,List(argTpe)) = tpe
      reify{
        try{
          Some(c.Expr(buildObject(argTpe,name,params)).splice)
        } catch {
          case _: java.util.NoSuchElementException => None
        }
      }.tree
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
        val (_,freshParams,freshParamsTree) = genFreshParams(name,params)
        
        val newObjTerm = newTermName(c.fresh("newObj$"))
        val newObjTypeTree = typeArgumentTree(tpe)
        val newObjTree = ValDef(Modifiers(),newObjTerm,newObjTypeTree,
          // New(typeTree,List(List(params))
          New(newObjTypeTree,ctorParams.map{_.zipWithIndex.map { 
            case (pSym,index) =>
            // Change out the types if it has type parameters
            val pTpe = pSym.typeSignature.substituteTypes(sym.asClass.typeParams,tpeArgs)
            val compName = LIT(pSym.name.decoded)
            // If param has defaults, try to find the val in map, or call 
            // default evaluation from its companion object
            if (pSym.asTerm.isParamWithDefault) {
              reify {
                try {
                  c.Expr(buildObject(pTpe, compName,freshParams)).splice // splice in another obj tree
                } catch {
                  case e: java.util.NoSuchElementException =>
                    // Need to use the origional symbol.companionObj to get defaults
                    // Would be better to find the generated TermNames if possible
                    c.Expr(Select(Ident(sym.companionSymbol),newTermName(
                      "$lessinit$greater$default$" + (index+1).toString))
                    ).splice
                }
              }.tree
            } else buildObject(pTpe, compName,freshParams) // Required
            
          }}) // Using the New(Tree,List(List(Tree))) constructor
        ) // newObjTree ValDef
        
        // Generate the code for setting fields not in the constructor
        val vars = getNonConstructorVars(tpe)
        val setParamsBlocks = vars map { vari =>
          val varName = vari.name.toTermName.toString.trim
          reify{
            try {
            c.Expr(Assign(Select(Ident(newObjTerm),newTermName(varName)),
              buildObject(vari.typeSignature,
                LIT(varName),
                freshParams
              )
            )).splice
            } catch { // Don't care if they fail
              case _ : java.util.NoSuchElementException =>
            }
          }.tree
        }
        
        Block(freshParamsTree::newObjTree::setParamsBlocks,Ident(newObjTerm))
      }
    }
    
    val tpe = weakTypeOf[U]
    c.Expr[U](buildObject(tpe,name,params))
  }
}
