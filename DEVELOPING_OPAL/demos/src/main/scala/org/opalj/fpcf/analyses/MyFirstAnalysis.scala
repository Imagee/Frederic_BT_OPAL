


import org.opalj.br.analyses.{BasicReport, Project, ProjectAnalysisApplication}
import org.opalj.collection.immutable.IntTrieSet
import org.opalj.tac.{AITACode, Assignment, DUVar, Expr, ExprStmt, GetField, GetStatic, LazyDetachedTACAIKey, New, PutField, PutStatic, StaticFunctionCall, StringConst, TACMethodParameter, UVar, VirtualFunctionCall, VirtualMethodCall}
import org.opalj.value.ValueInformation

import java.net.URL

object MyFirstAnalysis extends ProjectAnalysisApplication {

  type TACTYPE = AITACode[TACMethodParameter,ValueInformation]
  var _tac:Option[TACTYPE] = None
  var _initTAC:Option[TACTYPE] = None
  var _clinitTAC:Option[TACTYPE] = None
  var _tainted: Set[Int] = Set()

    override def doAnalyze(project: Project[URL], parameters: Seq[String], isInterrupted: () => Boolean): BasicReport = {

      val tacProvider = project.get(LazyDetachedTACAIKey)
      //var allStringset : Set[String] = Set()
      //var certainStringset : Set[String] = Set()
      val methodName = "main" //"<clinit>"
      // val usedMethods = Seq("insertStatement","selectStatement", "execSelect", "execInsert")


      for {
        cf <- project.allProjectClassFiles
        m <- cf.methods
        if m.body.isDefined
        if m.name == "<init>" || m.name == methodName ||m.name == "<clinit>"
      } {

        //val tac = tacProvider(m)
        _tac = Some(tacProvider(m))

        val initMethod = cf.methods.find(m => m.name == "<init>").getOrElse(m)

        //val initTAC = tacProvider(initMethod)
        _initTAC = Some(tacProvider(initMethod))

        val clinitMethod = cf.methods.find(m => m.name =="<clinit>").getOrElse(initMethod)

        //val clinitTAC = tacProvider(clinitMethod)
        _clinitTAC = Some(tacProvider(clinitMethod))

        taintAnalyze(_tac.get,Seq("source"),Seq("executeUpdate","executeQuery"),Seq("sink"))

        //allStringset ++= getAllStrings(_tac.get)
        //certainStringset ++= getAllStringsUsedByCertainMethods(_tac.get,initTAC,clinitTAC,usedMethods)

        //println(m.toJava(ToTxt(tac).mkString("\n", "\n", "\n"))+"\n\n")
      }

      /*
      Ziel 1: alle String ermitteln
      Ziel 1.1: Strings mit concat ermitteln
      Ziel 2: alle bis einem gewissen Punkt ermitteln
      Ziel 3: Nur bestimmte String ermitteln
      Ziel 4: Wert von Variablen an bestimmten Punkten ermittel
      Ziel 5: if cases Handlen.
       */

      BasicReport(""
        /*
        "\nResult of MyFirstAnalsis: \n" + allStringset.mkString(" all found Strings: \n","\n","\n \n")
          + certainStringset.mkString(" Strings used by" + usedMethods.mkString(" ",", ",":") +" \n","\n","\n")

         */
        )
    }



  def getAllStrings(tac: TACTYPE): Set[String] ={
    var set: Set[String] = Set()

    tac.stmts.foreach(stmt => {
      stmt match {
        case Assignment(pc, targetVar, StringConst(spc,value))  =>
          set += value

          /*
        case PutField( pc,declaringClass,name,declaredFieldType,objRef,UVar(value,defSites)) =>
          val i = defSites.iterator.next()
          val expr = tac.stmts(i).asAssignment.expr
          if(expr.isStringConst) println("")
           */

        case _ =>
      }
    })
     set
  }


  def getAllStringsUsedByCertainMethods(tac: TACTYPE,init: TACTYPE,clinit: TACTYPE, methods: Seq[String]): Set[String] ={
    var set : Set[String] = Set()

    tac.stmts.foreach(stmt => {
      stmt match {

        case Assignment(pc,targetVar,VirtualFunctionCall(vpc,declaringClass,isInterface,name,descriptor,receiver,params)) =>
          set++=searchValuebyFunctionName(methods,name, params, tac, init,clinit)

        case Assignment(pc,targetVar,GetStatic(gpc,declaringClass,name,declaredFieldType)) =>
          //searchValuebyFunctionName(methods,name,params,tac, clinit)
          //searchStaticFiledValue(methods, name, tac, clinit)



          //Für String concatenation
        case Assignment(pc,targetVar,
        sfc@ StaticFunctionCall( spc, declaringClass,isInterface, name, descriptor, params )) =>

        case ExprStmt(pc,VirtualFunctionCall(vpc,declaringClass,isInterface,name,descriptor,receiver,params))=>
          set++= searchValuebyFunctionName(methods, name, params, tac, init,clinit)

        case VirtualMethodCall(pc, declaringClass, isInterface, name, descriptor, receiver, params) =>
          set ++= searchValuebyFunctionName(methods, name, params, tac, init,clinit)

          /*
        case VirtualFunctionCall(pc,declaringClass,isInterface,name,descriptor,receiver,params) =>
          set++=searchValuebyFunctionName(methods,name, params, tac, init)
           */

        case _ =>
          }
    })
          return set
  }

  def searchStaticFiledValue(methods: Seq[String], name:String, tac: TACTYPE, init: TACTYPE):Unit = {

  }


  def searchValuebyFunctionName(methods: Seq[String], name:String, params: Seq[Expr[DUVar[ValueInformation]]],
                                tac: TACTYPE, init: TACTYPE, clinit: TACTYPE):Set[String] = {
    var set : Set[String] = Set()
    if(methods.contains(name)) {
      params.foreach(p => {
        p match {
          case UVar(value, defSites) =>
            set ++= searchValue(defSites,tac)
          case _ =>
        }
      })
    }
    set
  }

  def searchValue(defSites:IntTrieSet, tac: TACTYPE):Set[String] = {
    val defSiteIterator = defSites.iterator
    var set : Set[String] = Set()

    while(defSiteIterator.hasNext){
      val i = defSiteIterator.next()
      if(i >= 0){
          tac.stmts(i).asAssignment.expr match {
          case StringConst(pc,value) => set += value
          case GetField(pc, declaringClass, name, declaredFieldType, objRef) =>
            set++=searchFieldValue(name,_tac.get, _initTAC.get,_clinitTAC.get)
            set++=searchFieldValue(name,_initTAC.get,_initTAC.get,_clinitTAC.get)

          case GetStatic(pc,declaringClass,name,declaredFieldType) =>
            set++=searchFieldValue(name,_tac.get, _initTAC.get,_clinitTAC.get)
            set++=searchFieldValue(name,_clinitTAC.get,_initTAC.get,_clinitTAC.get)
          case _=>
        }
      }else{
        println("lad in Else")
      }
    }
    set
  }

  def searchFieldValue(FieldName: String, tac: TACTYPE, init: TACTYPE, clinit: TACTYPE):Set[String] =  {
    var set : Set[String] = Set()

    tac.stmts.foreach(stmt => {
      stmt match {
        case PutField( pc,declaringClass,name,declaredFieldType,objRef,v@ UVar(value,defSites)) if (name == FieldName) =>
            set ++= searchValue(defSites, tac)

        case PutStatic(pc,declaringClass,name,declaredFieldType,value) if (name == FieldName )=>
            set++= searchValue(value.asVar.definedBy,tac)

        case _=>
      }
    })

    return set
  }
  // Plan B Part:

  def searchTheCurrentValue() = {
    ""
  }

  def isValueTainted()={
    ""
  }

  def taintAnalyze(tac: TACTYPE, sourceNames:Seq[String], toInspectMethods:Seq[String],sinkMethod:Seq[String] ): Unit ={

    val sqlStringTaintAnalyzer = SqlStringTaintAnalyzer
    val sqlmemory = TaintMemorySQL
    sqlmemory.taint("TAINTED")


    for((stmt,index) <- tac.stmts.zipWithIndex){

      stmt match {
        case Assignment(pc,targetVar,VirtualFunctionCall(vfpc,declaringClass,isInterface, name, descriptor, receiver, params)) =>
          if(sourceNames.contains(name))_tainted +=index
          if(name == "append"||"toString" == name){
            if(receiver.isVar){
              if(_tainted.contains(receiver.asVar.definedBy.head)){
                _tainted += index
              }
            }
            for(param <- params){
              if(param.isVar){
                if(_tainted.contains(param.asVar.definedBy.head)){
                  _tainted += index
                }
              }
            }
          }
          if(toInspectMethods.contains(name)){
            for(param <- params){
              if(param.isVar){
                val definedBy = param.asVar.definedBy.head
                val value = getValue(definedBy)
                  println((index,definedBy,value))
                  //analye sql
                  if(name == "executeQuery" && sqlStringTaintAnalyzer.doAnalyze(value) ){
                    _tainted += index
                  }
              }
            }
          }

        case VirtualMethodCall(pc,declaringClass,isInterface,name,descriptor,receiver,params) =>
          if(toInspectMethods.contains(name)){
            for(param <- params){
              if(param.isVar){
                val definedBy = param.asVar.definedBy.head
                if(_tainted.contains(definedBy)){
                  val value = getValue(definedBy)
                  println((index,definedBy,value))
                  val er = sqlStringTaintAnalyzer.doAnalyze(value)
                  println(er)

                  //analyse sql

                }
              }
            }
          }
          if(sinkMethod.contains(name)){
            params.foreach(p => {
              if(p.isVar){
                val definedBy = p.asVar.definedBy.head
                if(_tainted.contains(definedBy)){
                  println(index)
                }
              }
            } )

          }


        case PutStatic(pc,declaringClass,name,declaredFieldType,UVar(value, defSites)) =>
          if(_tainted.contains(defSites.head)){
            _tainted += index
          }

        case PutField(pc,declaringClass,name,declaredFieldType,objRef,UVar(value, defSites)) =>
          if(_tainted.contains(defSites.head)){
            _tainted += index
          }

        case Assignment(pc,targetVar,GetStatic(gtpc,declaringClass,gname,declaredFieldType)) =>
          var i = index;
          while (i >= 0 ){
            _tac.get.stmts(i) match {
              case PutStatic(pc,declaringClass,pname,declaredFieldType,UVar(value, defSites)) =>
                if(gname == pname){
                  if(_tainted.contains(defSites.head)){
                    _tainted += index
                  }
                  i = -1
                }
              case _=>
            }
            i -= 1
          }

        case Assignment(pc,targetVar,GetField(gtpc,declaringClass,gname,declaredFieldType,objRef)) =>
          var i = index;
          while (i >= 0 ){
            _tac.get.stmts(i) match {
              case PutField(pc,declaringClass,pname,declaredFieldType,objRef,UVar(value, defSites)) =>
                if(gname == pname){
                  if(_tainted.contains(defSites.head)){
                    _tainted += index
                  }
                  i = -1
                }
              case _=>
            }
            i -= 1
          }


        case _=>
      }
    }


  }


  def getValue(index:Int): String = {
    var retValue =""

    _tac.get.stmts(index) match {

      case Assignment(pc,targetVar,VirtualFunctionCall(vfpc,declaringClass,isInterface, name, descriptor, receiver, params)) =>
        name match {
          case "source" =>
            retValue += "TAINTED"
          case "toString" => val receiverVarDefSide = receiver.asVar.definedBy.head
            retValue += getValue(receiverVarDefSide)
          case "append" =>
            val paramVarDefSide = params(0).asVar.definedBy.head
            val receiverVarDefSide = receiver.asVar.definedBy.head
            retValue += getValue(receiverVarDefSide) + getValue(paramVarDefSide)
          case _=>
        }

      case Assignment(pc,targetVar,New(npc, tpe)) =>
          if(tpe.fqn == "java/lang/StringBuilder") retValue +=""

      case Assignment(pc,targetVar,StringConst(spc,value)) =>
        retValue += value

      case Assignment(pc,targetVar,GetStatic(gtpc,declaringClass,gname,declaredFieldType)) =>
        var i = index;
        while (i >= 0 ){
          _tac.get.stmts(i) match {
            case PutStatic(pc,declaringClass,pname,declaredFieldType,UVar(value, vdefSites)) =>
              if(gname == pname){
                retValue += getValue(vdefSites.head)
                i = -1
              }
            case _=>
          }
          i -= 1
        }

      case Assignment(pc,targetVar,GetField(gfpc,declaringClass,gname,declaredFieldType,objRef)) =>
        var i = index;
        while (i >= 0 ){
          _tac.get.stmts(i) match {
            case PutField(pc,declaringClass,pname,declaredFieldType,objRef,UVar(value, vdefSites)) =>
              if(gname == pname){
                retValue += getValue(vdefSites.head)
                i = -1
              }
            case _=>
          }
          i -= 1
        }


      case _=>
    }

    retValue
  }


  def searchValueAndStatusbyFunctionName(params:Seq[Expr[DUVar[ValueInformation]]]): Unit ={


        params.foreach(param => {





        })
  }

}

