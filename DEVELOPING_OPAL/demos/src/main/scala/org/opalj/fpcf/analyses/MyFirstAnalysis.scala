

import org.opalj.br.analyses.{BasicReport, Project, ProjectAnalysisApplication}
import org.opalj.collection.immutable.IntTrieSet
import org.opalj.tac.{AITACode, Assignment, DUVar, Expr, ExprStmt, GetField, GetStatic, LazyDetachedTACAIKey, PutField, PutStatic, StaticFunctionCall, StringConst, TACMethodParameter, UVar, VirtualFunctionCall, VirtualMethodCall}
import org.opalj.value.ValueInformation

import java.net.URL

object MyFirstAnalysis extends ProjectAnalysisApplication {

    override def doAnalyze(project: Project[URL], parameters: Seq[String], isInterrupted: () => Boolean): BasicReport = {

      val tacProvider = project.get(LazyDetachedTACAIKey)
      var allStringset : Set[String] = Set()
      var certainStringset : Set[String] = Set()
      val methodName = "main" //"<clinit>"
      val usedMethods = Seq("insertStatement","selectStatement", "execSelect", "execInsert")


      for {
        cf <- project.allProjectClassFiles
        m <- cf.methods
        if m.body.isDefined
        if m.name == "<init>" || m.name == methodName ||m.name == "<clinit>"
      } {

        val tac = tacProvider(m)
        val initMethod = cf.methods.filter(m => m.name == "<init>").head
        val intTAC = tacProvider(initMethod)

        val clinitMethod = cf.methods.filter(m => m.name =="<clinit>").head

        val clinitTAC = tacProvider(clinitMethod)

        allStringset ++= getAllStrings(tac)
        certainStringset ++= getAllStringsUsedByCertainMethods(tac,intTAC,clinitTAC,usedMethods)

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

      BasicReport(
        "\nResult of MyFirstAnalsis: \n" + allStringset.mkString(" all found Strings: \n","\n","\n \n")
          + certainStringset.mkString(" Strings used by" + usedMethods.mkString(" ",", ",":") +" \n","\n","\n")
        )
    }



  def getAllStrings(tac: AITACode[TACMethodParameter, ValueInformation]): Set[String] ={
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


  def getAllStringsUsedByCertainMethods(tac: AITACode[TACMethodParameter, ValueInformation],init: AITACode[TACMethodParameter, ValueInformation],clinit: AITACode[TACMethodParameter, ValueInformation], methods: Seq[String]): Set[String] ={
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

  def searchStaticFiledValue(
                              methods: Seq[String],
                              name:String,
                              tac: AITACode[TACMethodParameter, ValueInformation],
                              init: AITACode[TACMethodParameter, ValueInformation]
                            ):Unit = {

  }

  def searchValuebyFunctionName(
          methods: Seq[String],
          name:String,
          params: Seq[Expr[DUVar[ValueInformation]]],
          tac: AITACode[TACMethodParameter, ValueInformation],
          init: AITACode[TACMethodParameter, ValueInformation],
          clinit: AITACode[TACMethodParameter, ValueInformation]
        ):Set[String] = {

    var set : Set[String] = Set()
    if(methods.contains(name)) {
      params.foreach(p => {
        p match {
          case UVar(value, defSites) =>
            set ++= searchValue(defSites,tac,init,clinit)
          case _ =>
        }
      })
    }
    set
  }


  def searchValue(
          defSites:IntTrieSet,
          tac: AITACode[TACMethodParameter, ValueInformation],
          init: AITACode[TACMethodParameter, ValueInformation],
          clinit: AITACode[TACMethodParameter, ValueInformation]
                 ):Set[String] = {
    val defSiteIterator = defSites.iterator
    var set : Set[String] = Set()

    while(defSiteIterator.hasNext){
      val i = defSiteIterator.next()
      if(i >= 0){
          tac.stmts(i).asAssignment.expr match {
          case StringConst(pc,value) => set += value
          case GetField(pc, declaringClass, name, declaredFieldType, objRef) =>
            set++=searchFieldValue(name,tac, init,clinit)
            set++=searchFieldValue(name,init,init,clinit)

          case GetStatic(pc,declaringClass,name,declaredFieldType) =>
            set++= searchFieldValue(name,tac, init,clinit)
            set++= searchFieldValue(name,clinit,init,clinit)
          case _=>
        }
      }else{
        println("lad in Else")
      }
    }
    set
  }

  def searchFieldValue(
          FieldName: String,
          tac: AITACode[TACMethodParameter, ValueInformation],
          init: AITACode[TACMethodParameter, ValueInformation],
          clinit: AITACode[TACMethodParameter, ValueInformation],
           ):Set[String] =  {
    var set : Set[String] = Set()

    tac.stmts.foreach(stmt => {
      stmt match {
        case PutField( pc,declaringClass,name,declaredFieldType,objRef,v@ UVar(value,defSites)) if (name == FieldName) =>
            set ++= searchValue(defSites, tac, init,clinit)

        case PutStatic(pc,declaringClass,name,declaredFieldType,value) if (name == FieldName )=>
            set++= searchValue(value.asVar.definedBy,tac, init,clinit)

        case _=>
      }
    })

    return set
  }


  def doAnalyseSQlstmts():String = {

    ""
  }

  def filterSQLString(strings:Set[String]):Set[String] = {
    var set : Set[String] = Set()
    val word = raw"(\w+|'\w+')"
    val columnWord = raw"(\w+|'\w+'|`\w+`)"
    val tableWord = raw"((('\w+-)|\w+.)+|$word)"

    val insertPattern = raw"INSERT (IGNORE)? INTO $tableWord \( $columnWord (, $columnWord )*\) VALUES \( $word (, $word )*\) (, \( $word (, $word )*\) )*;".r
    val selectPattern = raw"SELECT ((\w+ (, \w*))|\*)+ FROM \w+( WHERE \w+ = '[^']*')* ;".r
    val updatePattern = raw"UPDATE [a-zA-Z]\w+ SET \w+ = (\w+|'\w+') (, \w+ = (\w+|'\w+'))* (WHERE .+)?;".r

    strings.foreach(str => {

      str match {
        case i@ insertPattern(_*) => set += i
        case s@ selectPattern(_*) => set += s
        case u@ updatePattern(_*) => set += u
        case _ =>
      }
    })

    set
  }

}

