

import org.opalj.br.analyses.{BasicReport, Project, ProjectAnalysisApplication}
import org.opalj.tac.{AITACode, Assignment, ExprStmt, LazyDetachedTACAIKey, StaticFunctionCall, StringConst, TACMethodParameter, UVar, VirtualFunctionCall, VirtualMethodCall}
import org.opalj.value.ValueInformation

import java.net.URL

object MyFirstAnalysis extends ProjectAnalysisApplication {
    override def doAnalyze(project: Project[URL], parameters: Seq[String], isInterrupted: () => Boolean): BasicReport = {

      val tacProvider = project.get(LazyDetachedTACAIKey)
      var allStringset : Set[String] = Set()
      var certainStringset : Set[String] = Set()
      val methodName = "main"
      val usedMethods = Seq("insertStatement", "selectStatement")

      for {
        cf <- project.allProjectClassFiles
        m <- cf.methods
        if m.body.isDefined && m.name == methodName
      } {
        val tac = tacProvider(m)

        println("\n Print all String: ")
        allStringset = getAllStrings(tac)
        allStringset.foreach(str => println(str))


        println("\n Print all Strings used by certain Methods: ")
        certainStringset = getAllStringsUsedByCertainMethods(tac,usedMethods)
        certainStringset.foreach(str => println(str))










        /*
        //println(m.toJava(ToTxt(tac).mkString("\n", "\n", "\n"))+"\n\n")
        tac.stmts.foreach( st => {

          st match {
            case PutField( pc,declaringClass,name,declaredFieldType,objRef,v@ UVar(value,defSites)) =>
              val i = defSites.iterator.next()
              val expr = tac.stmts(i).asAssignment.expr
              if(expr.isStringConst) println("")
               // println(expr.asStringConst.value) //println(v.value.toString)

            case Assignment(pc, targetVar, StringConst(spc,value))  =>
              set += value //println(value)

            case Assignment(pc, targetVar,
            VirtualFunctionCall(vpc, declaringClass, isInterface, name, descriptor, receiver, params)) =>
              params.foreach( v => {
                v match {
                  case UVar(value,defSites) =>
                    val i = defSites.iterator.next()
                    val expr = tac.stmts(i).asAssignment.expr
                    if(expr.isStringConst) println(expr.asStringConst.value)
                  case _ =>
                }
              })

            case VirtualMethodCall(pc, declaringClass, isInterface, name, descriptor, receiver, params) =>
              params.foreach(p => {
              p match {
                case pv@ UVar(value,defSites) =>
                  val i = defSites.iterator.next()
                  val expr = tac.stmts(i).asAssignment.expr
                  if(expr.isStringConst) println(expr.asStringConst.value)

                case _=>
              }
            })

            case _ =>
          }
        })
      }



         */

        //println(set)
      }

      /*
       Assign -> expr (StringConst?) -> value
       Assign -> expr (VirtualFunct) ->

       Ziel:
       taint
       tainted


       dataflow:
       Virtmeth -> name -> "sink"?
        -> params -> 0 - n -> defSites -> i - n

        for(i in n){
          stmt(i) -> Assisgn? -> expr(StringCont) -> value
                              -> expr(VirtFunk)
        }


       */



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
          + certainStringset.mkString(" Strings used by insertStatement, selectStatement: \n","\n","\n")
        )
    }



  def getAllStrings(tac: AITACode[TACMethodParameter, ValueInformation]): Set[String] ={
    var set: Set[String] = Set()

    tac.stmts.foreach(stmt => {
      stmt match {
        case Assignment(pc, targetVar, StringConst(spc,value))  =>
          set += value //println(value)

        case _ =>
      }
    })

    return set
  }


  def getAllStringsUsedByCertainMethods(tac: AITACode[TACMethodParameter, ValueInformation], methods: Seq[String]): Set[String] ={
    var set : Set[String] = Set()

    tac.stmts.foreach(stmt => {
      stmt match {
        case VirtualMethodCall(pc, declaringClass, isInterface, "insertStatement", descriptor, receiver, params) =>
          params.foreach(p => {
            p match {
              case pv@UVar(value, defSites) =>
                val i = defSites.iterator.next()
                val expr = tac.stmts(i).asAssignment.expr
                if (expr.isStringConst) set += expr.asStringConst.value


              case _ =>
            }})


        case Assignment(pc, targetVar,
        VirtualFunctionCall(vpc, declaringClass, isInterface, "selectStatement", descriptor, receiver, params)) =>
          params.foreach( v => {
            v match {
              case UVar(value,defSites) =>
                val i = defSites.iterator.next()
                val expr = tac.stmts(i).asAssignment.expr
                if(expr.isStringConst) set += expr.asStringConst.value
              case _ =>
            }
          })

        case Assignment(pc,targetVar,
        ss@ StaticFunctionCall( spc, declaringClass,isInterface, name, descriptor, params )) =>


        case ExprStmt(pc,VirtualFunctionCall(vpc,declaringClass,isInterface,"selectStatement",descriptor,receiver,params))=>
          params.foreach( v => {
            v match {
              case UVar(value,defSites) =>
                val i = defSites.iterator.next()
                val expr = tac.stmts(i).asAssignment.expr
                if(expr.isStringConst) set += expr.asStringConst.value
              case _ =>
            }
          })

        case _ =>
          }}
      )
          return set
  }
}