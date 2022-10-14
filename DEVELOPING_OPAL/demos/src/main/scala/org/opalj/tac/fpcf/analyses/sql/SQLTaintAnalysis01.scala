
package org.opalj.tac.fpcf.analyses.sql

import org.opalj.br.analyses.{BasicReport, Project, ProjectAnalysisApplication}
import org.opalj.br.{ClassFile, Method, ObjectType}
import org.opalj.tac._
import org.opalj.value.ValueInformation

import java.net.URL

object SQLTaintAnalysis01 extends ProjectAnalysisApplication {
  type V = DUVar[ValueInformation]
  type TACAICode = AITACode[TACMethodParameter, ValueInformation]
  var tacProvider = None: Option[Function1[Method, TACAICode]]
  val sourceName = "source"
  val sinkName = "sink"
  val dummySourceString = "ghjgh"
  var _project = None: Option[Project[URL]]
  var mainTAC = None: Option[TACAICode]
  val classToInspect = "MysqlConnection"
  val entryMethod = "insertData"
  var toInspectMethods = None: Option[Seq[String]]

  sealed trait Fact

  case class Variable(index: Int) extends Fact

  case class StaticField(classtype: ObjectType, fieldName: String) extends Fact

  case class InstanceField(index: Int, classType: ObjectType, fieldName: String) extends Fact

  override def doAnalyze(project: Project[URL], parameters: Seq[String], isInterrupted: () => Boolean): BasicReport = {
    _project = Some(project)
    tacProvider = Some(project.get(LazyDetachedTACAIKey))
    toInspectMethods = Some(parameters)

    val needClassFile = _project.get.allProjectClassFiles.find(cf => cf.fqn == classToInspect).get
    val neededMethod = needClassFile.methods.find(m => m.name == entryMethod && m.body.isDefined).get
    mainTAC = Some(tacProvider.get(neededMethod))

    sqlAnalyze(mainTAC.get, sourceName, sinkName, toInspectMethods.get, Set.empty[Fact])


    BasicReport(""
      /*
      "\nResult of MyFirstAnalsis: \n" + allStringset.mkString(" all found Strings: \n","\n","\n \n")
        + certainStringset.mkString(" Strings used by" + usedMethods.mkString(" ",", ",":") +" \n","\n","\n")

       */
    )

  }

  def sqlAnalyze(tac: TACAICode, source: String, sink: String, toInspectMethods: Seq[String], oldIn: Set[Fact]): Set[Fact] = {

    var in: Set[Fact] = oldIn
    for ((stmt, index) <- tac.stmts.zipWithIndex) {

      stmt match {

        case assignment: Assignment[V] => in ++= handleAssignment(index, assignment, in)
        case staticMethodCall: StaticMethodCall[V] => in ++= handleCalls(index, staticMethodCall, in)

        /*
        case Assignment(pc,targetVar,GetStatic(gpc,declaringClass,name,declaredFieldType)) =>
          if(in.contains(StaticField(declaringClass,name)))
            in += Variable(index)

        case Assignment(pc,targetVar,GetField(gpc,declaringClass,name,declaredFieldType,objRef)) =>
          if(in.exists {
            case InstanceField(index, classType, fname) =>  fname == name && objRef.asVar.definedBy.contains(index)
            case Variable(index) => objRef.asVar.definedBy.contains(index)
            case _ => false
          })
            in += Variable(index)

         */

        case PutStatic(pc, declaringClass, name, declaredFieldType, value) =>
          if (isTainted(value, in))
            in += StaticField(declaringClass, name)
          else
            in -= StaticField(declaringClass, name)
        case PutField(pc, declaringClass, name, declaredFieldType, objRef, value) =>
          if (isTainted(value, in))
            in = objRef.asVar.definedBy.foldLeft(in) { (in, defsite) => in + InstanceField(defsite, declaringClass, name) }
          else
            in = objRef.asVar.definedBy.foldLeft(in) { (in, defsite) => in - InstanceField(defsite, declaringClass, name) }


        /*
        case Assignment(pc,targetVar,v@VirtualFunctionCall(vpc,declaringClass,isInterface,name,descriptor,receiver,params)) =>
          // sind die Parameter getainted ?

          if(name == source){
            //handleSourceCall()
            in += Variable(index)
          }else {
            var callFacts = Set.empty[Fact]
            in.foreach{
              case Variable(index) =>
                params.zipWithIndex.foreach{
                  case (param,pIndex) if param.asVar.definedBy.contains(index) =>
                    callFacts += Variable(paramToIndex(pIndex,!v.isStaticFunctionCall))
                  case _=>
                }

              case InstanceField(index, classType, fieldName)  =>
                params.zipWithIndex.foreach{
                      // TODO Noch nach Objecttype vergleichen
                  case (param,pIndex) if param.asVar.definedBy.contains(index)  =>
                    callFacts += InstanceField(paramToIndex(pIndex,!v.isStaticFunctionCall), classType, fieldName)
                  case _=>
                }
              case sf:StaticField =>
                callFacts += sf
            }
          }

           */

        case _ =>
      }


    }
    in
  }

  def isTainted(expr: Expr[V], in: Set[Fact]): Boolean = {
    expr.isVar && in.exists {
      case Variable(index) => expr.asVar.definedBy.contains(index)
      case InstanceField(index, classType, fieldName) => expr.asVar.definedBy.contains(index)
      case _ => false
    }
  }

  def paramToIndex(param: Int, includeThis: Boolean): Int = (if (includeThis) -1 else -2) - param

  //TODO
  //desides wich call should be used
  def handleCalls(index: Int, stmt: Stmt[V], in: Set[Fact]) = {
    val callFacts = Set.empty[Fact]
    //...

    stmt match {
      case StaticMethodCall(pc, declaringClass, isInterface, name, descriptor, params) =>
        handleInSameProjectCall(index, stmt, in)
      case _ =>
    }

    callFacts
  }

  //TODO
  def handleSameClassCall(index: Int, stmt: Stmt[V], in: Set[Fact]) = {

  }

  //TODO
  def handleInSameProjectCall(index: Int, stmt: Stmt[V], in: Set[Fact]) = {
    val callFacts = Set.empty[Fact]

    stmt match {
      case smc@StaticMethodCall(pc, declaringClass, isInterface, name, descriptor, params) =>
        //val tac = getNeededTAC(declaringClass.fqn,name)

        var callFacts = Set.empty[Fact]
        in.foreach {
          case Variable(index) =>
            params.zipWithIndex.foreach {
              case (param, pIndex) if param.asVar.definedBy.contains(index) =>
                callFacts += Variable(paramToIndex(pIndex, false))
              case _ =>
            }

          case InstanceField(index, classType, fieldName) =>
            params.zipWithIndex.foreach {
              // TODO Noch nach Objecttype vergleichen
              case (param, pIndex) if param.asVar.definedBy.contains(index) =>
                callFacts += InstanceField(paramToIndex(pIndex, false), classType, fieldName)
              case _ =>
            }
          case sf: StaticField =>
            callFacts += sf
        }
      //taintAnalyze(tac,sourceName,sinkName,toInspectMethods.get,callFacts)

      case _ =>
    }
    callFacts
  }

  //TODO
  def handleSpecialCalls() = {

  }

  //TODO
  def handleLibraryCall() = {
  }

  //TODO
  def handleNornmalCall(): Unit = {

  }

  def handleSourceCall() = {

  }

  def handleAssignment(index: Int, stmt: Stmt[V], in: Set[Fact]): Set[Fact] = {
    stmt.asAssignment.expr match {
      case stringConst: StringConst =>
        if (stringConst.value == dummySourceString) in + Variable(index)
        else in

      case stf: StaticFunctionCall[V] =>
        if (stf.name == sourceName) in + Variable(index)
        else {
          //TODO
          handleCalls(index, stmt, in)

          in
        }
    }
  }


  //TODO
  def analyzeClass(cl: ClassFile, entryMethod: String, source: String, sink: String, toInspectMethods: Seq[String]) = {


  }

  def getNeededTAC(className: String, methodName: String) = {
    if (className != classToInspect || methodName != entryMethod) {
      val needClassFile = _project.get.allProjectClassFiles.find(cf => cf.fqn == className).get
      val neededMethod = needClassFile.methods.find(m => m.name == methodName && m.body.isDefined).get
      val neededTAC = Some(tacProvider.get(neededMethod)).get
      neededTAC
    }
    mainTAC.get
  }


}
