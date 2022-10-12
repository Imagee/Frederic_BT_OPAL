
package org.opalj.tac

//import org.opalj.br.{Method}
import org.opalj.br.{ClassFile, ObjectType}
import org.opalj.br.analyses.{BasicReport, Project, ProjectAnalysisApplication}
import org.opalj.value.ValueInformation

import java.net.URL


object SQLTaintAnalysis01 extends ProjectAnalysisApplication{
  type V = DUVar[ValueInformation]
  type TACAICode = AITACode[TACMethodParameter,ValueInformation]
  val tacProvider = Some(null)

  sealed trait Fact
  case class Variable(index: Int) extends Fact
  case class StaticField(classtype: ObjectType, fieldName: String) extends Fact
  case class InstanceField(index: Int, classType: ObjectType, fieldName: String) extends Fact

  override def doAnalyze(project: Project[URL], parameters: Seq[String], isInterrupted: () => Boolean): BasicReport = {

    val tacProvider = project.get(LazyDetachedTACAIKey)
    val classToInspect ="BasicObject"
    //val sourceName = "source"
    //val sinkName = "sink"
    //val toInspectedMethod
    // taint value "ghjgh"
    val entryMethod="main"
    var tmp:ClassFile = null



    for {
      cf <- project.allProjectClassFiles
      if cf.fqn == classToInspect
      m <- cf.methods
      if m.body.isDefined
      if m.name == entryMethod
    }{

      val tac = Some(tacProvider(m))
      println(tac.value.pcToIndex.length)
      tmp = cf



    }




    BasicReport(""
      /*
      "\nResult of MyFirstAnalsis: \n" + allStringset.mkString(" all found Strings: \n","\n","\n \n")
        + certainStringset.mkString(" Strings used by" + usedMethods.mkString(" ",", ",":") +" \n","\n","\n")

       */
    )

  }

  def taintAnalyze(tac: TACAICode,source:String, sink:String,toInspectMethods:Seq[String]) ={


    var in = Set.empty[Fact]

    for((stmt,index) <- tac.stmts.zipWithIndex){

      stmt match {

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

        case PutStatic(pc,declaringClass,name,declaredFieldType,value) =>
          if(isTainted(value,in))
            in += StaticField(declaringClass, name)
          else
            in -= StaticField(declaringClass, name)
        case PutField(pc,declaringClass,name,declaredFieldType,objRef,value)=>
          if(isTainted(value,in))
            in += objRef.asVar.definedBy.foldLeft(in){(in,defsite) => in + InstanceField(defsite, declaringClass, name)}
          else
            in = objRef.asVar.definedBy.foldLeft(in){(in,defsite) => in - InstanceField(defsite, declaringClass, name)}


        case Assignment(pc,targetVar,v@VirtualFunctionCall(vpc,declaringClass,isInterface,name,descriptor,receiver,params)) =>
          // sind die Parameter getainted ?
          {
            var callFacts = Set.empty[Fact]
            in.foreach{
              case Variable(index) =>
                params.zipWithIndex.foreach{
                  case (param,pIndex) if param.asVar.definedBy.contains(index) =>
                    callFacts += Variable(paramToIndex(pIndex,true))
                  case _=>
                }

              case InstanceField(index, classType, fieldName)  =>
                params.zipWithIndex.foreach{
                      // TODO Noch nach Objecttype vergleichen
                  case (param,pIndex) if param.asVar.definedBy.contains(index)  =>
                    callFacts += InstanceField(paramToIndex(pIndex,true), classType, fieldName)
                  case _=>
                }
              case sf:StaticField =>
                callFacts += sf
            }
          }

        case _=>
      }


    }
  }

  def isTainted(expr: Expr[V],in:Set[Fact]):Boolean={
    expr.isVar && in.exists {
      case Variable(index) => false
      case InstanceField(index, classType, fieldName) => false
      case _=> false
    }
  }

  def paramToIndex(param: Int, includeThis: Boolean): Int = (if (includeThis) -1 else -2) - param

  def handleCalls(): Unit ={

  }

  def analyzeClass(cl:ClassFile,entryMethod:String,source:String, sink:String,toInspectMethods:Seq[String]) ={


  }




}
