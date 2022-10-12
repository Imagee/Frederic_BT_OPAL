
package org.opalj.tac.fpcf.analyses
import org.opalj.br.{DeclaredMethod}
import org.opalj.br.analyses.SomeProject
import org.opalj.br.fpcf.properties.Context
import org.opalj.tac.{Assignment, PutField, PutStatic}
import org.opalj.tac.fpcf.properties.{IFDSProperty, IFDSPropertyMetaInformation}

/*
sealed trait Fact extends AbstractIFDSFact
case class Variable(index:Int, value:Option[String]) extends Fact
case class StaticField(classtype: ObjectType,value:Option[String]) extends Fact
case class InstanceField(index: Int, classType: ObjectType, fieldName: String, value:Option[String]) extends Fact

 */




class SQLTaintAnalysis02(val project: SomeProject ) extends AbstractIFDSAnalysis[Fact]{
  /**
   * Provides the concrete property key that must be unique for every distinct concrete analysis
   * and the lower bound for the IFDSProperty.
   */
  override val propertyKey: IFDSPropertyMetaInformation[Fact] = null // how to use

  /**
   * Creates an IFDSProperty containing the result of this analysis.
   *
   * @param result Maps each exit statement to the facts valid after the exit statement.
   * @return An IFDSProperty containing the `result`.
   */
  override def createPropertyValue(result: Map[Statement, Set[Fact]]): IFDSProperty[Fact] = ???

  /**
   * Computes the data flow for a normal statement.
   *
   * @param statement The analyzed statement.
   * @param successor The successor of the analyzed `statement`, to which the data flow is considered.
   * @param in        Some facts valid before the execution of the `statement`.
   * @return The facts valid after the execution of `statement`
   *         under the assumption that `in` held before `statement` and `successor` will be executed next.
   */
  override def normalFlow(statement: Statement, successor: Statement, in: Set[Fact]): Set[Fact] = {
    statement.stmt.astID match {
      case Assignment.ASTID =>in
      case PutStatic.ASTID =>in
      case PutField.ASTID =>in

      case _=> in
    }
  }

  /**
   * Computes the data flow for a call to start edge.
   *
   * @param call          The analyzed call statement.
   * @param calleeContext The called method.
   * @param in            Some facts valid before the execution of the `call`.
   * @return The facts valid after the execution of `statement` under the assumption that `in` held before `statement` and `statement` calls `callee`.
   */
  override def callFlow(call: Statement, calleeContext: Context, in: Set[Fact]): Set[Fact] = ???

  /**
   * Computes the data flow for a exit to return edge.
   *
   * @param call          The statement, which called the `callee`.
   * @param calleeContext The method called by `call`.
   * @param exit          The statement, which terminated the `calle`.
   * @param successor     The statement of the caller, which will be executed after the `callee` returned.
   * @param in            Some facts valid before the execution of the `exit`.
   * @return The facts valid after the execution of `exit` in the caller's context
   *         under the assumption that `in` held before the execution of `exit` and that `successor` will be executed next.
   */
  override def returnFlow(call: Statement, calleeContext: Context, exit: Statement, successor: Statement, in: Set[Fact]): Set[Fact] = ???

  /**
   * Computes the data flow for a call to return edge.
   *
   * @param call      The statement, which invoked the call.
   * @param successor The statement, which will be executed after the call.
   * @param in        Some facts valid before the `call`.
   * @return The facts valid after the call independently of what happens in the callee under the assumption that `in` held before `call`.
   */
  override def callToReturnFlow(call: Statement, successor: Statement, in: Set[Fact]): Set[Fact] = ???

  /**
   * Computes the data flow for a summary edge of a native method call.
   *
   * @param call          The statement, which invoked the call.
   * @param calleeContext The method, called by `call`.
   * @param successor     The statement, which will be executed after the call.
   * @param in            Some facts valid before the `call`.
   * @return The facts valid after the call, excluding the call-to-return flow.
   */
  override def nativeCall(call: Statement, calleeContext: Context, successor: Statement, in: Set[Fact]): Set[Fact] = ???

  /**
   * The entry points of this analysis.
   */
  override val entryPoints: Map[DeclaredMethod, Fact] = null // how to use
  //override val project: SomeProject = null // how to use

}
