
package org.opalj.tac.fpcf.analyses.sql

import org.opalj.br.analyses.SomeProject
import org.opalj.br.fpcf.properties.Context
import org.opalj.br.{DeclaredMethod, ObjectType}
import org.opalj.fpcf.PropertyKey
import org.opalj.tac._
import org.opalj.tac.fpcf.analyses.AbstractIFDSAnalysis.V
import org.opalj.tac.fpcf.analyses.{AbstractIFDSAnalysis, AbstractIFDSFact, AbstractIFDSNullFact, Statement}
import org.opalj.tac.fpcf.properties.{IFDSProperty, IFDSPropertyMetaInformation}


sealed trait SQLFact extends AbstractIFDSFact
case class SQLVariable(index: Int) extends SQLFact
case class SQLStaticField(classType: ObjectType, fieldName: String) extends SQLFact
case class SQLInstanceField(index: Int, classType: ObjectType, fieldName: String) extends SQLFact
case object SQLNullFact extends SQLFact with AbstractIFDSNullFact


class SQLTaintAnalysis02(implicit val project: SomeProject ) extends AbstractIFDSAnalysis[SQLFact]{
  /**
   * Provides the concrete property key that must be unique for every distinct concrete analysis
   * and the lower bound for the IFDSProperty.
   */
  override val propertyKey: IFDSPropertyMetaInformation[SQLFact] = SQLTaint

  /**
   * Creates an IFDSProperty containing the result of this analysis.
   *
   * @param result Maps each exit statement to the facts valid after the exit statement.
   * @return An IFDSProperty containing the `result`.
   */
  override def createPropertyValue(result: Map[Statement, Set[SQLFact]]): IFDSProperty[SQLFact] = {
    new SQLTaint(result)
  }

  /**
   * Computes the data flow for a normal statement.
   *
   * @param stmt The analyzed statement.
   * @param successor The successor of the analyzed `statement`, to which the data flow is considered.
   * @param in        Some facts valid before the execution of the `statement`.
   * @return The facts valid after the execution of `statement`
   *         under the assumption that `in` held before `statement` and `successor` will be executed next.
   */
  override def normalFlow(stmt: Statement, successor: Statement, in: Set[SQLFact]): Set[SQLFact] = {
    stmt.stmt.astID match {
      case Assignment.ASTID =>
        handleAssignment(stmt, stmt.stmt.asAssignment.expr, in)

      case PutStatic.ASTID =>
        val put = stmt.stmt.asPutStatic
        if (isTainted(put.value, in))
          in + SQLStaticField(put.declaringClass, put.name)
        else
          in

      case PutField.ASTID =>
        val put = stmt.stmt.asPutField
        val definedBy = put.objRef.asVar.definedBy
        if (isTainted(put.value, in))
          definedBy.foldLeft(in) { (in, defSite) =>
            in + SQLInstanceField(defSite, put.declaringClass, put.name)
          }
        else
          in

      case _=> in
    }
  }

  /**
   * Returns true if the expression contains a taint.
   */
  def isTainted(expr: Expr[V], in: Set[SQLFact]): Boolean = {
    expr.isVar && in.exists {
      case SQLVariable(index)            => expr.asVar.definedBy.contains(index)
      case SQLInstanceField(index, _, _) => expr.asVar.definedBy.contains(index)
      case _                          => false
    }
  }

  def handleAssignment(stmt: Statement, expr: Expr[V], in: Set[SQLFact]): Set[SQLFact] = {
    expr.astID match {
      case Var.ASTID =>
        // This path is not used if the representation is in standard SSA-like form.
        // It is NOT optimized!
        val newTaint = in.collect {
          case SQLVariable(index) if expr.asVar.definedBy.contains(index) =>
            Some(SQLVariable(stmt.index))
          case _ => None
        }.flatten
        in ++ newTaint

      case GetStatic.ASTID =>
        val get = expr.asGetStatic
        if (in.contains(SQLStaticField(get.declaringClass, get.name)))
          in + SQLVariable(stmt.index)
        else
          in

      case GetField.ASTID =>
        val get = expr.asGetField
        if (in.exists {
          // The specific field may be tainted
          case SQLInstanceField(index, _, taintedField) =>
            taintedField == get.name && get.objRef.asVar.definedBy.contains(index)
          // Or the whole object
          case SQLVariable(index) => get.objRef.asVar.definedBy.contains(index)
          case _ => false
        })
          in + SQLVariable(stmt.index)
        else
          in

      case _ => in
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
  override def callFlow(call: Statement, calleeContext: Context, in: Set[SQLFact]): Set[SQLFact] = ???

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
  override def returnFlow(call: Statement, calleeContext: Context, exit: Statement, successor: Statement, in: Set[SQLFact]): Set[SQLFact] = ???

  /**
   * Converts a parameter origin to the index in the parameter seq (and vice-versa).
   */
  def paramToIndex(param: Int, includeThis: Boolean): Int = (if (includeThis) -1 else -2) - param


  /**
   * Computes the data flow for a call to return edge.
   *
   * @param call      The statement, which invoked the call.
   * @param successor The statement, which will be executed after the call.
   * @param in        Some facts valid before the `call`.
   * @return The facts valid after the call independently of what happens in the callee under the assumption that `in` held before `call`.
   */
  override def callToReturnFlow(call: Statement, successor: Statement, in: Set[SQLFact]): Set[SQLFact] = ???

  /**
   * Computes the data flow for a summary edge of a native method call.
   *
   * @param call          The statement, which invoked the call.
   * @param calleeContext The method, called by `call`.
   * @param successor     The statement, which will be executed after the call.
   * @param in            Some facts valid before the `call`.
   * @return The facts valid after the call, excluding the call-to-return flow.
   */
  override def nativeCall(call: Statement, calleeContext: Context, successor: Statement, in: Set[SQLFact]): Set[SQLFact] = ???

  /**
   * The entry points of this analysis.
   */
  override val entryPoints: Map[DeclaredMethod, SQLFact] = null // how to use
  //override val project: SomeProject = null // how to use


}


class SQLTaint(val flows: Map[Statement, Set[SQLFact]]) extends IFDSProperty[SQLFact] {

  override type Self = SQLTaint

  def key: PropertyKey[SQLTaint] = SQLTaint.key
}

object SQLTaint extends IFDSPropertyMetaInformation[SQLFact]{
  override type Self = SQLTaint

  /**
   * The key uniquely identifies this property's category. All property objects
   * of the same kind have to use the same key.
   *
   * In general each `Property` kind is expected to have a companion object that
   * stores the unique `PropertyKey`.
   */
  override def key: PropertyKey[SQLTaint] = PropertyKey.create(
    "SQLTestTaint",
    new SQLTaint(Map.empty)
  )
}
