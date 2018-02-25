package sigmastate.utxo

import sigmastate._
import sigmastate.interpreter.Interpreter


class UtxoInterpreter(override val maxCost: Int = CostTable.ScriptLimit) extends Interpreter {
  override type CTX = UtxoContext

  override def specificTransformations(context: UtxoContext): PartialFunction[Value[_ <: SType], Value[_ <: SType]] = {
    case Inputs => ConcreteCollection(context.boxesToSpend.map(BoxConstant.apply))

    case Outputs => ConcreteCollection(context.spendingTransaction.outputs.map(BoxConstant.apply))

    case Self => BoxConstant(context.self)

    case Height => IntConstant(context.currentHeight)

    case LastBlockUtxoRootHash => AvlTreeConstant(context.lastBlockUtxoRoot)

    case t: TaggedVariable[_] if context.extension.values.contains(t.id) =>
      context.extension.values(t.id)
  }
}