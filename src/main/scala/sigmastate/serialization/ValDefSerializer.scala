package sigmastate.serialization

import sigmastate.Values._
import sigmastate._
import sigmastate.serialization.OpCodes._
import scorex.util.Extensions._
import sigmastate.utils.{SigmaByteReader, SigmaByteWriter}
import ValueSerializer._
import scala.collection.mutable

case class ValDefSerializer(override val opDesc: ValueCompanion) extends ValueSerializer[ValDef] {

  override def serialize(obj: ValDef, w: SigmaByteWriter): Unit = {
    w.putUInt(obj.id)
    optional("type arguments") {
      if (opCode == FunDefCode) {
        require(!obj.isValDef, s"expected FunDef, got $obj")
        require(obj.tpeArgs.nonEmpty, s"expected FunDef with type args, got $obj")
        w.put(obj.tpeArgs.length.toByteExact)
        obj.tpeArgs.foreach(w.putType(_))
      }
    }
    w.putValue(obj.rhs)
  }

  override def parse(r: SigmaByteReader): Value[SType] = {
    val id = r.getUInt().toInt
    val tpeArgs: Seq[STypeVar] = opCode match {
      case FunDefCode =>
        val tpeArgsCount = r.getByte()
        val inputsBuilder = mutable.ArrayBuilder.make[STypeVar]()
        for (_ <- 0 until tpeArgsCount) {
          inputsBuilder += r.getType().asInstanceOf[STypeVar]
        }
        inputsBuilder.result()
      case ValDefCode =>
        Seq()
    }
    val rhs = r.getValue()
    r.valDefTypeStore(id) = rhs.tpe
    ValDef(id, tpeArgs, rhs)
  }
}
