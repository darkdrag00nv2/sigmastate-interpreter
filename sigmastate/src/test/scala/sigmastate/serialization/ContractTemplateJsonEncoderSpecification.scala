package sigmastate.serialization

import org.ergoplatform.JsonCodecs
import sigmastate.{CrossVersionProps, EQ, Plus, SInt, SType, Values}
import sigmastate.Values.{BigIntConstant, ByteConstant, ConstantPlaceholder, ContractTemplate, IntConstant, Parameter, ShortConstant, SigmaPropValue}
import sigmastate.eval.{CBigInt, IRContext}
import sigmastate.helpers.SigmaTestingCommons

import java.math.BigInteger

class ContractTemplateJsonEncoderSpecification extends SerializationSpecification
  with SigmaTestingCommons with CrossVersionProps {
  object JsonCodecs extends JsonCodecs

  def roundtrip[T <: SType](obj: ContractTemplate) = {
    val json = ContractTemplate.jsonEncoder.encoder(obj)
    val res = ContractTemplate.jsonEncoder.decoder(json.hcursor).right.get
    res shouldBe obj
  }

  private def createParameter(name: String, placeholder: Int): Parameter = {
    Parameter(
      name,
      s"${name}_description",
      placeholder
    )
  }

  private def createContractTemplate(constTypes: IndexedSeq[SType],
                                     constValues: Option[IndexedSeq[Option[SType#WrappedType]]],
                                     parameters: IndexedSeq[Parameter],
                                     expressionTree: SigmaPropValue): ContractTemplate = {
    ContractTemplate(
      contractTemplateNameInTests,
      contractTemplateDescriptionInTests,
      constTypes,
      constValues,
      parameters,
      expressionTree
    )
  }

  property("Data Json serialization round trip") {
    val parameters = IndexedSeq(
      createParameter("p1", 0),
      createParameter("p2", 1),
      createParameter("p3", 2))
    val templates = Seq(
      createContractTemplate(
        IndexedSeq(SType.typeByte, SType.typeByte, SType.typeByte).asInstanceOf[IndexedSeq[SType]],
        Some(IndexedSeq(Some(10.toByte), Some(20.toByte), Some(30.toByte)).asInstanceOf[IndexedSeq[Option[SType#WrappedType]]]),
        parameters,
        EQ(Plus(ConstantPlaceholder(0, SType.typeByte),
          ConstantPlaceholder(1, SType.typeByte)),
          ConstantPlaceholder(2, SType.typeByte)).toSigmaProp
      ),
      createContractTemplate(
        IndexedSeq(SType.typeByte, SType.typeByte, SType.typeByte).asInstanceOf[IndexedSeq[SType]],
        Some(IndexedSeq(Some(10.toByte), None, Some(30.toByte)).asInstanceOf[IndexedSeq[Option[SType#WrappedType]]]),
        parameters,
        EQ(Plus(ConstantPlaceholder(0, SType.typeByte),
          ConstantPlaceholder(1, SType.typeByte)),
          ConstantPlaceholder(2, SType.typeByte)).toSigmaProp
      ),
      createContractTemplate(
        IndexedSeq(SType.typeShort, SType.typeShort, SType.typeShort).asInstanceOf[IndexedSeq[SType]],
        Some(IndexedSeq(Some(10.toShort), Some(20.toShort), Some(30.toShort)).asInstanceOf[IndexedSeq[Option[SType#WrappedType]]]),
        parameters,
        EQ(Plus(ConstantPlaceholder(0, SType.typeShort),
          ConstantPlaceholder(1, SType.typeShort)),
          ConstantPlaceholder(2, SType.typeShort)).toSigmaProp
      ),
      createContractTemplate(
        IndexedSeq(SType.typeInt, SType.typeInt, SType.typeInt).asInstanceOf[IndexedSeq[SType]],
        Some(IndexedSeq(Some(10), Some(20), Some(30)).asInstanceOf[IndexedSeq[Option[SType#WrappedType]]]),
        parameters,
        EQ(Plus(ConstantPlaceholder(0, SType.typeInt),
          ConstantPlaceholder(1, SType.typeInt)),
          ConstantPlaceholder(2, SType.typeInt)).toSigmaProp
      ),
      createContractTemplate(
        SType.EmptySeq,
        None,
        Parameter.EmptySeq,
        EQ(Plus(CBigInt(BigInteger.valueOf(10L)), BigIntConstant(20L)), BigIntConstant(30L)).toSigmaProp
      )
    )

    templates.foreach { template =>
      roundtrip(template)
    }
  }
}
