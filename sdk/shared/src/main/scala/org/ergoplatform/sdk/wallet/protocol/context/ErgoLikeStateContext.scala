package org.ergoplatform.sdk.wallet.protocol.context

import scorex.crypto.authds.ADDigest
import special.collection.Coll

/**
  * Blockchain context used in transaction validation.
  */
trait ErgoLikeStateContext {

  /**
    * @return fixed number (10 in Ergo) of last block headers
    */
  def sigmaLastHeaders: Coll[special.sigma.Header]

  // todo remove from ErgoLikeContext and from ErgoStateContext
  /**
    * @return UTXO set digest from a last header (of sigmaLastHeaders)
    */
  def previousStateDigest: Coll[Byte]

  /**
    * @return returns pre-header (header without certain fields) of the current block
    */
  def sigmaPreHeader: special.sigma.PreHeader
}

case class CErgoLikeStateContext(
  sigmaLastHeaders: Coll[special.sigma.Header],
  previousStateDigest: Coll[Byte],
  sigmaPreHeader: special.sigma.PreHeader
) extends ErgoLikeStateContext
