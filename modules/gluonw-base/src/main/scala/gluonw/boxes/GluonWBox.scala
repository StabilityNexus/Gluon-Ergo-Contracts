package gluonw.boxes

import commons.configs.GetServiceConfig.getServiceOwner
import commons.configs.MultiSig.getServiceMultiSig
import commons.configs.{
  GetServiceConfig,
  NodeConfig,
  OracleConfig,
  ServiceConfig
}
import edge.boxes.{Box, BoxWrapperHelper, BoxWrapperJson}
import edge.errors.ParseException
import edge.json.{ErgoJson, Register}
import edge.registers.{
  IntRegister,
  LongPairRegister,
  LongRegister,
  NumbersRegister,
  Register
}
import gluonw.boxes.GluonWBoxConstants.BUCKETS
import gluonw.common.{AssetPrice, GluonWAsset, GluonWConstants, GluonWTokens}
import gluonw.contracts.GluonWBoxContract
import io.circe.Json
import org.ergoplatform.appkit.{
  BlockchainContext,
  ErgoContract,
  ErgoValue,
  InputBox,
  NetworkType,
  Parameters,
  SigmaProp
}
import org.ergoplatform.sdk.{ErgoId, ErgoToken}
import special.collection.Coll

import scala.jdk.CollectionConverters.ListHasAsScala

object GluonWBoxConstants {
  val PRECISION: Long = 1_000_000_000L
  val DECIMALS: Double = 3.0
  val DECIMALS_FACTOR: Long = math.pow(10, DECIMALS).toLong
  val TOTAL_CIRCULATING_SUPPLY: Long = 1_000_000_000_000_000L * DECIMALS_FACTOR
  val PROTONS_TOTAL_CIRCULATING_SUPPLY: Long = TOTAL_CIRCULATING_SUPPLY
  val NEUTRONS_TOTAL_CIRCULATING_SUPPLY: Long = TOTAL_CIRCULATING_SUPPLY
  // This is the required fee for the box to be in existence. It's the minimum
  // amount a box require to have to exist on the ergo blockchain
  val GLUONWBOX_BOX_EXISTENCE_FEE: Long = Parameters.MinFee
  val GLUONWBOX_MAX_FEE: Long = 10_000_000L * Parameters.OneErg
  val BUCKETS: Int = 14 // Tracking volume of approximately 14 days
  val BLOCKS_PER_VOLUME_BUCKET
    : Int = 720 // Approximately 1 day per volume bucket

  // Units:
  // Reserve Units: nanoErg
  // Neutron Units: mUSD
  // Proton Units: mProton
  val INITIAL_RESERVE: Long = 1L * Parameters.OneErg
  val INITIAL_NEUTRON_CIRCULATING_SUPPLY: Long = 530 // i.e. 0.53 
  val INITIAL_PROTON_CIRCULATING_SUPPLY: Long = 10 // i.e 0.01 

}

case class GluonWBox(
  value: Long,
  totalSupplyRegister: LongPairRegister = new LongPairRegister(
    (
      GluonWBoxConstants.NEUTRONS_TOTAL_CIRCULATING_SUPPLY,
      GluonWBoxConstants.PROTONS_TOTAL_CIRCULATING_SUPPLY
    )
  ),
  treasuryMultisigRegister: SigmaPropRegister = new SigmaPropRegister(
    SigmaProp.createFromAddress(GetServiceConfig.getServiceOwner())
  ),
  feeRegister: LongPairRegister = new LongPairRegister(
    0L,
    GluonWBoxConstants.GLUONWBOX_MAX_FEE
  ),
  volumePlusRegister: NumbersRegister = new NumbersRegister(
    new Array[Long](BUCKETS)
  ),
  volumeMinusRegister: NumbersRegister = new NumbersRegister(
    new Array[Long](BUCKETS)
  ),
  lastDayBlockRegister: LongRegister = new LongRegister(0L),
  override val tokens: Seq[ErgoToken],
  override val id: ErgoId = ErgoId.create(""),
  override val box: Option[Box] = Option(null)
) extends BoxWrapperJson {

  override def getContract(implicit ctx: BlockchainContext): ErgoContract =
    GluonWBoxContract.getContract().contract.ergoContract

  def Neutrons: ErgoToken =
    tokens.filter(_.getId.equals(GluonWTokens.neutronId)).head

  def Protons: ErgoToken =
    tokens.filter(_.getId.equals(GluonWTokens.protonId)).head

  def DevFeeRepaid: Long = feeRegister.value._1

  def MaxFeeThreshold: Long = feeRegister.value._2

  def getProtonsPrice(oracleBox: OracleBox): AssetPrice = {
    val rErg: BigInt = BigInt(ergFissioned)
    val sProtons: BigInt = BigInt(protonsCirculatingSupply)
    val fusionRatio: BigInt = GluonWConstants().fusionRatio(
      neutronsCirculatingSupply,
      oracleBox.getPricePerGram,
      ergFissioned
    )

    val price: Long =
      ((GluonWBoxConstants.PRECISION - fusionRatio) * rErg / sProtons).toLong

    AssetPrice(
      name = GluonWAsset.PROTON.toString,
      price = price,
      id = GluonWTokens.protonId
    )
  }

  def getNeutronsPrice(oracleBox: OracleBox): AssetPrice = {
    val rErg: BigInt = BigInt(ergFissioned)
    val sNeutrons: BigInt = BigInt(neutronsCirculatingSupply)
    val fusionRatio: BigInt = GluonWConstants().fusionRatio(
      neutronsCirculatingSupply,
      oracleBox.getPricePerGram,
      ergFissioned
    )

    val price: Long = (fusionRatio * rErg / sNeutrons).toLong

    AssetPrice(
      name = GluonWAsset.NEUTRON.toString,
      price = price,
      id = GluonWTokens.neutronId
    )
  }

  def neutronsTotalSupply: Long = totalSupplyRegister.value._1
  def protonsTotalSupply: Long = totalSupplyRegister.value._2

  def neutronsCirculatingSupply: Long =
    neutronsTotalSupply - Neutrons.getValue

  def protonsCirculatingSupply: Long =
    protonsTotalSupply - Protons.getValue

  def ergFissioned: Long =
    value - GluonWBoxConstants.GLUONWBOX_BOX_EXISTENCE_FEE

  override def R4: Option[Register[_]] = Option(totalSupplyRegister)

  override def R5: Option[Register[_]] = Option(treasuryMultisigRegister)

  override def R6: Option[Register[_]] = Option(feeRegister)
  override def R7: Option[Register[_]] = Option(volumePlusRegister)
  override def R8: Option[Register[_]] = Option(volumeMinusRegister)
  override def R9: Option[Register[_]] = Option(lastDayBlockRegister)

  override def toJson(): Json =
    Json.fromFields(
      List(
        ("boxId", Json.fromString(this.id.toString)),
        (
          "assets",
          Json.fromFields(
            List(
              (
                "neutrons",
                Json.fromFields(
                  List(
                    ("tokenId", Json.fromString(Neutrons.getId.toString)),
                    (
                      "circulatingSupply",
                      Json.fromLong(neutronsCirculatingSupply)
                    )
                  )
                )
              ),
              (
                "protons",
                Json.fromFields(
                  List(
                    ("tokenId", Json.fromString(Protons.getId.toString)),
                    (
                      "circulatingSupply",
                      Json.fromLong(protonsCirculatingSupply)
                    )
                  )
                )
              )
            )
          )
        )
      )
    )
}

case class OracleBox(
  value: Long,
  priceRegister: LongRegister,
  epochIdRegister: IntRegister,
  override val tokens: Seq[ErgoToken],
  override val id: ErgoId = ErgoId.create(""),
  override val box: Option[Box] = Option(null)
) extends BoxWrapperJson {

  override def getContract(implicit ctx: BlockchainContext): ErgoContract =
    OracleConfig.get().address.toErgoContract

  def getPrice: Long = priceRegister.value
  def getPricePerGram: Long = priceRegister.value / 1000

  def getEpochId: Int = epochIdRegister.value

  override def R4: Option[Register[_]] = Option(priceRegister)

  override def R5: Option[Register[_]] = Option(epochIdRegister)

  override def toJson(): Json =
    Json.fromFields(
      List(
        ("name", Json.fromString(GluonWAsset.NEUTRON.toString)),
        ("tokenId", Json.fromString(tokens.tail.head.getId.toString)),
        ("epochId", Json.fromLong(getEpochId.toLong)),
        ("priceInNanoErgPerGram", Json.fromLong(getPricePerGram))
      )
    )
}

object OracleBox extends BoxWrapperHelper {

  override def from(inputBox: InputBox): OracleBox =
    OracleBox(
      value = inputBox.getValue,
      id = inputBox.getId,
      tokens = inputBox.getTokens.asScala.toSeq,
      box = Option(Box(inputBox)),
      priceRegister = new LongRegister(
        inputBox.getRegisters.get(0).getValue.asInstanceOf[Long]
      ),
      epochIdRegister = new IntRegister(
        inputBox.getRegisters.get(1).getValue.asInstanceOf[Int]
      )
    )

  // Json starts from items
  def from(json: Json): OracleBox = {
    val boxesJson: Seq[Json] = json.hcursor
      .downField("items")
      .as[Seq[Json]]
      .getOrElse(
        throw ParseException(
          "CiJson Parse Error at OracleBox.from"
        )
      )

    val id: String =
      boxesJson.head.hcursor.downField("boxId").as[String].getOrElse("")
    val value: Long =
      boxesJson.head.hcursor.downField("value").as[Long].getOrElse(0)
    val registers: Json = boxesJson.head.hcursor
      .downField("additionalRegisters")
      .as[Json]
      .getOrElse(null)
    val tokens: Option[Seq[ErgoToken]] =
      ErgoJson.getTokens(boxesJson.head)

    // Registers
    val r4Json: Option[Json] = Option(
      registers.hcursor
        .downField(Register.R4.toString)
        .as[Json]
        .getOrElse(null)
    )

    val r5Json: Option[Json] = Option(
      registers.hcursor
        .downField(Register.R5.toString)
        .as[Json]
        .getOrElse(null)
    )

    val r4: Option[Long] =
      Option(
        ErgoValue
          .fromHex(getRegisterValue(r4Json.get))
          .getValue
          .asInstanceOf[Long]
      )

    val r5: Option[Int] =
      Option(
        ErgoValue
          .fromHex(getRegisterValue(r5Json.get))
          .getValue
          .asInstanceOf[Int]
      )

    OracleBox(
      value = value,
      id = ErgoId.create(id),
      tokens = tokens.get,
      priceRegister = new LongRegister(
        r4.get
      ),
      epochIdRegister = new IntRegister(
        r5.get
      )
    )
  }

  private def getRegisterValue(registerJson: Json): String =
    registerJson.hcursor
      .downField("serializedValue")
      .as[String]
      .getOrElse("")
}

object GluonWBox extends BoxWrapperHelper {

  override def from(inputBox: InputBox): GluonWBox = {
//    val multiSigRegister: Array[Byte] =
//      inputBox.getRegisters
//        .get(1)
//        .getValue
//        .asInstanceOf[SigmaProp]

    val feeRegisterTuple: (Long, Long) = if (inputBox.getRegisters.size() > 2) {
      inputBox.getRegisters
        .get(2)
        .getValue
        .asInstanceOf[(Long, Long)]
    } else {
      (0, 0)
    }

    GluonWBox(
      value = inputBox.getValue,
      id = inputBox.getId,
      tokens = inputBox.getTokens.asScala.toSeq,
      box = Option(Box(inputBox)),
      totalSupplyRegister = new LongPairRegister(
        inputBox.getRegisters.get(0).getValue.asInstanceOf[(Long, Long)]
      ),
      treasuryMultisigRegister = new SigmaPropRegister(
        SigmaProp.createFromAddress(GetServiceConfig.getServiceOwner())
      ),
      feeRegister = new LongPairRegister(feeRegisterTuple),
      volumePlusRegister = new NumbersRegister(
        inputBox.getRegisters.get(3).getValue.asInstanceOf[Coll[Long]].toArray
      ),
      volumeMinusRegister = new NumbersRegister(
        inputBox.getRegisters.get(4).getValue.asInstanceOf[Coll[Long]].toArray
      ),
      lastDayBlockRegister = new LongRegister(
        inputBox.getRegisters.get(5).getValue.asInstanceOf[Long]
      )
    )
  }

  def create(
    neutronAmount: Long = GluonWBoxConstants.NEUTRONS_TOTAL_CIRCULATING_SUPPLY,
    protonAmount: Long = GluonWBoxConstants.PROTONS_TOTAL_CIRCULATING_SUPPLY,
    ergAmount: Long = GluonWBoxConstants.GLUONWBOX_BOX_EXISTENCE_FEE
  ): GluonWBox =
    GluonWBox(
      value = ergAmount,
      tokens = Seq(
        ErgoToken(GluonWTokens.gluonWBoxNFTId, 1),
        ErgoToken(GluonWTokens.neutronId, neutronAmount),
        ErgoToken(GluonWTokens.protonId, protonAmount)
      )
    )
}

object BoxHelper {

  def getIdFromJson(json: Json) = {
    val boxesJson: Seq[Json] = json.hcursor
      .downField("items")
      .as[Seq[Json]]
      .getOrElse(
        throw ParseException(
          "CiJson Parse Error at OracleBox.from"
        )
      )

    val id: String =
      boxesJson.head.hcursor.downField("boxId").as[String].getOrElse("")

    id
  }

}
