package gluonw.tools

import commons.configs.GetServiceConfig.getServiceOwner
import commons.configs.NodeConfig
import commons.node.{Client, TestClient}
import edge.boxes.FundsToAddressBox
import edge.node.BaseClient
import edge.registers.LongRegister
import edge.tools.BoxTools
import edge.tools.BoxTools.{mergeBox, mintTokens}
import gluonw.common.{GluonWBoxExplorer, GluonWTokens}
import org.ergoplatform.appkit.{
  Address,
  BlockchainContext,
  InputBox,
  NetworkType,
  Parameters,
  SignedTransaction
}
import org.ergoplatform.appkit.config.{ErgoNodeConfig, ErgoToolConfig}
import gluonw.boxes.{GluonWBox, GluonWBoxConstants, OracleBox}
import org.ergoplatform.sdk.{ErgoToken, SecretString}

import scala.collection.JavaConverters.collectionAsScalaIterableConverter

object BoxCreation extends App {

  val isMainNet: Boolean = true
  val configFileName = "ergo_config.json"
  val testNetConfigFileName = "ergo_config_testnet.json"

  val conf: ErgoToolConfig = if (isMainNet) {
    ErgoToolConfig.load(configFileName)
  } else {
    ErgoToolConfig.load(testNetConfigFileName)
  }
  val nodeConf: ErgoNodeConfig = conf.getNode
  val client: Client = new Client()

  val explorer: GluonWBoxExplorer = new GluonWBoxExplorer()(client)
  val reducedTxBytes: Seq[String] = Seq(
    ""
  )

  client.setClient()

  val tokens: Seq[(String, (String, Long))] = Seq(
    (
      "Gluon Dollar Reactor NFT",
      (
        "NFT uniquely identifying the Gluon Dollar reactor.",
        1L
      )
    ),
    (
      "USDG",
      (
        "Gluon Dollar",
        GluonWBoxConstants.TOTAL_CIRCULATING_SUPPLY
      )
    ),
    (
      "USDGYL",
      (
        "Gluon Dollar Leverage Yield",
        GluonWBoxConstants.TOTAL_CIRCULATING_SUPPLY
      )
    )
  )

  val txJson: Seq[Unit] = client.getClient.execute { (ctx: BlockchainContext) =>
    val SIGN_REDUCED: String = "signReduced"
    val MINT: String = "mint"
    val MUTATE: String = "mutate"
    val MERGE: String = "merge"
    val BURN: String = "burn"

    // SET RUN TX HERE
    val runTx: String = MERGE

    System.out.println(s"Running $runTx tx")
    val totalSupply: Long = GluonWBoxConstants.TOTAL_CIRCULATING_SUPPLY

    val signedTxs: Seq[SignedTransaction] = runTx match {
      case MINT => {
        val neutronsMintTx = mintTokens(
          tokens(1)._1,
          tokens(1)._2._1,
          tokens(1)._2._2,
          decimals = GluonWBoxConstants.DECIMALS.toInt
        )(client, conf, nodeConf)
        val protonsMintTx = mintTokens(
          tokens(2)._1,
          tokens(2)._2._1,
          tokens(2)._2._2,
          decimals = GluonWBoxConstants.DECIMALS.toInt
        )(client, conf, nodeConf)
        val gluonWNFTMintTx = mintTokens(
          tokens.head._1,
          tokens.head._2._1,
          tokens.head._2._2
        )(client, conf, nodeConf)

        protonsMintTx

      }
      case MERGE => {
        
        val nftToken = ErgoToken(GluonWTokens.gluonWBoxNFTId, 1)
        val gluonNeutronToken = ErgoToken(GluonWTokens.neutronId, totalSupply)
        val gluonProtonToken =
          ErgoToken(GluonWTokens.protonId, totalSupply)
        val gluonWBox: GluonWBox = GluonWBox(
          value =
            GluonWBoxConstants.INITIAL_RESERVE + GluonWBoxConstants.GLUONWBOX_BOX_EXISTENCE_FEE,
          tokens = Seq(
            ErgoToken(GluonWTokens.gluonWBoxNFTId, 1),
            ErgoToken(
              GluonWTokens.neutronId,
              totalSupply - GluonWBoxConstants.INITIAL_NEUTRON_CIRCULATING_SUPPLY
            ),
            ErgoToken(
              GluonWTokens.protonId,
              totalSupply - GluonWBoxConstants.INITIAL_PROTON_CIRCULATING_SUPPLY
            )
          ),
          lastDayBlockRegister = new LongRegister(client.getHeight)
        )

        mergeBox(
          Seq(nftToken, gluonNeutronToken, gluonProtonToken),
          Seq(gluonWBox)
        )(client, conf, nodeConf)
      }
      case MUTATE => {
        // mainnet: e6f7fd7a0ecf9c33bd95f23e747865670ef6e5ff430f925fb46ffbecc4ab8508
        val boxIdToMutate: String =
          "2c89f574973abb6ba0b2f5b197e8a97fc1fdcff257316695750058d2437a37c7"
        val gluonWBox: InputBox = ctx.getBoxesById(boxIdToMutate).head
        val mutatedGluonWBox: GluonWBox = GluonWBox.from(gluonWBox)
//        val mutatedGluonWBox: FundsToAddressBox = FundsToAddressBox.from(gluonWBox).copy(address = getServiceOwner(isMainNet = NodeConfig.networkType == NetworkType.MAINNET))
        val ownerAddress: Address = Address.createEip3Address(
          0,
          nodeConf.getNetworkType,
          SecretString.create(nodeConf.getWallet.getMnemonic),
          SecretString.create(""),
          false
        )
        // get some fee for spending
        val spendingBoxes =
          ctx.getDataSource
            .getUnspentBoxesFor(ownerAddress, 0, 500)
            .asScala
            .toSeq
            .filter(_.getTokens.isEmpty)
        val oracleBox: OracleBox = explorer.getOracleBox

        BoxTools.mutate(
          boxIdToMutate = boxIdToMutate,
          Seq(mutatedGluonWBox),
            dataInputs = Seq(oracleBox.box.get.input),
          inputBoxes = spendingBoxes
        )(
          client,
          conf,
          nodeConf
        )
      }
      case SIGN_REDUCED => {
        BoxTools.signReducedTx(
          reducedTxBytes
        )(client, conf, nodeConf)
      }
      case "consolidate" => {
        val ergValue =
          2 * Parameters.OneErg + GluonWBoxConstants.GLUONWBOX_BOX_EXISTENCE_FEE
        val tokens = Seq(
          ErgoToken(GluonWTokens.gluonWBoxNFTId, 1),
          ErgoToken(
            GluonWTokens.neutronId,
            totalSupply - (GluonWBoxConstants.PRECISION / 100)
          ),
          ErgoToken(
            GluonWTokens.protonId,
            totalSupply - (GluonWBoxConstants.PRECISION / 100)
          )
        )

        BoxTools.consolidateBoxes(ergValue = ergValue, tokens = tokens)(
          client,
          conf,
          nodeConf
        )
      }
      case BURN => {
        val tokens = Seq(
          ErgoToken(
            GluonWTokens.neutronId,
            (GluonWBoxConstants.PRECISION / 100)
          ),
          ErgoToken(
            GluonWTokens.protonId,
            (GluonWBoxConstants.PRECISION / 100)
          )
        )

        BoxTools.burnTokens(tokens)(client, conf, nodeConf)
      }
    }

    signedTxs.map { signedTx =>
      ctx.sendTransaction(signedTx)
      val jsonVal = signedTx.toJson(true)
      System.out.println(jsonVal)
    }
  }

  System.out.println("Completed Transaction")
}
