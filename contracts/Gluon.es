{
    // ===== Contract Info ===== //
    // Name             : Gluon Reactor
    // Type             : Guard Script
    // Author           : Kii, LGD, Sanjam, Zahnentferner
    // Last Modified    : 2026-10-11
    // Version          : v 3.0

    // ===== Version Logs ===== //
    // - 1.0: Basic Gluon without dev fees and UI fees
    // - 1.1: Dev fees and UI fees implemented
    // - 2.0: Oracle check enforced
    // - 3.0: Peg adjustment implemented

    // ===== Contract Hard-Coded Constants ===== //
    // val _MinFee:                     Long
    // val _OracleFeePk:                Coll[Byte]
    // val _OraclePoolNFT:              Coll[Byte]
    // val _OracleBuybackNFT:           Coll[Byte]

    // ===== Box Contents ===== //
    // Tokens
    // 1. (GluonNFT, 1)
    // 2. (Neutrons, Long)
    // 3. (Protons, Long)
    //
    // Registers
    // R4 - (Total Neutrons Supply, Total Protons Supply): (Long, Long)
    // R5 - TreasuryMultisig: SigmaProp
    // R6 - (TotalDevFeesPaid, MaxAmountDevFeesPaid): (Long, Long)
    // R7 - BetaPlusVolume: Coll[Long]
    // R8 - BetaMinusVolume: Coll[Long]
    // R9 - (LastBucketBlock, PegFactor): (Long, Long)
    //      PegFactor (scaled by one = 1e9) converts the oracle price into the target price of one neutron.
    //      Its initial value also fixes the unit: e.g. 0.001 (1000000) for an oracle quoting nanoERG per kg
    //      of gold pegs one neutron to one gram of gold; 1 (1000000000) for an oracle quoting nanoERG per USD
    //      pegs one neutron to one USD.

    // ===== Context Vars ===== //
    // val _optUIFeeAddress                    SigmaProp

    // ===== Transaction Types ===== //
    // 1. Fission                   - The user sends Ergs to the reactor (bank) and receives Neutrons and Protons
    // 2. Fusion                    - The user sends Neutrons and Protons to the reactor and receives Ergs
    // 3. Beta Decay +              - The user sends Protons to the reactor and receives Neutrons
    // 4. Beta Decay -              - The user sends Neutrons to the reactor and receives Protons
    // 5. Adjust Peg Factor         - Any user readjusts the peg if the fusion ratio is outside the bounds of the healthy range.
    // 6. Update Treasury Multisig  - The current treasury multisig is used to create a new box containing the updated treasury multisig address.

    // For all of the first five transaction types:
    // Inputs: GluonBox, UserPk
    // DataInputs: Oracle
    // Outputs: GluonBox, UserPk

    // For the sixth transaction type:
    // Inputs: GluonBox, MultisigUtxo
    // DataInputs: Oracle (unused by this transaction type, but required because the script reads
    //             CONTEXT.dataInputs(0), its R4 and its first token for every transaction type)
    // Outputs: GluonBox

    // # Box Definitions
    val GLUON_BOX: Box = SELF
    val OUT_GLUON_BOX: Box = OUTPUTS(0)
    val ORACLE_BOX: Box = CONTEXT.dataInputs(0)

    // # Register Definitions
    val NEUTRONS_TOKEN: (Coll[Byte], Long) = GLUON_BOX.tokens(1)
    val PROTONS_TOKEN: (Coll[Byte], Long) = GLUON_BOX.tokens(2)
    val OUT_NEUTRONS_TOKEN: (Coll[Byte], Long) = OUT_GLUON_BOX.tokens(1)
    val OUT_PROTONS_TOKEN: (Coll[Byte], Long) = OUT_GLUON_BOX.tokens(2)
    
    val NEUTRONS_TOTAL_SUPPLY: Long = GLUON_BOX.R4[(Long, Long)].get._1
    val PROTONS_TOTAL_SUPPLY: Long = GLUON_BOX.R4[(Long, Long)].get._2

    val TREASURY_MULTISIG: SigmaProp = SELF.R5[SigmaProp].get

    val ASSET_MAX_DEV_FEE_THRESHOLD: (Long, Long) = GLUON_BOX.R6[(Long, Long)].get
    val OUT_ASSET_MAX_DEV_FEE_THRESHOLD: (Long, Long) = OUT_GLUON_BOX.R6[(Long, Long)].get    
    val DEV_FEE_REPAID: Long = ASSET_MAX_DEV_FEE_THRESHOLD._1 
    val MAX_DEV_FEE_THRESHOLD: Long = ASSET_MAX_DEV_FEE_THRESHOLD._2
    val OUT_DEV_FEE_REPAID: Long = OUT_ASSET_MAX_DEV_FEE_THRESHOLD._1
    val OUT_MAX_DEV_FEE_THRESHOLD: Long = OUT_ASSET_MAX_DEV_FEE_THRESHOLD._2

    val VOLUME_PLUS: Coll[Long] = GLUON_BOX.R7[Coll[Long]].get
    val OUT_VOLUME_PLUS: Coll[Long] = OUT_GLUON_BOX.R7[Coll[Long]].get
    val VOLUME_MINUS: Coll[Long] = GLUON_BOX.R8[Coll[Long]].get
    val OUT_VOLUME_MINUS: Coll[Long] = OUT_GLUON_BOX.R8[Coll[Long]].get

    val REGISTER_9: (Long, Long) = GLUON_BOX.R9[(Long, Long)].get
    val OUT_REGISTER_9: (Long, Long) = OUT_GLUON_BOX.R9[(Long, Long)].get
    val LAST_BUCKET_BLOCK: Long = REGISTER_9._1
    val OUT_LAST_BUCKET_BLOCK: Long = OUT_REGISTER_9._1
    val PEG_FACTOR: Long = REGISTER_9._2
    val OUT_PEG_FACTOR: Long = OUT_REGISTER_9._2

    // # Constants
    val one: BigInt = (1000000000).toBigInt // one is 1,000,000,000 because we are using 9 decimal digits.

    // # Parameters
    val qStar: BigInt           = (99 * one / 100) // q* = 99%
    val qUpperThreshold: BigInt = (98 * one / 100) // qUpper = 98%
    val qLowerThreshold: BigInt = one / 2          // qLower = 50%
    val phiFission: BigInt = (one / 1000).toBigInt // fission fee = 0.1%
    val phiFusion: BigInt  = (one / 200).toBigInt  // fusion fee  = 0.5%
    val phi0 = one / 200 // BetaDecay Fee y-intercept: phi0 = 0.5%
    val phi1 = one       // BetaDecay Fee slope:       phi1 = 1
    val blocksPerVolumeBucket: Int = 720 // Approximately 1 day per volume bucket
    val buckets: Int = 14                // Tracking volume for approximately 14 days
    val feeDenom: BigInt = 1000L.toBigInt
    val initialDevFee: BigInt = 5L.toBigInt    // Initial dev fee: 0.5%
    val oracleFee: BigInt = 1L.toBigInt // Oracle fee: 0.1%
    val uiFee: BigInt = 4L.toBigInt     // Optional UI fee: 0.4%

    // # Internal State Variables
    val supplyNeutrons: BigInt = (NEUTRONS_TOTAL_SUPPLY - NEUTRONS_TOKEN._2).toBigInt // Variable in Paper: S_neutrons
    val supplyProtons: BigInt  = (PROTONS_TOTAL_SUPPLY - PROTONS_TOKEN._2).toBigInt   // Variable in Paper: S_protons
    val reserve: BigInt        = (GLUON_BOX.value - _MinFee).toBigInt // Variable in Paper: R
    val outSupplyNeutrons: BigInt = (NEUTRONS_TOTAL_SUPPLY - OUT_NEUTRONS_TOKEN._2).toBigInt
    val outReserve: BigInt        = (OUT_GLUON_BOX.value - _MinFee).toBigInt

    // # External State Variables
    val price: BigInt = ORACLE_BOX.R4[Long].get.toBigInt  // Oracle price, in nanoERG per unit of the oracle's asset

    // # State Dependent Variables
    val priceAdjusted: BigInt     = price * PEG_FACTOR.toBigInt / one // Target price of one neutron: P_adjusted = price * pegFactor / one
    val q: BigInt = supplyNeutrons * priceAdjusted / reserve  // fusion ratio
    val qOut: BigInt = outSupplyNeutrons * priceAdjusted / outReserve 
    val qNorm: BigInt = min(one * q / (q + one - qStar), q)   // normalized fusion ratio
    val isHealthy: Boolean    = (q >= qLowerThreshold) && (q <= qUpperThreshold) // Fusion, fission and beta decays only permitted when 0.50 <= q <= 0.98
    val isHealthyAfter: Boolean   = (qOut >= qLowerThreshold) && (qOut <= qUpperThreshold) // Fusion, fission and beta decays only permitted when 0.50 <= q <= 0.98

    // # Transaction Type Definitions //

    // Note: Circulating Supply = TotalSupply - AmountInBox
    // Therefore an increase/decrease in circulating supply means AmountInBox decreases/increases

    // ## Fission: Splits ERG into protons and neutrons (mints protons and neutrons)
    val isFissionTx: Boolean = allOf(Coll(
        NEUTRONS_TOKEN._2 > OUT_NEUTRONS_TOKEN._2, // Neutrons decrease (circulating supply increases)
        PROTONS_TOKEN._2 > OUT_PROTONS_TOKEN._2,   // Protons decrease  (circulating supply increases)
        GLUON_BOX.value < OUT_GLUON_BOX.value      // ERG value increases
    ))

    // ## Fusion: Merges protons and neutrons into ERG (redeems protons and neutrons)
    val isFusionTx: Boolean = allOf(Coll(
        NEUTRONS_TOKEN._2 < OUT_NEUTRONS_TOKEN._2, // Neutrons increase (circulating supply decreases)
        PROTONS_TOKEN._2 < OUT_PROTONS_TOKEN._2,   // Protons increase  (circulating supply decreases)
        GLUON_BOX.value > OUT_GLUON_BOX.value      // ERG value decreases
    ))

    // ## BetaDecayPlus: Transmutes Protons to Neutrons
    // Decreases protons in circulation and increases neutrons in circulation
    val isBetaDecayPlusTx: Boolean = allOf(Coll(
        NEUTRONS_TOKEN._2 > OUT_NEUTRONS_TOKEN._2, // Neutrons decrease
        PROTONS_TOKEN._2 < OUT_PROTONS_TOKEN._2,   // Protons increase
        GLUON_BOX.value == OUT_GLUON_BOX.value     // ERG value is preserved
    ))

    // ## BetaDecayMinus: Transmutes Neutrons to Protons
    // Decreases neutrons in circulation and increases protons in circulation
    val isBetaDecayMinusTx: Boolean = allOf(Coll(
        NEUTRONS_TOKEN._2 < OUT_NEUTRONS_TOKEN._2, // Neutrons increase
        PROTONS_TOKEN._2 > OUT_PROTONS_TOKEN._2,   // Protons decrease
        GLUON_BOX.value == OUT_GLUON_BOX.value     // ERG value is preserved
    ))

    // ## AdjustPeg: Changes the peg factor that determines the peg
    val isAdjustPegTx: Boolean = (PEG_FACTOR != OUT_PEG_FACTOR)

    // ## UpdateTreasury: Changes the address that receives dev fees
    val isUpdateTreasury: Boolean = (INPUTS(1).propositionBytes == TREASURY_MULTISIG.propBytes)


    // # Preservation Checks: When a transaction does not change something, we must explicitly check that it remained unchanged
    val cSameContract: Boolean = GLUON_BOX.propositionBytes == OUT_GLUON_BOX.propositionBytes
    val cSameTokens: Boolean   = GLUON_BOX.tokens == OUT_GLUON_BOX.tokens
    val cSameTokenIdentifiers: Boolean = GLUON_BOX.tokens(0)._1 == OUT_GLUON_BOX.tokens(0)._1 && // For fission, fusion and beta decays,
                                         GLUON_BOX.tokens(1)._1 == OUT_GLUON_BOX.tokens(1)._1 && // token amounts may change,
                                         GLUON_BOX.tokens(2)._1 == OUT_GLUON_BOX.tokens(2)._1    // but the token identifiers must be preserved
    val cSameValue: Boolean    = GLUON_BOX.value  == OUT_GLUON_BOX.value
    val cSameR4: Boolean       = GLUON_BOX.R4[(Long,Long)].get == OUT_GLUON_BOX.R4[(Long,Long)].get  // Total Neutron and Proton Suplies preserved
    val cSameR5: Boolean       = GLUON_BOX.R5[SigmaProp].get  == OUT_GLUON_BOX.R5[SigmaProp].get     // Treasury Multisig preserved    
    val cSameR6: Boolean       = GLUON_BOX.R6[(Long,Long)].get == OUT_GLUON_BOX.R6[(Long,Long)].get  // Dev fees accounting preserved
    val cSameR7: Boolean       = GLUON_BOX.R7[Coll[Long]].get == OUT_GLUON_BOX.R7[Coll[Long]].get    // BetaDecayPlus volume preserved
    val cSameR8: Boolean       = GLUON_BOX.R8[Coll[Long]].get == OUT_GLUON_BOX.R8[Coll[Long]].get    // BetaDecayMinus volume preserved
    val cSameR9: Boolean       = GLUON_BOX.R9[(Long, Long)].get == OUT_GLUON_BOX.R9[(Long, Long)].get // LastBucketBlock and PegFactor preserved
    val cSameR9LastBucketBlock: Boolean = LAST_BUCKET_BLOCK == OUT_LAST_BUCKET_BLOCK // LastBucketBlock preserved
    val cSameR9PegFactor: Boolean = PEG_FACTOR == OUT_PEG_FACTOR   // PegFactor preserved

    // # Oracle Checks
    val oracleDelay: Int = CONTEXT.HEIGHT - ORACLE_BOX.creationInfo._1 // Difference between now and the time when the oracle box was created, in blocks.
    val cOracle: Boolean = allOf(Coll(
        oracleDelay < 35 && oracleDelay >= 0,     // Oracle delay is at most 34 blocks (~70 min) in the past
        ORACLE_BOX.tokens(0)._1 == _OraclePoolNFT // The oracle NFT is the right NFT
    ))

    // # Auxiliary Functions 
    def valueOfProtons(protonsAmount: Long): BigInt = { // value in nanoERG
        val protonsPrice: BigInt = (one - qNorm) * reserve / supplyProtons
        protonsAmount.toBigInt * protonsPrice / one
    }
    def valueOfNeutrons(neutronsAmount: Long): BigInt = { // value in nanoERG
        val neutronPrice: BigInt = (qNorm * reserve) / supplyNeutrons
        neutronsAmount.toBigInt * neutronPrice / one
    }

    // # Basic Math Functions
    def sum(collLong: Coll[Long]): BigInt = collLong.fold(0L, {(acc: Long, indexedValue: Long) => acc + indexedValue}).toBigInt

    // # Transaction Validity Conditions
    if (anyOf(Coll(isFissionTx, isFusionTx, isBetaDecayPlusTx, isBetaDecayMinusTx))) {
        // ## General Fee Checks
        val principal: BigInt = // the value of the amount transacted, measured in nanoERG
            if (isFissionTx) (OUT_GLUON_BOX.value - GLUON_BOX.value).toBigInt 
            else if (isFusionTx) (GLUON_BOX.value - OUT_GLUON_BOX.value).toBigInt
            else if (isBetaDecayPlusTx) valueOfProtons(OUT_PROTONS_TOKEN._2 - PROTONS_TOKEN._2)
            else valueOfNeutrons(OUT_NEUTRONS_TOKEN._2 - NEUTRONS_TOKEN._2)

        val oracleFeePayout: BigInt = (oracleFee * principal) / feeDenom
        val oracleFeesToBePaid: Boolean = (isBetaDecayPlusTx || isBetaDecayMinusTx) && oracleFeePayout > 0
        val oracleFeesPaid: Boolean = {
            val oracleOutput: Box = OUTPUTS(2)
            if (oracleFeesToBePaid) {
                val oracleBuybackInputBox: Box = INPUTS(INPUTS.size - 1) // The oracle buy back input box is always the last input
                allOf(Coll(
                    oracleOutput.propositionBytes == _OracleFeePk,
                    oracleOutput.propositionBytes == oracleBuybackInputBox.propositionBytes,
                    oracleOutput.tokens(0)._1     == _OracleBuybackNFT,
                    oracleOutput.value.toBigInt   == oracleBuybackInputBox.value.toBigInt + oracleFeePayout + _MinFee
                ))
            } else true // if oracle fee does not need to be paid, then default to true.
        }

        val devFeePayout: BigInt = if (DEV_FEE_REPAID < MAX_DEV_FEE_THRESHOLD) { // Decreases linearly from initialDevFee to zero
            ((initialDevFee * principal) / feeDenom) * (MAX_DEV_FEE_THRESHOLD - DEV_FEE_REPAID) / MAX_DEV_FEE_THRESHOLD
        } else 0L.toBigInt    
        val devFeesToBePaid: Boolean = devFeePayout > 0
        val devFeesPaid: Boolean = {
            if (devFeesToBePaid) {
                val devOutput: Box = if (!oracleFeesToBePaid) { OUTPUTS(2) } else { OUTPUTS(3) } // If there is a need to pay oracle fees, we check OUTPUTS(3)
                allOf(Coll(
                    devOutput.propositionBytes == TREASURY_MULTISIG.propBytes,
                    devOutput.value.toBigInt   == devFeePayout + _MinFee
                ))
            } else true // if dev fee does not need to be paid, then default to true.
        }
        val devFeeRepaidValueAdded: Boolean = (OUT_DEV_FEE_REPAID - DEV_FEE_REPAID) == devFeePayout
        val maxDevFeeThresholdSame: Boolean = OUT_MAX_DEV_FEE_THRESHOLD == MAX_DEV_FEE_THRESHOLD

        val uiFeePayout: BigInt = (uiFee * principal) / feeDenom
        val _optUIFeeAddress = getVar[SigmaProp](0)
        val uiFeesPaid: Boolean = {
            if (_optUIFeeAddress.isDefined && uiFeePayout > 0) {
                val uiOutput: Box = if (oracleFeesToBePaid && devFeesToBePaid) OUTPUTS(4) 
                                    else if (oracleFeesToBePaid || devFeesToBePaid) OUTPUTS(3)
                                    else OUTPUTS(2)
                allOf(Coll(
                    uiOutput.propositionBytes == _optUIFeeAddress.get.propBytes,
                    uiOutput.value.toBigInt   == uiFeePayout + _MinFee
                ))
            } else true // if ui fee does not need to be paid, then default to true.
        }

        val cFees: Boolean = allOf(Coll(
            oracleFeesPaid, devFeesPaid, uiFeesPaid,
            devFeeRepaidValueAdded,
            maxDevFeeThresholdSame
        ))
        

        // ## Transaction-Specific Checks
        if (isFissionTx) {
            // Equation: M [Ergs] ==> (M (1 - phiFission) (S Protons / R)) [Protons] + (M (1 - phiFission) (S Neutrons / R)) [Neutrons]
            val M: BigInt = (OUT_GLUON_BOX.value - GLUON_BOX.value).toBigInt 

            val NeutronsActualValue: BigInt = (NEUTRONS_TOKEN._2 - OUT_NEUTRONS_TOKEN._2).toBigInt
            val ProtonsActualValue: BigInt = (PROTONS_TOKEN._2 - OUT_PROTONS_TOKEN._2).toBigInt

            val NeutronsExpectedValue: BigInt = (M * supplyNeutrons * (one - phiFission) / reserve) / one
            val ProtonsExpectedValue: BigInt = (M * supplyProtons * (one - phiFission) / reserve) / one

            val __outNeutronsValueValid: Boolean = NeutronsActualValue == NeutronsExpectedValue
            val __outProtonsValueValid: Boolean = ProtonsActualValue == ProtonsExpectedValue

            sigmaProp(allOf(Coll(
                isHealthy, // isHealthyAfter is not checked: the fee left in the reserve lowers q, but by at most a factor of (1 - phiFission)
                cSameContract, cSameTokenIdentifiers,
                __outNeutronsValueValid, __outProtonsValueValid,
                cSameR4, cSameR5, cSameR7, cSameR8, cSameR9,
                cFees, // cFees also checks that R6._2 remains the same
                cOracle
            )))
        }
        else if (isFusionTx) {
            // Equation: (M (S neutrons / R)) [Protons] + (M (S protons / R)) [Neutrons] ==> M (1 - phiFusion) [Ergs]

            // The protons and neutrons are more in outbox than inputbox
            val NeutronsActualValue: BigInt = (OUT_NEUTRONS_TOKEN._2 - NEUTRONS_TOKEN._2).toBigInt
            val ProtonsActualValue: BigInt = (OUT_PROTONS_TOKEN._2 - PROTONS_TOKEN._2).toBigInt

            // M = Ergs
            val M: BigInt = (GLUON_BOX.value - OUT_GLUON_BOX.value).toBigInt

            val inProtonsNumerator: BigInt = M * supplyProtons * one
            val inNeutronsNumerator: BigInt = M * supplyNeutrons * one
            val denominator: BigInt = reserve * (one - phiFusion)

            val NeutronsExpectedValue: BigInt = inNeutronsNumerator / denominator
            val ProtonsExpectedValue: BigInt =  inProtonsNumerator / denominator

            val __inNeutronsValueValid: Boolean = NeutronsActualValue == NeutronsExpectedValue
            val __inProtonsValueValid: Boolean = ProtonsActualValue == ProtonsExpectedValue

            sigmaProp(allOf(Coll(
                // isHealthyAfter is deliberately not checked. The fee left in the reserve lowers q, by a lot for large
                // fusions, so a fusion may take q below qLowerThreshold, after which anyone may raise the peg factor.
                // Checking it would block every fusion while q sits at qLowerThreshold, and the last one ever (q = 0).
                isHealthy,
                cSameContract, cSameTokenIdentifiers,  
                __inNeutronsValueValid, __inProtonsValueValid,
                cSameR4, cSameR5, cSameR7, cSameR8, cSameR9,
                cFees, // cFees also checks that R6._2 remains the same
                cOracle
            )))
        }
        else if (isBetaDecayPlusTx) {
            // Equation: M [Protons] ==> M * (1 - phiBeta(T)) * ((1 - q(R, S neutron)) / q(R, S neutron)) * (S neutrons / S protons) [Neutrons]
            val M: Long = (OUT_PROTONS_TOKEN._2 - PROTONS_TOKEN._2) // Number of protons being decayed.
            val worthOfMInErgs: BigInt = valueOfProtons(M) // This is the volume of protons in units of ERG

            // Calculate the amount of days that has been since the last betaDecayTx
            // 1000 - 200 = 800 | 800 / 720 = 1
            val currentBlockNumber: Int = CONTEXT.HEIGHT
            val getNDaysPreFilteredValue: Int = ((currentBlockNumber - LAST_BUCKET_BLOCK) / blocksPerVolumeBucket).toInt
            val nDays: Int = if (getNDaysPreFilteredValue >= buckets) {buckets} else getNDaysPreFilteredValue

            // We don't need to shift it, we just need to check if OUT_VOLUME_PLUS is correct.
            // Therefore, if there is a requirement to shift, we just need to check if the value after n is the same for the next 14.
            //
            // Here's an example:
            // assuming our initial block is this
            // [1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14]
            //
            // If nDays = 4, and worthOfMInErgs = x
            // We should expect:
            // [x, 0, 0 ,0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11]
            //
            // If nDays = 0, and worthOfMInErgs = x
            // [1 + x, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14]
            //
            // The conditions are:
            // 1. The value in OUT_VOLUME_PLUS(0) should be worthOfMInErgs + if (n == 0) VOLUME_PLUS(0) else 0
            // 2. For the block after 0, if n > 1, then n - 1 of the blocks after should be 0.
            // 3. The rest of the value, OUT_VOLUME_PLUS(x) [where x = n up to 14 - n] should be
            //      equal to VOLUME_PLUS(y) [where y = 0 up to n]
            // 4. The volume should be 14.
            // The same conditions go for OUT_VOLUME_MINUS, other than #1 whereby, it is replaced
            // by OUT_VOLUME_MINUS(0) == if (nDays == 0) {VOLUME_MINUS(0)} else {0L}

            // #1
            val outVolumePlusExpectedValue = (if (nDays == 0) {VOLUME_PLUS(0)} else {0L}) + worthOfMInErgs
            val _volumePlusAccounted = OUT_VOLUME_PLUS(0) == outVolumePlusExpectedValue

            // #2: We sliced n - 1 of the value in between, and we check if all of it are 0s.
            val slicedNVolumePlus: Coll[Long] = OUT_VOLUME_PLUS.slice(1, nDays)
            val _nVolumePlusAllZeros: Boolean = slicedNVolumePlus.forall{(indexedValue: Long) => indexedValue == 0L}

            // #3: If we slice the correct pieces from in and out, we should get the same exact value
            val slicedOutVolumePlus: Coll[Long] = OUT_VOLUME_PLUS.slice(nDays, buckets)
            val slicedInVolumePlus: Coll[Long] = VOLUME_PLUS.slice(0, buckets - nDays)
            val _isSlicedValuedVolumePlusEqual: Boolean = if (nDays > 0) {
                // When there are multiple days involved, we have to compare the days that are pushed towards 
                // the right in OUT_VOLUME_MINUS, this starts at nDays and end at the last index.
                // For VOLUME_MINUS, it would be the first till buckets - nDays
                slicedOutVolumePlus == slicedInVolumePlus
            } else { // When the days are the same, we compare 1 - buckets because only the first index changed.
                OUT_VOLUME_PLUS.slice(1, buckets) == VOLUME_PLUS.slice(1, buckets)
            }

            val __outVolumePlusValidated: Boolean = allOf(Coll(
                OUT_VOLUME_PLUS.size == buckets,
                _volumePlusAccounted,
                _isSlicedValuedVolumePlusEqual,
                _nVolumePlusAllZeros
            ))

            // #1
            val outVolumeMinusExpectedValue = if (nDays == 0) {VOLUME_MINUS(0)} else {0L}
            val _outVolumeMinusFirstIndexedPreserved = outVolumeMinusExpectedValue == OUT_VOLUME_MINUS(0)

            // #2: We sliced n - 1 of the value in between, and we check if all of it are 0s.
            val slicedNVolumeMinus: Coll[Long] = OUT_VOLUME_MINUS.slice(1, nDays)
            val _nVolumeMinusAllZeros: Boolean = slicedNVolumeMinus.forall{(indexedValue: Long) => indexedValue == 0L}

            // #3
            val slicedOutVolumeMinus: Coll[Long] = OUT_VOLUME_MINUS.slice(nDays, buckets)
            val slicedInVolumeMinus: Coll[Long] = VOLUME_MINUS.slice(0, buckets - nDays)
            val _isSlicedValuedVolumeMinusEqual: Boolean = if (nDays > 0) {
                // When there are multiple days involved, we have to compare the days that are pushed towards
                // the right in OUT_VOLUME_MINUS, this starts at nDays and end at the last index.
                // For VOLUME_MINUS, it would be the first till buckets - nDays.
                slicedOutVolumeMinus == slicedInVolumeMinus
            } else { // When the days are the same, we compare 1 - buckets because only the first index changed.
                OUT_VOLUME_MINUS.slice(1, buckets) == VOLUME_MINUS.slice(1, buckets)
            }

            val __outVolumeMinusValidated: Boolean = allOf(Coll(
                OUT_VOLUME_MINUS.size == buckets,
                _outVolumeMinusFirstIndexedPreserved,
                _isSlicedValuedVolumeMinusEqual,
                _nVolumeMinusAllZeros
            ))

            val volumePlus: BigInt = sum(OUT_VOLUME_PLUS) // adds all elements of the collection, computing the total volume
            val volumeMinus: BigInt = sum(OUT_VOLUME_MINUS)

            val volume: BigInt = if (volumeMinus > volumePlus) {0L.toBigInt} else {volumePlus - volumeMinus} // integer subtraction

            val VarPhiBeta: BigInt = phi0 + ((phi1 * volume) / reserve) // This fee remains in the reserve

            // Due to some issues with moving towards the next block. We should give it a margin of error of +/- 3 blocks.
            // There is a tricky situation where if the lastblock is within a day, and if it is always updated,
            // then we will always be at day 0 as long as there is a decay that happened within a day before
            // the lastBlockPreserved.
            //
            // To counteract this situation, we want to only get the currentBlockNumber that is closest to the previous blocksPerVolumeBucket.
            val closestPreviousBlockValueViaBuckets: Int = (currentBlockNumber / blocksPerVolumeBucket) * blocksPerVolumeBucket
            val __lastBlockPreserved: Boolean = OUT_LAST_BUCKET_BLOCK == closestPreviousBlockValueViaBuckets

            // The protons increase in output, neutrons decrease in outputs
            val NeutronsActualValue: BigInt = (NEUTRONS_TOKEN._2 - OUT_NEUTRONS_TOKEN._2).toBigInt
            val ProtonsActualValue: BigInt = (OUT_PROTONS_TOKEN._2 - PROTONS_TOKEN._2).toBigInt
            val ErgsActualValue: BigInt = (OUT_GLUON_BOX.value).toBigInt

            // The steps of multiplication and division done below are to avoid overflow errors.
            val ratio1: BigInt = (M.toBigInt * (one - VarPhiBeta)) / supplyProtons
            val ratio2: BigInt = ((one - qNorm) * supplyNeutrons) / one
            val outNeutronsAmount: BigInt = (ratio1 * ratio2) / qNorm

            val NeutronsExpectedValue: BigInt = outNeutronsAmount
            val ProtonsExpectedValue: BigInt = M.toBigInt
            val ErgsExpectedValue: BigInt = (GLUON_BOX.value).toBigInt

            val __neutronsValueValid: Boolean = NeutronsActualValue == NeutronsExpectedValue
            val __protonsValueValid: Boolean = ProtonsActualValue == ProtonsExpectedValue
            val __ergsValueValid: Boolean = ErgsActualValue == ErgsExpectedValue

            sigmaProp(allOf(Coll(
                isHealthy, isHealthyAfter,
                cSameContract, cSameTokenIdentifiers,  
                __neutronsValueValid, __protonsValueValid, __ergsValueValid,
                cSameR4, cSameR5,
                __outVolumeMinusValidated, __outVolumePlusValidated, 
                __lastBlockPreserved,
                cSameR9PegFactor,
                cFees, // cFees also checks that R6._2 remains the same
                cOracle
            )))
        } else if (isBetaDecayMinusTx) {
            //Equation: M [Neutrons] = M * (1 - PhiBeta(T)) * ((q(R, S neutron)) / 1 - q(R, S neutron)) * (S protons / S neutrons) [Protons]
            val M: Long = (OUT_NEUTRONS_TOKEN._2 - NEUTRONS_TOKEN._2) // Number of neutron being decayed
            val worthOfMInErgs: BigInt = valueOfNeutrons(M)

            // Calculate the amount of days that has been since the last betaDecayTx
            // 1000 - 200 = 800 | 800 / 720 = 1
            val currentBlockNumber: Int = CONTEXT.HEIGHT
            val getNDaysPreFilteredValue: Int = ((currentBlockNumber - LAST_BUCKET_BLOCK) / blocksPerVolumeBucket).toInt
            val nDays: Int = if (getNDaysPreFilteredValue >= buckets) {buckets} else getNDaysPreFilteredValue

            // SAME AS BetaDecayPlus, but reversed between plus and minus
            // #1
            val outVolumeMinusExpectedValue = (if (nDays == 0) {VOLUME_MINUS(0)} else {0L}) + worthOfMInErgs
            val _volumeMinusAccounted = OUT_VOLUME_MINUS(0) == outVolumeMinusExpectedValue

            // #2: We sliced n - 1 of the value in between, and we check if all of it are 0s.
            val slicedNVolumeMinus: Coll[Long] = OUT_VOLUME_MINUS.slice(1, nDays)
            val _nVolumeMinusAllZeros: Boolean = slicedNVolumeMinus.forall{(indexedValue: Long) => indexedValue == 0L}

            // #3: If we slice the correct pieces from in and out, we should get the same exact value.
            val slicedOutVolumeMinus: Coll[Long] = OUT_VOLUME_MINUS.slice(nDays, buckets)
            val slicedInVolumeMinus: Coll[Long] = VOLUME_MINUS.slice(0, buckets - nDays)
            val _isSlicedValuedVolumeMinusEqual: Boolean = if (nDays > 0) {
                // When there are multiple days involved, we have to compare the days that are pushed towards
                // the right in OUT_VOLUME_MINUS, this starts at nDays and end at the last index.
                // For VOLUME_MINUS, it would be the first till buckets - nDays
                slicedOutVolumeMinus == slicedInVolumeMinus
            } else { // When the days are the same, we compare 1 - buckets because only the first index changed.
                OUT_VOLUME_MINUS.slice(1, buckets) == VOLUME_MINUS.slice(1, buckets)
            }

            val __outVolumeMinusValidated: Boolean = allOf(Coll(
                OUT_VOLUME_MINUS.size == buckets,
                _volumeMinusAccounted,
                _isSlicedValuedVolumeMinusEqual,
                _nVolumeMinusAllZeros
            ))

            // #1
            val outVolumePlusExpectedValue = if (nDays == 0) {VOLUME_PLUS(0)} else {0L}
            val _outVolumePlusFirstIndexedPreserved = outVolumePlusExpectedValue == OUT_VOLUME_PLUS(0)

            // #2: We sliced n - 1 of the value in between, and we check if all of it are 0s.
            val slicedNVolumePlus: Coll[Long] = OUT_VOLUME_PLUS.slice(1, nDays)
            val _nVolumePlusAllZeros: Boolean = slicedNVolumePlus.forall{(indexedValue: Long) => indexedValue == 0L}

            // #3
            val slicedOutVolumePlus: Coll[Long] = OUT_VOLUME_PLUS.slice(nDays, buckets)
            val slicedInVolumePlus: Coll[Long] = VOLUME_PLUS.slice(0, buckets - nDays)
            val _isSlicedValuedVolumePlusEqual: Boolean = if (nDays > 0) {
                // When there are multiple days involved, we have to compare the days that are pushed towards
                // the right in OUT_VOLUME_MINUS, this starts at nDays and end at the last index
                // for VOLUME_MINUS, it would be the first till buckets - nDays
                slicedOutVolumePlus == slicedInVolumePlus
            } else { // When the days are the same, we compare 1 - buckets because only the first index changed
                OUT_VOLUME_PLUS.slice(1, buckets) == VOLUME_PLUS.slice(1, buckets)
            }

            val __outVolumePlusValidated: Boolean = allOf(Coll(
                OUT_VOLUME_PLUS.size == buckets,
                _outVolumePlusFirstIndexedPreserved,
                _isSlicedValuedVolumePlusEqual,
                _nVolumePlusAllZeros
            ))

            val volumePlus: BigInt = sum(OUT_VOLUME_PLUS)
            val volumeMinus: BigInt = sum(OUT_VOLUME_MINUS)

            val volume: BigInt = if (volumePlus > volumeMinus) {0L.toBigInt} else {volumeMinus - volumePlus} // integer subtraction

            val VarPhiBeta: BigInt = phi0 + ((phi1 * volume) / reserve) // This fee remains in the reserve

            // Due to some issues with moving towards the next block. We should give it a margin of error of +/- 3 blocks.
            // There is a tricky situation where if the lastblock is within a day, and if it is always updated,
            // then we will always be at day 0 as long as there is a decay that happened within a day before
            // the lastBlockPreserved.
            //
            // To counteract this situation, we want to only get the currentBlockNumber that is closest to the previous blocksPerVolumeBucket
            val closestPreviousBlockValueViaBuckets: Int = (currentBlockNumber / blocksPerVolumeBucket) * blocksPerVolumeBucket
            val __lastBlockPreserved: Boolean = OUT_LAST_BUCKET_BLOCK == closestPreviousBlockValueViaBuckets

            // Neutrons increase in output, protons decrease in output.
            val NeutronsActualValue: BigInt = (OUT_NEUTRONS_TOKEN._2 - NEUTRONS_TOKEN._2).toBigInt
            val ProtonsActualValue: BigInt = (PROTONS_TOKEN._2 - OUT_PROTONS_TOKEN._2).toBigInt
            val ErgsActualValue: BigInt = (OUT_GLUON_BOX.value).toBigInt

            // The steps of multiplication and division done below are to avoid overflow errors.
            val ratio1: BigInt = (M.toBigInt * (one - VarPhiBeta)) / supplyNeutrons
            val ratio2: BigInt = (qNorm * supplyProtons) / one
            val outProtonsAmount: BigInt = (ratio1 * ratio2) / (one - qNorm)

            val NeutronsExpectedValue: BigInt = M.toBigInt
            val ProtonsExpectedValue: BigInt = outProtonsAmount
            val ErgsExpectedValue: BigInt = (GLUON_BOX.value).toBigInt

            val __neutronsValueValid: Boolean = NeutronsActualValue == NeutronsExpectedValue
            val __protonsValueValid: Boolean = ProtonsActualValue == ProtonsExpectedValue
            val __ergsValueValid: Boolean = ErgsActualValue == ErgsExpectedValue

            sigmaProp(allOf(Coll(
                isHealthy, isHealthyAfter,
                cSameContract, cSameTokenIdentifiers, 
                __neutronsValueValid, __protonsValueValid, __ergsValueValid,
                cSameR4, cSameR5,
                __outVolumePlusValidated, __outVolumeMinusValidated,
                __lastBlockPreserved,
                cSameR9PegFactor,
                cFees, // cFees also checks that R6._2 remains the same
                cOracle
            )))
        } else sigmaProp(false)
    } else if (isAdjustPegTx) {
        val pegFactorCorrect: Boolean = // PegFactor update direction and magnitude must be correct
            if (q > qUpperThreshold) OUT_PEG_FACTOR.toBigInt == PEG_FACTOR.toBigInt * 99 / 100       // Peg Factor must have decreased by 1%
            else if (q < qLowerThreshold) OUT_PEG_FACTOR.toBigInt == PEG_FACTOR.toBigInt * 101 / 100 // Peg Factor must have increased by 1%
            else false

        sigmaProp(allOf(Coll(
            !isHealthy, // Only when outside the healthy range.
            cOracle,
            cSameContract, cSameTokens, cSameValue, cSameR4, cSameR5, cSameR6, cSameR7, cSameR8, cSameR9LastBucketBlock,
            pegFactorCorrect // Peg Factor is the only register variable that changes
        ))) // Anyone may do this transaction when the fusion ratio is outside the healthy range
    } else if (isUpdateTreasury) {
        val newMultisig: SigmaProp = OUT_GLUON_BOX.R5[SigmaProp].get
        sigmaProp(allOf(Coll(
            cSameContract, cSameValue, cSameTokens, cSameR4,
            newMultisig != TREASURY_MULTISIG, // R5 is the only register that changes and it must change
            cSameR6, cSameR7, cSameR8, cSameR9
        ))) && TREASURY_MULTISIG // The transaction must be signed by the current TREASURY_MULTISIG
    } else { 
        sigmaProp(false)
    }
}
