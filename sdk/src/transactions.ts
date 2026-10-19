import type { EIP12UnsignedTransaction } from "@fleet-sdk/common";
import {
  type Amount,
  type Box,
  ErgoUnsignedInput,
  OutputBuilder,
  RECOMMENDED_MIN_FEE_VALUE,
  SAFE_MIN_BOX_VALUE,
  TransactionBuilder,
} from "@fleet-sdk/core";
import { SByte, SColl, SGroupElement, SInt, SSigmaProp } from "@fleet-sdk/serializer";
import {
  type ActionResult,
  type Fees,
  type ReactorState,
  adjustPeg,
  betaDecayMinus,
  betaDecayPlus,
  fission,
  fusion,
} from "./math.ts";
import { type ReactorTokens, oraclePrice, p2pkTree, reactorAssets, reactorRegisters, readReactor } from "./reactor.ts";

/** What every transaction on a reactor needs besides the parameters of the action itself. */
export interface TxContext {
  /** Current chain height. Scripts see height + 1: the height of the block that includes the transaction. */
  height: number;
  /** `_MinFee` of the deployment. */
  minFee: bigint;
  /** The reactor box. It is spent as INPUTS(0). */
  reactor: Box<Amount>;
  /** The oracle pool box. It is passed as data input 0. */
  oracle: Box<Amount>;
  /** Boxes of the caller. They pay the amounts and the miner fee and provide the tokens a user sends. */
  inputs: Box<Amount>[];
  /** Address or ErgoTree that receives the tokens the reactor releases, and the change. */
  recipient: string;
  /** Public key of the optional UI-fee recipient, passed to the contract as context variable 0. */
  uiFeeKey?: string;
  /** The oracle buyback box. A beta decay that owes an oracle fee spends it as its last input. */
  buyback?: Box<Amount>;
}

// Oracle-core v2 buyback contracts (such as the USD oracle's DORT buyback) let anyone top them up: the
// spending input sets context variable 0 to 1, and the successor box keeps the script and tokens and
// stores the id of the box it replaces in R4. The Gluon contract itself only checks the successor's
// script, first token and value.
const TOP_UP = { 0: SInt(1).toHex() };

export function fissionTx(ctx: TxContext, erg: bigint): EIP12UnsignedTransaction {
  const { state, tokens } = readReactor(ctx.reactor);
  return userActionTx(ctx, state, tokens, fission(state, ctx.minFee, erg, ctx.uiFeeKey !== undefined), false);
}

export function fusionTx(ctx: TxContext, erg: bigint): EIP12UnsignedTransaction {
  const { state, tokens } = readReactor(ctx.reactor);
  return userActionTx(ctx, state, tokens, fusion(state, ctx.minFee, erg, ctx.uiFeeKey !== undefined), false);
}

export function betaDecayPlusTx(ctx: TxContext, protons: bigint): EIP12UnsignedTransaction {
  const { state, tokens } = readReactor(ctx.reactor);
  const result = betaDecayPlus(state, ctx.minFee, oraclePrice(ctx.oracle), protons, ctx.height + 1, ctx.uiFeeKey !== undefined);
  return userActionTx(ctx, state, tokens, result, true);
}

export function betaDecayMinusTx(ctx: TxContext, neutrons: bigint): EIP12UnsignedTransaction {
  const { state, tokens } = readReactor(ctx.reactor);
  const result = betaDecayMinus(state, ctx.minFee, oraclePrice(ctx.oracle), neutrons, ctx.height + 1, ctx.uiFeeKey !== undefined);
  return userActionTx(ctx, state, tokens, result, true);
}

/** Adjust Peg: anyone may move the peg factor by 1 % while the reactor is outside its healthy range. */
export function adjustPegTx(ctx: TxContext): EIP12UnsignedTransaction {
  const { state } = readReactor(ctx.reactor);
  const next = adjustPeg(state, ctx.minFee, oraclePrice(ctx.oracle));
  if (!next) throw new Error("the reactor is in its healthy range, so Adjust Peg is not allowed");
  return reactorTx(ctx, next);
}

/**
 * A transaction that spends the reactor into a box with state `next`, with no other required outputs.
 * Whatever the reactor releases goes to the change. Adjust Peg and Update Treasury have this shape.
 */
export function reactorTx(ctx: TxContext, next: ReactorState): EIP12UnsignedTransaction {
  const { tokens } = readReactor(ctx.reactor);
  return assemble(ctx, [reactorOutput(ctx, next, tokens)]);
}

/**
 * Update Treasury: the current treasury replaces the key in R5.
 * The contract identifies this transaction by INPUTS(1), so `ctx.inputs[0]` must be a box of the current
 * treasury's address, and the treasury must sign.
 */
export function updateTreasuryTx(ctx: TxContext, newTreasury: string): EIP12UnsignedTransaction {
  const { state } = readReactor(ctx.reactor);
  if (ctx.inputs[0]?.ergoTree !== p2pkTree(state.treasury)) {
    throw new Error("the first funding input must be a box of the current treasury");
  }
  if (newTreasury === state.treasury) throw new Error("the new treasury must differ from the current one");
  return reactorTx(ctx, { ...state, treasury: newTreasury });
}

// Layout shared by fission, fusion and beta decays:
// OUTPUTS(0) is the reactor, OUTPUTS(1) receives the released tokens, and the fee outputs follow in the
// order the contract checks them: oracle fee (beta decays only), dev fee, UI fee. Fees that are zero have
// no output.
function userActionTx(
  ctx: TxContext,
  s: ReactorState,
  tokens: ReactorTokens,
  { next, fees }: ActionResult,
  betaDecay: boolean,
): EIP12UnsignedTransaction {
  const released = [
    { tokenId: tokens.neutron, amount: s.neutrons - next.neutrons },
    { tokenId: tokens.proton, amount: s.protons - next.protons },
  ].filter((t) => t.amount > 0n);
  const outputs = [
    reactorOutput(ctx, next, tokens),
    new OutputBuilder(SAFE_MIN_BOX_VALUE, ctx.recipient).addTokens(released),
    ...feeOutputs(ctx, s, fees, betaDecay),
  ];
  return assemble(ctx, outputs, betaDecay && fees.oracle > 0n ? buybackInput(ctx) : undefined);
}

function reactorOutput(ctx: TxContext, next: ReactorState, tokens: ReactorTokens): OutputBuilder {
  return new OutputBuilder(next.value, ctx.reactor.ergoTree)
    .addTokens(reactorAssets(next, tokens))
    .setAdditionalRegisters(reactorRegisters(next));
}

function feeOutputs(ctx: TxContext, s: ReactorState, fees: Fees, betaDecay: boolean): OutputBuilder[] {
  const outputs: OutputBuilder[] = [];
  if (betaDecay && fees.oracle > 0n) {
    // The buyback box is recreated with the oracle fee (plus `_MinFee`) added to its value.
    const buyback = requireBuyback(ctx);
    outputs.push(
      new OutputBuilder(BigInt(buyback.value) + fees.oracle + ctx.minFee, buyback.ergoTree)
        .addTokens(buyback.assets)
        .setAdditionalRegisters({ R4: SColl(SByte, buyback.boxId).toHex() }),
    );
  }
  if (fees.dev > 0n) outputs.push(new OutputBuilder(fees.dev + ctx.minFee, p2pkTree(s.treasury)));
  if (fees.ui > 0n && ctx.uiFeeKey) outputs.push(new OutputBuilder(fees.ui + ctx.minFee, p2pkTree(ctx.uiFeeKey)));
  return outputs;
}

function requireBuyback(ctx: TxContext): Box<Amount> {
  if (!ctx.buyback) throw new Error("this beta decay owes an oracle fee, so it needs the oracle buyback box");
  return ctx.buyback;
}

function buybackInput(ctx: TxContext): ErgoUnsignedInput {
  return new ErgoUnsignedInput(requireBuyback(ctx)).setContextExtension(TOP_UP);
}

function assemble(ctx: TxContext, outputs: OutputBuilder[], lastInput?: ErgoUnsignedInput): EIP12UnsignedTransaction {
  const reactor = new ErgoUnsignedInput(ctx.reactor);
  if (ctx.uiFeeKey) reactor.setContextExtension({ 0: SSigmaProp(SGroupElement(ctx.uiFeeKey)).toHex() });
  let builder = new TransactionBuilder(ctx.height)
    .from(reactor, { ensureInclusion: true })
    .and.from(ctx.inputs, { ensureInclusion: true });
  if (lastInput) builder = builder.and.from(lastInput, { ensureInclusion: true });
  const tx = builder
    .withDataFrom([ctx.oracle])
    .to(outputs)
    .sendChangeTo(ctx.recipient)
    .payFee(RECOMMENDED_MIN_FEE_VALUE)
    .build()
    .toEIP12Object();
  // The contract relies on input positions; make sure the builder kept them.
  if (tx.inputs[0].boxId !== ctx.reactor.boxId) throw new Error("the reactor must be the first input");
  if (lastInput && tx.inputs[tx.inputs.length - 1].boxId !== lastInput.boxId) {
    throw new Error("the buyback box must be the last input");
  }
  return tx;
}
