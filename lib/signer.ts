import type { EIP12UnsignedTransaction, SignedTransaction } from "@fleet-sdk/common";
import { Network, utxoDiff, utxoSum } from "@fleet-sdk/common";
import { ErgoHDKey } from "@fleet-sdk/wallet";
import { type AvlTree, AvlTree$, type BlockchainStateContext, GroupElement$, ProverBuilder$ } from "sigmastate-js/main";
import type { ErgoNode } from "../sdk/src/node.ts";

/**
 * The signing key, derived from MNEMONIC (and the optional MNEMONIC_PASSWORD) in the environment along the
 * EIP-3 path m/44'/429'/0'/0/index. This is the only secret the scripts read (N2).
 */
export function keyFromEnvironment(index = 0): ErgoHDKey {
  const mnemonic = process.env.MNEMONIC;
  if (!mnemonic) throw new Error("set MNEMONIC in .env (see .env.example)");
  return ErgoHDKey.fromMnemonicSync(mnemonic.trim(), { passphrase: process.env.MNEMONIC_PASSWORD ?? "" }).deriveChild(index);
}

// AvlTree$.fromDigest exists in sigmastate-js 0.4.6 (Fleet's mock-chain uses it) but is missing from its typings.
const avlTreeFromDigest = (digest: string) =>
  (AvlTree$ as unknown as { fromDigest(digest: string): AvlTree }).fromDigest(digest);

/** The context in which the next block will evaluate scripts, built from the node's last 10 headers. */
export async function blockchainContext(node: ErgoNode): Promise<BlockchainStateContext> {
  const raw = await node.lastHeaders(10);
  const headers = raw.map((h) => ({
    id: h.id,
    version: h.version,
    parentId: h.parentId,
    ADProofsRoot: h.adProofsRoot,
    stateRoot: avlTreeFromDigest(h.stateRoot),
    transactionsRoot: h.transactionsRoot,
    timestamp: BigInt(h.timestamp),
    nBits: BigInt(h.nBits),
    height: h.height,
    extensionRoot: h.extensionHash,
    minerPk: GroupElement$.fromPointHex(h.powSolutions.pk),
    powOnetimePk: GroupElement$.fromPointHex(h.powSolutions.w),
    powNonce: h.powSolutions.n,
    powDistance: BigInt(h.powSolutions.d),
    votes: h.votes,
  }));
  const last = headers[0];
  return {
    sigmaLastHeaders: headers,
    previousStateDigest: raw[0].stateRoot,
    sigmaPreHeader: {
      version: last.version,
      parentId: last.id,
      timestamp: BigInt(Date.now()),
      nBits: last.nBits,
      height: last.height + 1,
      minerPk: last.minerPk,
      votes: "000000",
    },
  };
}

/**
 * Signs a transaction. Signing reduces every input's script in `context`, so it fails, with the
 * interpreter's message, if a contract rejects the transaction.
 */
export async function signTransaction(
  node: ErgoNode,
  tx: EIP12UnsignedTransaction,
  keys: ErgoHDKey[],
): Promise<SignedTransaction> {
  const [{ parameters }, context] = await Promise.all([node.info(), blockchainContext(node)]);
  const builder = ProverBuilder$.create(
    { ...parameters, softForkStartingHeight: undefined, softForkVotesCollected: undefined },
    Network.Mainnet,
  );
  for (const key of keys) {
    if (!key.privateKey) throw new Error("the signing key has no private key");
    builder.withDLogSecret(BigInt(`0x${Buffer.from(key.privateKey).toString("hex")}`));
  }
  const prover = builder.build();
  const burning = utxoDiff(utxoSum(tx.inputs), utxoSum(tx.outputs)).tokens.filter((t) => t.tokenId !== tx.inputs[0].boxId);
  const reduced = prover.reduce(context, tx, tx.inputs, tx.dataInputs, burning, 0);
  return prover.signReduced(reduced, undefined);
}
