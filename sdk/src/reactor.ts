import type { Amount, Box } from "@fleet-sdk/core";
import { SColl, SGroupElement, SLong, SPair, SSigmaProp, parse } from "@fleet-sdk/serializer";
import type { ReactorState } from "./math.ts";

/** Token ids of a reactor, in the order the contract expects them in the box. */
export interface ReactorTokens {
  nft: string;
  neutron: string;
  proton: string;
}

/** The R4–R9 values of a reactor box, serialized as the contract expects them. */
export function reactorRegisters(s: ReactorState) {
  return {
    R4: SPair(SLong(s.totalNeutrons), SLong(s.totalProtons)).toHex(),
    R5: SSigmaProp(SGroupElement(s.treasury)).toHex(),
    R6: SPair(SLong(s.devFeeRepaid), SLong(s.maxDevFee)).toHex(),
    R7: SColl(SLong, s.volumePlus).toHex(),
    R8: SColl(SLong, s.volumeMinus).toHex(),
    R9: SPair(SLong(s.lastBucketBlock), SLong(s.pegFactor)).toHex(),
  };
}

/** The tokens of a reactor box: the NFT, then neutrons, then protons. */
export function reactorAssets(s: ReactorState, tokens: ReactorTokens) {
  return [
    { tokenId: tokens.nft, amount: 1n },
    { tokenId: tokens.neutron, amount: s.neutrons },
    { tokenId: tokens.proton, amount: s.protons },
  ];
}

import type { GluonDeployment } from "./types.ts";
import { oraclePrice } from "./oracle.ts";

export { oraclePrice } from "./oracle.ts";

export function readReactor(
  box: Box<Amount>,
  deployment?: GluonDeployment,
): { state: ReactorState; tokens: ReactorTokens } {
  if (deployment) {
    if (box.ergoTree !== deployment.ergoTree) {
      throw new Error(`box has wrong ErgoTree: expected ${deployment.ergoTree}, got ${box.ergoTree}`);
    }
    const [nft, neutron, proton] = box.assets;
    if (
      !nft ||
      !neutron ||
      !proton ||
      nft.tokenId !== deployment.tokens.nft.id ||
      neutron.tokenId !== deployment.tokens.neutron.id ||
      proton.tokenId !== deployment.tokens.proton.id
    ) {
      throw new Error("box has wrong token ids");
    }
  }
  const r = box.additionalRegisters;
  if (!r.R4 || !r.R5 || !r.R6 || !r.R7 || !r.R8 || !r.R9) throw new Error(`box ${box.boxId} is not a reactor box`);
  const [nft, neutron, proton] = box.assets;
  const [totalNeutrons, totalProtons] = parse<[bigint, bigint]>(r.R4);
  const key = parse<unknown>(r.R5);
  if (!(key instanceof Uint8Array) || key.length !== 33) throw new Error("R5 must hold a single-signature treasury key");
  const treasury = Array.from(key, (b) => b.toString(16).padStart(2, "0")).join("");
  const [devFeeRepaid, maxDevFee] = parse<[bigint, bigint]>(r.R6);
  const [lastBucketBlock, pegFactor] = parse<[bigint, bigint]>(r.R9);
  return {
    state: {
      value: BigInt(box.value),
      neutrons: BigInt(neutron.amount),
      protons: BigInt(proton.amount),
      totalNeutrons,
      totalProtons,
      treasury,
      devFeeRepaid,
      maxDevFee,
      volumePlus: parse<bigint[]>(r.R7),
      volumeMinus: parse<bigint[]>(r.R8),
      lastBucketBlock,
      pegFactor,
    },
    tokens: { nft: nft.tokenId, neutron: neutron.tokenId, proton: proton.tokenId },
  };
}

/** ErgoTree of the P2PK address of a public key: the tree whose propositionBytes equal the key's propBytes. */
export function p2pkTree(publicKey: string): string {
  return `0008cd${publicKey}`;
}
