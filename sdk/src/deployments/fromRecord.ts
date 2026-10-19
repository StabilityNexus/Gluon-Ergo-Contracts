import { parse } from "@fleet-sdk/serializer";
import { GluonError } from "../errors.ts";
import type { GluonDeployment, TokenInfo } from "../types.ts";

export function deploymentFromRecord(record: unknown): GluonDeployment {
  if (typeof record !== "object" || record === null) {
    throw new GluonError("INVALID_DEPLOYMENT", "record must be a non-null object");
  }
  const r = record as Record<string, unknown>;

  if (
    typeof r.name !== "string" ||
    (r.status !== "live" && r.status !== "superseded" && r.status !== "unused") ||
    typeof r.peg !== "string" ||
    typeof r.ergoTree !== "string" ||
    typeof r.address !== "string"
  ) {
    throw new GluonError("INVALID_DEPLOYMENT", "missing or invalid deployment metadata");
  }

  const tokens = r.tokens as Record<string, unknown> | undefined;
  if (!tokens || typeof tokens !== "object") {
    throw new GluonError("INVALID_DEPLOYMENT", "missing tokens");
  }

  const parseToken = (t: unknown): TokenInfo => {
    if (!t || typeof t !== "object") throw new GluonError("INVALID_DEPLOYMENT", "invalid token record");
    const tok = t as Record<string, unknown>;
    if (typeof tok.id !== "string" || typeof tok.name !== "string" || typeof tok.decimals !== "number" || tok.emission === undefined) {
      throw new GluonError("INVALID_DEPLOYMENT", "invalid token fields");
    }
    try {
      return {
        id: tok.id,
        name: tok.name,
        decimals: tok.decimals,
        emission: BigInt(tok.emission as string | number | bigint),
      };
    } catch {
      throw new GluonError("INVALID_DEPLOYMENT", "invalid token emission");
    }
  };

  const nft = parseToken(tokens.nft);
  const neutron = parseToken(tokens.neutron);
  const proton = parseToken(tokens.proton);

  const constants = r.constants as Record<string, unknown> | undefined;
  if (!constants || typeof constants !== "object") {
    throw new GluonError("INVALID_DEPLOYMENT", "missing constants");
  }
  if (
    typeof constants._MinFee !== "string" ||
    typeof constants._OraclePoolNFT !== "string" ||
    typeof constants._OracleBuybackNFT !== "string" ||
    typeof constants._OracleFeePk !== "string"
  ) {
    throw new GluonError("INVALID_DEPLOYMENT", "missing required constants");
  }

  let minFee: bigint;
  try {
    minFee = BigInt(constants._MinFee);
  } catch {
    throw new GluonError("INVALID_DEPLOYMENT", "invalid _MinFee");
  }

  const genesis = r.genesis as Record<string, unknown> | undefined;
  if (!genesis || typeof genesis !== "object") {
    throw new GluonError("INVALID_DEPLOYMENT", "missing genesis");
  }
  const registers = genesis.registers as Record<string, unknown> | undefined;
  if (!registers || typeof registers !== "object" || typeof registers.R9 !== "string") {
    throw new GluonError("INVALID_DEPLOYMENT", "missing genesis R9 register");
  }

  let decodedR9: unknown;
  try {
    decodedR9 = parse(registers.R9);
  } catch {
    throw new GluonError("INVALID_DEPLOYMENT", "failed to parse R9 register");
  }

  if (!Array.isArray(decodedR9) || decodedR9.length !== 2 || typeof decodedR9[0] !== "bigint" || typeof decodedR9[1] !== "bigint") {
    throw new GluonError("INVALID_DEPLOYMENT", "genesis R9 register is not a (Long, Long) pair");
  }

  const initialPegFactor = decodedR9[1];

  return {
    name: r.name,
    status: r.status,
    peg: r.peg,
    ergoTree: r.ergoTree,
    address: r.address,
    tokens: { nft, neutron, proton },
    minFee,
    oraclePoolNft: constants._OraclePoolNFT,
    oracleBuybackNft: constants._OracleBuybackNFT,
    oracleFeeTree: constants._OracleFeePk,
    initialPegFactor,
  };
}
