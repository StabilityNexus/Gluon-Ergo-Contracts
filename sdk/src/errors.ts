export type GluonErrorCode =
  | "REACTOR_UNHEALTHY"            // fission, fusion or beta decay while q is outside [0.5, 0.98]
  | "WOULD_LEAVE_HEALTHY_RANGE"    // a beta decay that would end outside the range
  | "REACTOR_HEALTHY"              // Adjust Peg while healthy
  | "ORACLE_STALE"                 // oracle box 35 or more blocks old at HEIGHT
  | "BOX_AMBIGUOUS"                // several unspent boxes hold the token (competing mempool updates): retry after the next block
  | "BOX_NOT_FOUND"
  | "BUYBACK_UNAVAILABLE"          // a beta decay owes an oracle fee but no buyback box exists
  | "AMOUNT_TOO_SMALL"             // the transaction would move no tokens
  | "AMOUNT_TOO_LARGE"             // the reactor cannot pay out or release that amount
  | "INSUFFICIENT_FUNDS"           // the wallet's boxes cannot cover the amounts and fees
  | "INVALID_ADDRESS"              // not a mainnet address, or not single-signature where that is required
  | "UNKNOWN_DEPLOYMENT"           // no bundled deployment with that name
  | "INVALID_DEPLOYMENT"           // a record that is malformed or not a v3 reactor
  | "NODE_ERROR";                  // the node did not answer, or answered with an error

export class GluonError extends Error {
  readonly code: GluonErrorCode;

  constructor(code: GluonErrorCode, message?: string) {
    super(message ?? code);
    this.name = "GluonError";
    this.code = code;
  }
}
