import type { Box, SignedTransaction } from "@fleet-sdk/common";
import JSONBig from "json-bigint";
import { GluonError } from "./errors.ts";

const jsonBig = JSONBig({ useNativeBigInt: true });

/** JSON.parse that keeps integers beyond 2^53 exact, as bigint. */
export function parseJson<T>(text: string): T {
  return jsonBig.parse(text) as T;
}

/** JSON.stringify that writes bigint values as plain JSON numbers. */
export function stringifyJson(value: unknown, space?: number): string {
  return jsonBig.stringify(value, undefined, space);
}

export interface NodeHeader {
  id: string;
  parentId: string;
  version: number;
  height: number;
  timestamp: number;
  nBits: number;
  stateRoot: string;
  adProofsRoot: string;
  transactionsRoot: string;
  extensionHash: string;
  powSolutions: { pk: string; w: string; n: string; d: number | bigint | string };
  votes: string;
}

/** Blockchain parameters as the node reports them in /info. */
export interface NodeParameters {
  storageFeeFactor: number;
  minValuePerByte: number;
  maxBlockSize: number;
  tokenAccessCost: number;
  inputCost: number;
  dataInputCost: number;
  outputCost: number;
  maxBlockCost: number;
  blockVersion: number;
}

interface NodeBox {
  boxId: string;
  transactionId: string;
  index: number;
  value: number | bigint;
  ergoTree: string;
  creationHeight: number;
  assets: { tokenId: string; amount: number | bigint }[];
  additionalRegisters: Record<string, string>;
}

export interface ErgoNodeOptions {
  fetch?: typeof fetch;
  timeoutMs?: number;
}

export class ErgoNode {
  readonly url: string;
  private readonly fetch: typeof fetch;
  private readonly timeoutMs: number;

  constructor(url: string, options?: ErgoNodeOptions) {
    this.url = url;
    this.fetch = options?.fetch ?? globalThis.fetch;
    this.timeoutMs = options?.timeoutMs ?? 25_000;
  }

  async info(): Promise<{ fullHeight: number; parameters: NodeParameters }> {
    return this.get("/info");
  }

  async height(): Promise<number> {
    return (await this.info()).fullHeight;
  }

  /** The last `count` block headers, newest first. */
  async lastHeaders(count = 10): Promise<NodeHeader[]> {
    const headers = await this.get<NodeHeader[]>(`/blocks/lastHeaders/${count}`);
    return headers.sort((a, b) => b.height - a.height);
  }

  /** Unspent boxes holding a token, including unconfirmed ones and excluding boxes spent in the mempool. */
  async unspentByTokenId(tokenId: string): Promise<Box<bigint>[]> {
    const boxes = await this.live(
      await this.get<NodeBox[]>(`/blockchain/box/unspent/byTokenId/${tokenId}?${MEMPOOL_AWARE}&offset=0&limit=100`),
    );
    const spent = await Promise.all(boxes.map((b) => this.spentInMempool(b.boxId)));
    return boxes.filter((_, i) => !spent[i]);
  }

  /** Unspent boxes of an address, including unconfirmed ones and excluding boxes spent in the mempool. */
  async unspentByAddress(address: string): Promise<Box<bigint>[]> {
    const boxes = await this.post<NodeBox[]>(
      `/blockchain/box/unspent/byAddress?${MEMPOOL_AWARE}&offset=0&limit=500`,
      JSON.stringify(address),
    );
    return this.live(boxes);
  }

  /**
   * The boxes that are in the UTXO set or created in the mempool. The node's blockchain index can list
   * boxes as unspent after they have been spent, so its answers are checked against the UTXO set.
   */
  private async live(boxes: NodeBox[]): Promise<Box<bigint>[]> {
    if (boxes.length === 0) return [];
    const live = await this.post<NodeBox[]>("/utxo/withPool/byIds", JSON.stringify(boxes.map((b) => b.boxId)));
    const ids = new Set(live.map((b) => b.boxId));
    return boxes.filter((b) => ids.has(b.boxId)).map(toBox);
  }

  /** Whether an unconfirmed transaction spends the box. */
  private async spentInMempool(boxId: string): Promise<boolean> {
    const path = `/transactions/unconfirmed/inputs/byBoxId/${boxId}`;
    const request = `GET ${path}`;
    try {
      const signal = AbortSignal.timeout(this.timeoutMs);
      const response = await this.fetch(new URL(path, this.url), { signal });
      if (response.status === 404) return false;
      if (!response.ok) {
        const text = await response.text();
        throw new GluonError("NODE_ERROR", `${request} failed with ${response.status}: ${text}`);
      }
      return true;
    } catch (e) {
      if (e instanceof GluonError) throw e;
      const message = e instanceof Error ? e.message : String(e);
      throw new GluonError("NODE_ERROR", `${request} failed: ${message}`);
    }
  }

  /** Has the node validate a signed transaction without broadcasting it. Returns the transaction id. */
  async checkTransaction(tx: SignedTransaction): Promise<string> {
    return this.post("/transactions/check", stringifyJson(tx));
  }

  /** Submits a signed transaction to the node's mempool. Returns the transaction id. */
  async submitTransaction(tx: SignedTransaction): Promise<string> {
    return this.post("/transactions", stringifyJson(tx));
  }

  private async get<T>(path: string): Promise<T> {
    const request = `GET ${path}`;
    try {
      const signal = AbortSignal.timeout(this.timeoutMs);
      const response = await this.fetch(new URL(path, this.url), { signal });
      return await this.read<T>(response, request);
    } catch (e) {
      if (e instanceof GluonError) throw e;
      const message = e instanceof Error ? e.message : String(e);
      throw new GluonError("NODE_ERROR", `${request} failed: ${message}`);
    }
  }

  private async post<T>(path: string, body: string): Promise<T> {
    const request = `POST ${path.split("?")[0]}`;
    try {
      const signal = AbortSignal.timeout(this.timeoutMs);
      const response = await this.fetch(new URL(path, this.url), {
        method: "POST",
        headers: { "Content-Type": "application/json" },
        body,
        signal,
      });
      return await this.read<T>(response, request);
    } catch (e) {
      if (e instanceof GluonError) throw e;
      const message = e instanceof Error ? e.message : String(e);
      throw new GluonError("NODE_ERROR", `${request} failed: ${message}`);
    }
  }

  private async read<T>(response: Response, request: string): Promise<T> {
    let text: string;
    try {
      text = await response.text();
    } catch (e) {
      const message = e instanceof Error ? e.message : String(e);
      throw new GluonError("NODE_ERROR", `${request} failed: ${message}`);
    }
    if (!response.ok) throw new GluonError("NODE_ERROR", `${request} failed with ${response.status}: ${text}`);
    try {
      return parseJson<T>(text);
    } catch (e) {
      const message = e instanceof Error ? e.message : String(e);
      throw new GluonError("NODE_ERROR", `${request} failed to parse response: ${message}`);
    }
  }
}

const MEMPOOL_AWARE = "includeUnconfirmed=true&excludeMempoolSpent=true";

function toBox(b: NodeBox): Box<bigint> {
  return {
    boxId: b.boxId,
    transactionId: b.transactionId,
    index: b.index,
    value: BigInt(b.value),
    ergoTree: b.ergoTree,
    creationHeight: b.creationHeight,
    assets: b.assets.map((a) => ({ tokenId: a.tokenId, amount: BigInt(a.amount) })),
    additionalRegisters: b.additionalRegisters,
  };
}

/**
 * The unspent box whose first token is `tokenId` (and whose ErgoTree is `ergoTree`, if given): the box the
 * contract finds by that token. Unconfirmed boxes count, and boxes spent in the mempool do not.
 */
export async function findUniqueBox(
  node: ErgoNode,
  tokenId: string,
  what: string,
  ergoTree?: string,
): Promise<Box<bigint>> {
  const boxes = (await node.unspentByTokenId(tokenId)).filter(
    (b) => b.assets[0]?.tokenId === tokenId && (ergoTree === undefined || b.ergoTree === ergoTree),
  );
  if (boxes.length > 1) {
    throw new GluonError(
      "BOX_AMBIGUOUS",
      `${boxes.length} unspent ${what} boxes hold token ${tokenId}: unconfirmed transactions compete to replace the box, so try again after the next block`,
    );
  }
  if (boxes.length === 0) {
    throw new GluonError("BOX_NOT_FOUND", `no unspent ${what} box holds token ${tokenId} first`);
  }
  return boxes[0];
}
