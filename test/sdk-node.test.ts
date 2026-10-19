import { describe, expect, it } from "vitest";
import { ErgoNode, findUniqueBox, parseJson, stringifyJson } from "../sdk/src/node.ts";
import { GluonError } from "../sdk/src/errors.ts";

function mockFetch(handler: (url: URL, init?: Parameters<typeof fetch>[1]) => Response | Promise<Response>): typeof fetch {
  return (async (input: Parameters<typeof fetch>[0], init?: Parameters<typeof fetch>[1]) => {
    const url = typeof input === "string" ? new URL(input) : input instanceof URL ? input : new URL(input.url);
    return handler(url, init);
  }) as typeof fetch;
}

function makeNodeBox(id: string, tokenId: string, ergoTree = "tree1", value = 1_000_000_000n) {
  return {
    boxId: id,
    transactionId: "tx_" + id,
    index: 0,
    value,
    ergoTree,
    creationHeight: 1000,
    assets: [{ tokenId, amount: 100n }],
    additionalRegisters: {},
  };
}

describe("ErgoNode & JSON-bigint", () => {
  it("preserves integers beyond 2^53 during parsing and stringifying", () => {
    const text = '{"large":12345678901234567890,"safe":42}';
    const parsed = parseJson<{ large: bigint; safe: number }>(text);
    expect(typeof parsed.large).toBe("bigint");
    expect(parsed.large).toBe(12345678901234567890n);
    expect(parsed.safe).toBe(42);

    const written = stringifyJson(parsed);
    expect(written).toBe('{"large":12345678901234567890,"safe":42}');
  });

  it("handles HTTP errors by throwing GluonError NODE_ERROR", async () => {
    const node = new ErgoNode("http://fake-node:9053", {
      fetch: mockFetch(() => new Response("Internal Server Error", { status: 500 })),
    });

    await expect(node.info()).rejects.toThrow(GluonError);
    await expect(node.info()).rejects.toMatchObject({
      code: "NODE_ERROR",
      message: expect.stringContaining("500: Internal Server Error"),
    });
  });

  it("handles request timeouts by throwing GluonError NODE_ERROR", async () => {
    const node = new ErgoNode("http://fake-node:9053", {
      timeoutMs: 10,
      fetch: mockFetch((_url, init) => {
        return new Promise((resolve, reject) => {
          init?.signal?.addEventListener("abort", () => {
            const err = new Error("The operation was aborted due to timeout");
            err.name = "TimeoutError";
            reject(err);
          });
          setTimeout(() => resolve(new Response("{}")), 200);
        });
      }),
    });

    await expect(node.info()).rejects.toThrow(GluonError);
    await expect(node.info()).rejects.toMatchObject({
      code: "NODE_ERROR",
      message: expect.stringContaining("failed:"),
    });
  });

  it("handles malformed JSON response with NODE_ERROR", async () => {
    const node = new ErgoNode("http://fake-node:9053", {
      fetch: mockFetch(() => new Response("not json", { status: 200 })),
    });

    await expect(node.info()).rejects.toThrow(GluonError);
    await expect(node.info()).rejects.toMatchObject({
      code: "NODE_ERROR",
      message: expect.stringContaining("failed to parse response"),
    });
  });
});

describe("Box lookup (findUniqueBox)", () => {
  const TOKEN = "token_abc";
  const REACTOR_TREE = "0008cd_reactor_tree";

  it("drops stale index entries that are missing from /utxo/withPool/byIds", async () => {
    const box1 = makeNodeBox("box1", TOKEN, REACTOR_TREE);
    const box2 = makeNodeBox("box2", TOKEN, REACTOR_TREE);

    const node = new ErgoNode("http://fake-node:9053", {
      fetch: mockFetch((url, init) => {
        if (url.pathname.includes("/blockchain/box/unspent/byTokenId/")) {
          return new Response(stringifyJson([box1, box2]));
        }
        if (url.pathname === "/utxo/withPool/byIds") {
          // box1 is stale in index, only box2 is returned by UTXO set
          return new Response(stringifyJson([box2]));
        }
        if (url.pathname.includes("/transactions/unconfirmed/inputs/byBoxId/")) {
          // not spent in mempool
          return new Response("", { status: 404 });
        }
        return new Response("Not Found", { status: 404 });
      }),
    });

    const box = await findUniqueBox(node, TOKEN, "reactor", REACTOR_TREE);
    expect(box.boxId).toBe("box2");
  });

  it("drops boxes spent in the mempool", async () => {
    const box1 = makeNodeBox("box1", TOKEN, REACTOR_TREE);

    const node = new ErgoNode("http://fake-node:9053", {
      fetch: mockFetch((url) => {
        if (url.pathname.includes("/blockchain/box/unspent/byTokenId/")) {
          return new Response(stringifyJson([box1]));
        }
        if (url.pathname === "/utxo/withPool/byIds") {
          return new Response(stringifyJson([box1]));
        }
        if (url.pathname === "/transactions/unconfirmed/inputs/byBoxId/box1") {
          // spent in mempool!
          return new Response(JSON.stringify({}), { status: 200 });
        }
        return new Response("Not Found", { status: 404 });
      }),
    });

    await expect(findUniqueBox(node, TOKEN, "reactor", REACTOR_TREE)).rejects.toThrow(GluonError);
    await expect(findUniqueBox(node, TOKEN, "reactor", REACTOR_TREE)).rejects.toMatchObject({
      code: "BOX_NOT_FOUND",
      message: `no unspent reactor box holds token ${TOKEN} first`,
    });
  });

  it("throws BOX_AMBIGUOUS when multiple live candidates exist", async () => {
    const box1 = makeNodeBox("box1", TOKEN, REACTOR_TREE);
    const box2 = makeNodeBox("box2", TOKEN, REACTOR_TREE);

    const node = new ErgoNode("http://fake-node:9053", {
      fetch: mockFetch((url) => {
        if (url.pathname.includes("/blockchain/box/unspent/byTokenId/")) {
          return new Response(stringifyJson([box1, box2]));
        }
        if (url.pathname === "/utxo/withPool/byIds") {
          return new Response(stringifyJson([box1, box2]));
        }
        if (url.pathname.includes("/transactions/unconfirmed/inputs/byBoxId/")) {
          return new Response("", { status: 404 });
        }
        return new Response("Not Found", { status: 404 });
      }),
    });

    await expect(findUniqueBox(node, TOKEN, "reactor", REACTOR_TREE)).rejects.toThrow(GluonError);
    await expect(findUniqueBox(node, TOKEN, "reactor", REACTOR_TREE)).rejects.toMatchObject({
      code: "BOX_AMBIGUOUS",
      message: expect.stringContaining(
        `2 unspent reactor boxes hold token ${TOKEN}: unconfirmed transactions compete to replace the box, so try again after the next block`,
      ),
    });
  });

  it("filters out boxes with the wrong ErgoTree", async () => {
    const wrongTreeBox = makeNodeBox("box_wrong", TOKEN, "wrong_tree");
    const rightTreeBox = makeNodeBox("box_right", TOKEN, REACTOR_TREE);

    const node = new ErgoNode("http://fake-node:9053", {
      fetch: mockFetch((url) => {
        if (url.pathname.includes("/blockchain/box/unspent/byTokenId/")) {
          return new Response(stringifyJson([wrongTreeBox, rightTreeBox]));
        }
        if (url.pathname === "/utxo/withPool/byIds") {
          return new Response(stringifyJson([wrongTreeBox, rightTreeBox]));
        }
        if (url.pathname.includes("/transactions/unconfirmed/inputs/byBoxId/")) {
          return new Response("", { status: 404 });
        }
        return new Response("Not Found", { status: 404 });
      }),
    });

    const box = await findUniqueBox(node, TOKEN, "reactor", REACTOR_TREE);
    expect(box.boxId).toBe("box_right");
  });

  it("throws BOX_NOT_FOUND if the token is not the box's first token", async () => {
    const box = {
      boxId: "box_second",
      transactionId: "tx_box_second",
      index: 0,
      value: 1_000_000_000n,
      ergoTree: REACTOR_TREE,
      creationHeight: 1000,
      assets: [
        { tokenId: "other_token", amount: 1n },
        { tokenId: TOKEN, amount: 100n },
      ],
      additionalRegisters: {},
    };

    const node = new ErgoNode("http://fake-node:9053", {
      fetch: mockFetch((url) => {
        if (url.pathname.includes("/blockchain/box/unspent/byTokenId/")) {
          return new Response(stringifyJson([box]));
        }
        if (url.pathname === "/utxo/withPool/byIds") {
          return new Response(stringifyJson([box]));
        }
        if (url.pathname.includes("/transactions/unconfirmed/inputs/byBoxId/")) {
          return new Response("", { status: 404 });
        }
        return new Response("Not Found", { status: 404 });
      }),
    });

    await expect(findUniqueBox(node, TOKEN, "reactor", REACTOR_TREE)).rejects.toThrow(GluonError);
    await expect(findUniqueBox(node, TOKEN, "reactor", REACTOR_TREE)).rejects.toMatchObject({
      code: "BOX_NOT_FOUND",
      message: `no unspent reactor box holds token ${TOKEN} first`,
    });
  });
});
