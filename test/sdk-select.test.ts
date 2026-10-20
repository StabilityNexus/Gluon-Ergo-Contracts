import { describe, expect, it } from "vitest";
import type { Box } from "@fleet-sdk/core";
import { ErgoAddress, Network, SAFE_MIN_BOX_VALUE } from "@fleet-sdk/core";
import { selectInputs } from "../sdk/src/select.ts";
import { GluonError } from "../sdk/src/errors.ts";

function makeBox(
  id: string,
  nanoErgs: bigint,
  tokens: { tokenId: string; amount: bigint }[] = [],
  ergoTree = "default_tree",
): Box<bigint> {
  return {
    boxId: id,
    transactionId: "tx_" + id,
    index: 0,
    ergoTree,
    creationHeight: 1000,
    value: nanoErgs,
    assets: tokens,
    additionalRegisters: {},
  };
}

describe("Input selection (selectInputs)", () => {
  const TOKEN_A = "token_a";
  const TOKEN_B = "token_b";

  it("selects only needed boxes, satisfying tokens first (largest amount first)", () => {
    // 3 boxes holding TOKEN_A
    const boxSmall = makeBox("b_small", 5_000_000n, [{ tokenId: TOKEN_A, amount: 10n }]);
    const boxLarge = makeBox("b_large", 5_000_000n, [{ tokenId: TOKEN_A, amount: 50n }]);
    const boxMedium = makeBox("b_medium", 5_000_000n, [{ tokenId: TOKEN_A, amount: 25n }]);

    // Target needs 40 of TOKEN_A and 2 ERG
    const selected = selectInputs(
      [boxSmall, boxLarge, boxMedium],
      {
        nanoErgs: 2_000_000n,
        tokens: [{ tokenId: TOKEN_A, amount: 40n }],
      },
    );

    // boxLarge alone provides 50 of TOKEN_A and 5_000_000 nanoERG (change = 3_000_000 >= SAFE_MIN_BOX_VALUE)
    expect(selected.map((b) => b.boxId)).toEqual(["b_large"]);
  });

  it("satisfies ERG requirement with largest value boxes first after tokens", () => {
    const boxToken = makeBox("b_token", 1_000_000n, [{ tokenId: TOKEN_A, amount: 10n }]);
    const boxErgSmall = makeBox("b_erg_small", 2_000_000n);
    const boxErgLarge = makeBox("b_erg_large", 6_000_000n);
    const boxErgMedium = makeBox("b_erg_medium", 4_000_000n);

    // Target needs TOKEN_A (10) and 5_000_000 nanoERG
    const selected = selectInputs(
      [boxErgSmall, boxToken, boxErgLarge, boxErgMedium],
      {
        nanoErgs: 5_000_000n,
        tokens: [{ tokenId: TOKEN_A, amount: 10n }],
      },
    );

    // boxToken gives 1_000_000 nanoERG. We need 4_000_000 more nanoERG.
    // boxErgLarge gives 6_000_000 nanoERG. Total = 7_000_000, change = 2_000_000 >= SAFE_MIN_BOX_VALUE.
    // Selected should be [boxToken, boxErgLarge].
    expect(selected.map((b) => b.boxId)).toEqual(["b_token", "b_erg_large"]);
  });

  it("prioritizes boxes of firstAddress", () => {
    const treasuryKey = "0279be667ef9dcbbac55a06295ce870b07029bfcdb2dce28d959f2815b16f81798";
    const treasuryAddr = ErgoAddress.fromPublicKey(treasuryKey, Network.Mainnet);
    const treasuryTree = treasuryAddr.ergoTree;

    const boxOther = makeBox("b_other", 10_000_000n, [], "other_tree");
    const boxTreasury = makeBox("b_treasury", 3_000_000n, [], treasuryTree);

    // Target needs 2_000_000 nanoERG, with treasury address prioritized
    const selected = selectInputs(
      [boxOther, boxTreasury],
      { nanoErgs: 2_000_000n },
      { firstAddress: treasuryAddr.encode() },
    );

    // boxTreasury has 3_000_000 nanoERG (enough for 2_000_000 + 1_000_000 change) and matches firstAddress.
    // It should be chosen instead of boxOther despite boxOther having higher value!
    expect(selected.map((b) => b.boxId)).toEqual(["b_treasury"]);
  });

  it("pads change below SAFE_MIN_BOX_VALUE by selecting an additional box", () => {
    // Target needs 1_500_000 nanoERG
    // Box 1 has 2_000_000 nanoERG -> leaves 500_000 change (< 1_000_000 SAFE_MIN_BOX_VALUE)
    // Box 2 has 2_000_000 nanoERG -> pads change to 2_500_000 >= SAFE_MIN_BOX_VALUE
    const box1 = makeBox("b1", 2_000_000n);
    const box2 = makeBox("b2", 2_000_000n);

    const selected = selectInputs([box1, box2], { nanoErgs: 1_500_000n });
    expect(selected.map((b) => b.boxId)).toEqual(["b1", "b2"]);
  });

  it("leaves exact change (0 nanoERG, no tokens) without selecting an unnecessary box", () => {
    const box1 = makeBox("b1", 2_000_000n);
    const box2 = makeBox("b2", 1_000_000n);

    // Target needs exactly 2_000_000 nanoERG
    const selected = selectInputs([box1, box2], { nanoErgs: 2_000_000n });
    expect(selected.map((b) => b.boxId)).toEqual(["b1"]);
  });

  it("pads change when token change exists even if ERG exactly matches", () => {
    // Box 1 has TOKEN_A (20) and 2_000_000 nanoERG
    // Target needs TOKEN_A (10) and 2_000_000 nanoERG
    // ERG change is 0, but 10 of TOKEN_A remains as change, requiring a change box with >= SAFE_MIN_BOX_VALUE
    const box1 = makeBox("b1", 2_000_000n, [{ tokenId: TOKEN_A, amount: 20n }]);
    const box2 = makeBox("b2", 1_000_000n);

    const selected = selectInputs([box1, box2], {
      nanoErgs: 2_000_000n,
      tokens: [{ tokenId: TOKEN_A, amount: 10n }],
    });

    expect(selected.map((b) => b.boxId)).toEqual(["b1", "b2"]);
  });

  it("throws INSUFFICIENT_FUNDS when required token cannot be covered", () => {
    const box1 = makeBox("b1", 5_000_000n, [{ tokenId: TOKEN_A, amount: 5n }]);

    expect(() =>
      selectInputs([box1], {
        nanoErgs: 1_000_000n,
        tokens: [{ tokenId: TOKEN_A, amount: 10n }],
      }),
    ).toThrowError(
      new GluonError("INSUFFICIENT_FUNDS", `insufficient funds for token ${TOKEN_A}: needed 10, available 5`),
    );
  });

  it("throws INSUFFICIENT_FUNDS when ERG cannot be covered", () => {
    const box1 = makeBox("b1", 1_000_000n);

    expect(() => selectInputs([box1], { nanoErgs: 2_000_000n })).toThrowError(
      new GluonError("INSUFFICIENT_FUNDS", "insufficient funds for ERG: needed 2000000, available 1000000"),
    );
  });

  it("throws INSUFFICIENT_FUNDS when change cannot be padded to SAFE_MIN_BOX_VALUE", () => {
    // Box 1 has 1_500_000 nanoERG, target needs 1_000_000 nanoERG.
    // Change would be 500_000 nanoERG, but no other box exists to pad it to 1_000_000.
    const box1 = makeBox("b1", 1_500_000n);

    expect(() => selectInputs([box1], { nanoErgs: 1_000_000n })).toThrowError(
      new GluonError(
        "INSUFFICIENT_FUNDS",
        `insufficient funds for change box: change is 500000 nanoERG, need at least ${SAFE_MIN_BOX_VALUE}`,
      ),
    );
  });
});
