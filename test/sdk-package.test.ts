import { existsSync, readFileSync } from "node:fs";
import { resolve } from "node:path";
import { describe, expect, it } from "vitest";

const root = resolve(import.meta.dirname, "..");
const sdk = resolve(root, "sdk");

describe("SDK Package & Documentation (A6)", () => {
  it("sdk/package.json has correct metadata, version 2.0.0-next.1, and exports", () => {
    const pkg = JSON.parse(readFileSync(resolve(sdk, "package.json"), "utf8"));
    expect(pkg.name).toBe("@stability-nexus/gluon-ergo-sdk");
    expect(pkg.version).toBe("2.0.0-next.1");
    expect(pkg.license).toBe("GPL-3.0-only");
    expect(pkg.type).toBe("module");
    expect(pkg.publishConfig).toEqual({ access: "public", tag: "next" });
    expect(pkg.exports["."].types).toBe("./dist/index.d.ts");
    expect(pkg.exports["."].import).toBe("./dist/index.js");
  });

  it("sdk/README.md contains the required v3-only notice, links, flow, Nautilus signing, and all 14 error codes", () => {
    const readme = readFileSync(resolve(sdk, "README.md"), "utf8");

    // Notice at the top
    expect(readme).toMatch(/2\.x supports Gluon v3 reactors only/i);
    expect(readme).toMatch(/npm install gluon-ergo-sdk@1/);
    expect(readme).toContain("../README.md#the-contract");
    expect(readme).toContain("../Deployments.md");

    // Sections
    expect(readme).toContain("## Installation");
    expect(readme).toContain("## Choosing a Deployment");
    expect(readme).toContain("## Client: Snapshot, Quote, and Build Flow");
    expect(readme).toContain("## EIP-12 Signing with Nautilus");
    expect(readme).toContain("## Error Codes");
    expect(readme).toContain("ergo.sign_tx");
    expect(readme).toContain("ergo.submit_tx");

    // All 14 error codes
    const errorCodes = [
      "REACTOR_UNHEALTHY",
      "WOULD_LEAVE_HEALTHY_RANGE",
      "REACTOR_HEALTHY",
      "ORACLE_STALE",
      "BOX_AMBIGUOUS",
      "BOX_NOT_FOUND",
      "BUYBACK_UNAVAILABLE",
      "AMOUNT_TOO_SMALL",
      "AMOUNT_TOO_LARGE",
      "INSUFFICIENT_FUNDS",
      "INVALID_ADDRESS",
      "UNKNOWN_DEPLOYMENT",
      "INVALID_DEPLOYMENT",
      "NODE_ERROR",
    ];
    for (const code of errorCodes) {
      expect(readme).toContain(`\`${code}\``);
    }
  });

  it("sdk/CHANGELOG.md contains the initial entry for 2.0.0-next.1", () => {
    const changelog = readFileSync(resolve(sdk, "CHANGELOG.md"), "utf8");
    expect(changelog).toContain("## 2.0.0-next.1");
    expect(changelog).toMatch(/Initial pre-release/i);
  });

  it("root README.md contains the SDK section, CD4 license note, and updated introduction", () => {
    const rootReadme = readFileSync(resolve(root, "README.md"), "utf8");
    expect(rootReadme).toContain("[Gluon Ergo SDK](sdk/)");
    expect(rootReadme).toContain("@stability-nexus/gluon-ergo-sdk");
    expect(rootReadme).toContain("## SDK");
    expect(rootReadme).toContain("npm run build:sdk");
    expect(rootReadme).toContain("npm run check:sdk");
    expect(rootReadme).toContain(".github/workflows/release.yml");
    expect(rootReadme).toContain("CD4");
  });

  it(".github/workflows/release.yml exists and matches the plan specification", () => {
    const workflowPath = resolve(root, ".github/workflows/release.yml");
    expect(existsSync(workflowPath)).toBe(true);
    const yaml = readFileSync(workflowPath, "utf8");
    expect(yaml).toContain('tags: ["sdk-v*"]');
    expect(yaml).toContain("environment: npm");
    expect(yaml).toContain("id-token: write");
    expect(yaml).toContain('test "$GITHUB_REF_NAME" = "sdk-v$(node -p "require(\'./sdk/package.json\').version")"');
    expect(yaml).toContain("npm install --global npm@11.20.0");
    expect(yaml).toContain("npm publish -w @stability-nexus/gluon-ergo-sdk");
  });
});
