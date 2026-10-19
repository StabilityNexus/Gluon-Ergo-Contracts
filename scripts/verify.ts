// Consistency checks, run by CI:
// - every deployment config is well formed and the values it sets are valid (TODO values are allowed);
// - every recorded deployment is reproduced: compiling the contract source at the recorded revision with the
//   recorded constants yields exactly the recorded ErgoTree, address and template hashes;
// - Deployments.md matches the records.
// It needs the full git history (in GitHub Actions, checkout with fetch-depth: 0).
// Usage: node scripts/verify.ts
import { execFileSync } from "node:child_process";
import { existsSync, readFileSync, readdirSync } from "node:fs";
import { run } from "../lib/cli.ts";
import { readDeployment, validateDeployment } from "../lib/config.ts";
import { DEPLOYMENTS_MD, NETWORKS, readRecords, renderDeployments, reproduce } from "../lib/records.ts";
import { SDK_DATA_TS, renderSdkData } from "./build-sdk-data.ts";

const ROOT = new URL("..", import.meta.url);

// Where the contract source has lived in the repository's history.
const SOURCES = [
  "contracts/Gluon.es",
  "modules/gluonw-base/src/main/resources/ErgoContracts/GluonW/Ergs/BoxGuardScripts/GluonWBoxGuardScript.es",
];

function sourceAt(revision: string, paths: string[]): string {
  for (const path of paths) {
    try {
      return execFileSync("git", ["show", `${revision}:${path}`], { cwd: ROOT, encoding: "utf8", stdio: ["ignore", "pipe", "ignore"] });
    } catch {
      // not at this path in that revision
    }
  }
  throw new Error(`no contract source at revision ${revision}; is the git history complete?`);
}

run(async () => {
  const problems: string[] = [];

  const configs = readdirSync(new URL("config/", ROOT))
    .filter((f) => f.endsWith(".json") && f !== "networks.json")
    .map((f) => `config/${f}`);
  for (const file of configs) {
    for (const problem of validateDeployment(readDeployment(new URL(file, ROOT)))) problems.push(`${file}: ${problem}`);
  }
  console.log(`checked ${configs.length} deployment configs`);

  for (const network of NETWORKS) {
    for (const d of readRecords(network).deployments) {
      if (!d.contract.revision || d.contract.repository) {
        console.log(`skipped ${network}/${d.name}: its source is ${d.contract.revision ? `in ${d.contract.repository}` : "unknown"}`);
        continue;
      }
      const source = sourceAt(d.contract.revision, d.contract.source ? [d.contract.source] : SOURCES);
      const mismatches = reproduce(d, source);
      if (mismatches.length > 0) problems.push(`${network}/${d.name}: does not reproduce the recorded ${mismatches.join(", ")}`);
      else console.log(`reproduced ${network}/${d.name} from ${d.contract.revision.slice(0, 7)}`);
    }
  }

  const current = existsSync(DEPLOYMENTS_MD) ? readFileSync(DEPLOYMENTS_MD, "utf8").replace(/\r\n/g, "\n") : "";
  if (current !== renderDeployments()) problems.push("Deployments.md is out of date: run node scripts/render-deployments.ts");

  const currentSdkData = existsSync(SDK_DATA_TS) ? readFileSync(SDK_DATA_TS, "utf8").replace(/\r\n/g, "\n") : "";
  if (currentSdkData !== renderSdkData()) problems.push("sdk/src/deployments/data.ts is out of date: run node scripts/build-sdk-data.ts");

  if (problems.length > 0) throw new Error(`\n- ${problems.join("\n- ")}`);
  console.log("all checks passed");
});
