// Regenerates Deployments.md from deployments/<network>.json. With --check, fails if it is out of date.
// Usage: node scripts/render-deployments.ts [--check]
import { existsSync, readFileSync, writeFileSync } from "node:fs";
import { parseArgs } from "node:util";
import { run } from "../lib/cli.ts";
import { DEPLOYMENTS_MD, renderDeployments } from "../lib/records.ts";

run(async () => {
  const { values } = parseArgs({ options: { check: { type: "boolean", default: false } } });
  const rendered = renderDeployments();
  if (values.check) {
    const current = existsSync(DEPLOYMENTS_MD) ? readFileSync(DEPLOYMENTS_MD, "utf8").replace(/\r\n/g, "\n") : "";
    if (current !== rendered) throw new Error("Deployments.md is out of date: run node scripts/render-deployments.ts");
    console.log("Deployments.md is up to date");
  } else {
    writeFileSync(DEPLOYMENTS_MD, rendered);
    console.log("wrote Deployments.md");
  }
});
