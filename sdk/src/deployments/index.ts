import data from "./data.ts";
import { deploymentFromRecord } from "./fromRecord.ts";
import { GluonError } from "../errors.ts";
import type { GluonDeployment } from "../types.ts";

export { deploymentFromRecord } from "./fromRecord.ts";

export const DEPLOYMENTS: readonly GluonDeployment[] = Object.freeze(
  data.map((record) => deploymentFromRecord(record)),
);

export function getDeployment(name: string): GluonDeployment {
  const deployment = DEPLOYMENTS.find((d) => d.name === name);
  if (!deployment) {
    throw new GluonError("UNKNOWN_DEPLOYMENT", `no bundled deployment named ${name}`);
  }
  return deployment;
}
