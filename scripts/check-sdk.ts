import { execFileSync } from "node:child_process";
import { existsSync, mkdtempSync, readdirSync, readFileSync, rmSync, writeFileSync } from "node:fs";
import { tmpdir } from "node:os";
import { join, resolve } from "node:path";

const root = resolve(import.meta.dirname, "..");
const sdk = join(root, "sdk");
const dist = join(sdk, "dist");
const fail = (message: string): never => { throw new Error(`SDK check failed: ${message}`); };
for (const file of [join(dist, "index.js"), join(dist, "index.d.ts")]) if (!existsSync(file)) fail(`missing ${file}`);
const files = (directory: string): string[] => readdirSync(directory, { withFileTypes: true }).flatMap((entry) => entry.isDirectory() ? files(join(directory, entry.name)) : [join(directory, entry.name)]);
const sourceFiles = files(dist).filter((file) => /\.(?:js|d\.ts)$/.test(file));
const sdkPackage = JSON.parse(readFileSync(join(sdk, "package.json"), "utf8")) as { dependencies: Record<string, string> };
const rootPackage = JSON.parse(readFileSync(join(root, "package.json"), "utf8")) as { dependencies: Record<string, string> };
for (const file of sourceFiles) {
  const text = readFileSync(file, "utf8");
  if (/from\s+["']node:|\b(?:Buffer|process)\b/.test(text)) fail(`${file} is not browser-safe`);
  for (const match of text.matchAll(/(?:from\s+["']|import\s*["'])([^"']+)/g)) {
    const name = match[1]; if (name.startsWith(".") || name.startsWith("node:")) continue;
    const dependency = name.startsWith("@") ? name.split("/").slice(0, 2).join("/") : name.split("/")[0];
    if (!(dependency in sdkPackage.dependencies)) fail(`${dependency} is not declared by sdk/package.json`);
    if (rootPackage.dependencies[dependency] !== undefined && rootPackage.dependencies[dependency] !== sdkPackage.dependencies[dependency]) fail(`${dependency} version differs from the root package`);
  }
}
const packed = JSON.parse(execFileSync("npm", ["pack", "--dry-run", "--json", "-w", "@stability-nexus/gluon-ergo-sdk"], { cwd: root, encoding: "utf8" })) as [{ files: { path: string }[] }];
for (const file of packed[0].files.map((entry) => entry.path)) if (!["package.json", "README.md", "LICENSE", "CHANGELOG.md"].includes(file) && !file.startsWith("dist/")) fail(`unexpected packed file ${file}`);
const temp = mkdtempSync(join(tmpdir(), "gluon-sdk-"));
try {
  const tarball = JSON.parse(execFileSync("npm", ["pack", "--json", "-w", "@stability-nexus/gluon-ergo-sdk"], { cwd: root, encoding: "utf8" })) as [{ filename: string }];
  const archive = join(root, tarball[0].filename);
  execFileSync("npm", ["install", "--ignore-scripts", archive], { cwd: temp, stdio: "inherit" });
  execFileSync("node", ["--input-type=module", "-e", 'import("@stability-nexus/gluon-ergo-sdk")'], { cwd: temp, stdio: "inherit" });
  writeFileSync(join(temp, "sdk-types.ts"), `import { math, readReactor } from "@stability-nexus/gluon-ergo-sdk"; void math; void readReactor;`);
  execFileSync(process.execPath, [join(root, "node_modules", "typescript", "bin", "tsc"), "--noEmit", "--module", "NodeNext", "--moduleResolution", "NodeNext", "--skipLibCheck", "false", "--target", "ES2022", "sdk-types.ts"], { cwd: temp, stdio: "inherit" });
  rmSync(archive, { force: true });
} finally { rmSync(temp, { recursive: true, force: true }); }