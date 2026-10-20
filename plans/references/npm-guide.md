# Guide: a new npm package for the Gluon SDK, published from GitHub Actions with trusted publishing

*For the Gluon maintainers. Written on 2026-10-13 from npm's documentation of that day (sources at the end). It sets up
the package that `plans/UI-SDK-v3-migration.md` builds in the contracts repository and publishes from it.*

## 0. What you end up with

- An npm organization, `stability-nexus`, which owns the scope `@stability-nexus`. Only its members can publish under that
  scope, so nobody else can take a name in it, and the owner is visible on every package page.
- A public package, `@stability-nexus/gluon-ergo-sdk`, licensed GPL-3.0-only. Its first version is published by hand,
  once. Every later version is published by one GitHub Actions workflow through npm **trusted publishing**: no npm token
  exists anywhere, and each version carries a provenance attestation that links it to the commit and the workflow run
  that built it.
- Publishing by a person requires two-factor authentication, and tokens cannot publish at all.
- The old package, `gluon-ergo-sdk` (1.x, for the v2 contract), is not touched.

The versions start at 2.0.0, continuing the line of `gluon-ergo-sdk` 1.x under the new name, so that "SDK 2.x" means the
same thing everywhere in the plans.

## 1. Values used in this guide

| | |
|---|---|
| npm organization and scope | `stability-nexus`, `@stability-nexus` |
| Package | `@stability-nexus/gluon-ergo-sdk` |
| Repository that publishes | `StabilityNexus/Gluon-Ergo-Contracts` |
| Folder of the package in it | `sdk/`, an npm workspace |
| Workflow file | `.github/workflows/release.yml` |
| GitHub environment | `npm` |
| Release tags | `sdk-v<version>`, e.g. `sdk-v2.0.0-next.1` |
| First version, published by hand | `2.0.0-next.0` |

If the name `stability-nexus` is already taken on npm, choose another organization name and replace `@stability-nexus` in
this guide and in the migration plan.

## 2. Before you start

- **At least two maintainers**, each with an npm account (https://www.npmjs.com/signup) and two-factor authentication
  enabled. Set it up on the website (profile picture → **Account** → **Two-Factor Authentication**), preferably with a
  security key (WebAuthn: a hardware key, Touch ID, Windows Hello), which npm recommends doing on the web. Keep the
  recovery codes somewhere safe. With two owners, the package never depends on one person's devices.
- **npm 11.20.0 or newer** on the machine that runs the commands below (`npm --version`; to update,
  `npm install --global npm@11.20.0`), and **Node 24**, the version in the repository's `.nvmrc`. `npm trust` needs npm
  11.15.0 or newer.
- **A public publishing repository.** npm generates provenance only for public repositories.
- **The package's `package.json`, ready** (`sdk/package.json`, created in step A1 of the migration plan), with:
  - `"name": "@stability-nexus/gluon-ergo-sdk"`;
  - `"publishConfig": { "access": "public", "tag": "next" }`: a scoped package is private unless published with public
    access, and pre-releases must not take the `latest` tag;
  - `"repository": { "type": "git", "url": "git+https://github.com/StabilityNexus/Gluon-Ergo-Contracts.git",
    "directory": "sdk" }`. npm requires `repository.url` to match the GitHub repository that publishes.

## 3. Create the organization

1. Sign in on https://www.npmjs.com with one of the maintainers' accounts.
2. Profile picture (top right) → **Add Organization**.
3. **Name:** `stability-nexus`. The organization's name is also its scope: its packages are named
   `@stability-nexus/<name>`.
4. Plan: **Unlimited public packages**, which is free → **Create**.
5. Invite the other maintainers by npm username or email address → **Invite** → **Continue**.
6. In the organization's members list, give at least two people the **owner** role.

## 4. Publish the first version by hand

A trusted publisher can only be added to a package that exists: `npm trust` refuses a package that has never been
published, and the website's settings exist only for a published package. So one version is published from a
maintainer's machine: `2.0.0-next.0`, the version the package has after step A1 of the migration plan. It is a
pre-release under the `next` tag, and nobody needs to install it.

```sh
# a clean checkout of the commit to publish
git clone https://github.com/StabilityNexus/Gluon-Ergo-Contracts.git gluon-release
cd gluon-release
git checkout <commit>              # main, once A1 is merged
npm ci --ignore-scripts
npm run typecheck && npm test && npm run verify
npm run build:sdk && npm run check:sdk
npm login                          # opens the browser: sign in with 2FA
cd sdk
npm publish                        # public, under the next tag (publishConfig); asks for 2FA
```

Then check:
- `npm view @stability-nexus/gluon-ergo-sdk versions` lists `2.0.0-next.0`;
- `npm dist-tag ls @stability-nexus/gluon-ergo-sdk` shows `next: 2.0.0-next.0`. npm's documentation does not say
  whether the first publication of a package under another tag also creates `latest`; either way it is harmless until
  2.0.0;
- https://www.npmjs.com/package/@stability-nexus/gluon-ergo-sdk shows the package as public.

This one version has no provenance, which only CI can produce. Every later version has it.

## 5. Add the trusted publisher

Do this when the first release from CI is ready (step A6 of the migration plan): npm requires a new trusted publisher
to be used for a successful publication within 2 days of its creation.

**On the website:**
1. https://www.npmjs.com → the package → **Settings** → **Trusted Publisher** → **GitHub Actions**.
2. Fill in:
   - **Organization or user:** `StabilityNexus`;
   - **Repository:** `Gluon-Ergo-Contracts`;
   - **Workflow filename:** `release.yml`, the file's name only, with its extension; the file must be in
     `.github/workflows/`;
   - **Environment name:** `npm`;
   - **Allowed actions:** publish (or stage publish only: see §9).
3. Save. npm asks for 2FA.

**Or from the command line** (npm 11.15.0 or newer; 2FA):

```sh
npm trust github @stability-nexus/gluon-ergo-sdk \
  --repo StabilityNexus/Gluon-Ergo-Contracts --file release.yml --env npm --allow-publish
npm trust list @stability-nexus/gluon-ergo-sdk
```

`npm trust` does not know about workspaces, which is why the package is named explicitly.

## 6. Lock the package down

1. The package → **Settings** → **Publishing access** → **Require two-factor authentication and disallow tokens** →
   **Update Package Settings** (2FA). From then on no token can publish, whatever its settings. Trusted publishing is
   not affected, and a maintainer can still publish by hand, with 2FA.
2. Profile picture → **Access Tokens**: delete any token that was created for this package. None is needed.
3. From time to time, review the organization's members and their roles, and remove anyone who no longer maintains the
   package.

## 7. The GitHub side

1. **The environment.** In the publishing repository: **Settings** → **Environments** → **New environment**, named
   `npm`. Under **Deployment branches and tags**, choose **Selected branches and tags** and add a rule for the tags
   `sdk-v*`. Optionally, add the maintainers as **Required reviewers**, so that each run waits for a
   maintainer's approval before it publishes.
2. **The workflow.** `.github/workflows/release.yml` (it is also in `plans/UI-SDK-v3-migration.md` §7):

   ```yaml
   name: Release the SDK

   on:
     push:
       tags: ["sdk-v*"]

   permissions:
     contents: read

   concurrency:
     group: release-sdk
     cancel-in-progress: false

   jobs:
     publish:
       runs-on: ubuntu-24.04
       timeout-minutes: 20
       environment: npm
       permissions:
         contents: read
         id-token: write   # npm trusted publishing (OIDC), which also signs the provenance
       steps:
         - uses: actions/checkout@3d3c42e5aac5ba805825da76410c181273ba90b1 # v7.0.1
           with:
             # scripts/verify.ts recompiles each deployment from the commit it was deployed from.
             fetch-depth: 0
         - uses: actions/setup-node@949feb2413d6458794dcd2491c4babbbce0c15c1 # v7.1.0
           with:
             node-version-file: .nvmrc
             registry-url: https://registry.npmjs.org
         - name: The tag must match the package's version
           run: test "$GITHUB_REF_NAME" = "sdk-v$(node -p "require('./sdk/package.json').version")"
         - name: npm 11.20.0 (trusted publishing needs 11.5.1 or newer)
           run: npm install --global npm@11.20.0 && npm --version
         - run: npm ci --ignore-scripts
         - run: npm run typecheck
         - run: npm test
         - run: npm run verify
         - run: npm run build:sdk
         - run: npm run check:sdk
         - run: npm publish -w @stability-nexus/gluon-ergo-sdk
   ```

   What makes trusted publishing work in it:
   - the job has `permissions: id-token: write` and runs on a GitHub-hosted runner (npm does not support self-hosted
     runners for this);
   - the job's `environment` is `npm`, the same name as in the trusted publisher;
   - npm is 11.5.1 or newer: the workflow installs npm 11.20.0 before it publishes;
   - `actions/setup-node` has `registry-url: https://registry.npmjs.org`;
   - there is no `NODE_AUTH_TOKEN` and no npm token secret anywhere.

## 8. Release

For each version:
1. Set the version in the package's `package.json` (for example `2.0.0-next.1`), add an entry to `CHANGELOG.md`,
   commit, and push to `main`.
2. Tag that commit, and push the tag:

   ```sh
   git tag sdk-v2.0.0-next.1
   git push origin sdk-v2.0.0-next.1
   ```
3. The workflow runs, after the environment's approval if you set one up. It checks that the tag matches the version,
   runs the checks, builds, and publishes.
4. Check: `npm view @stability-nexus/gluon-ergo-sdk@next version` prints the new version; the package page shows its
   provenance, with a link to the workflow run; and, in a project that installs the package, `npm audit signatures`
   verifies the registry's signatures and the attestation.

**2.0.0 and `latest`.** In the commit that releases 2.0.0 (the plans say when), remove `"tag": "next"` from
`publishConfig`, so that 2.0.0 becomes `latest`. Put it back for the next pre-releases. An owner can also move a tag by
hand, with 2FA: `npm dist-tag add @stability-nexus/gluon-ergo-sdk@<version> latest`.

## 9. Optional: approve each release on npm (staged publishing)

npm can hold a new version in a staging area until a maintainer approves it with 2FA; only then does it go live. With
this, neither a compromised workflow nor a stolen GitHub account can publish a version on its own.
- In the trusted publisher, allow **stage publish** only (`--allow-stage-publish` instead of `--allow-publish`).
- In the workflow, replace the last step with `npm stage publish`, run in the package's folder (`working-directory:
  sdk`).
- To approve: the package on npmjs.com → **Staged Packages** → review → **Approve** (2FA). Or from the command line:
  `npm stage list @stability-nexus/gluon-ergo-sdk`, `npm stage view <stage-id>`, `npm stage approve <stage-id>`.
- It needs npm 11.15.0 or newer, in CI and on the approver's machine, and Node 22.14.0 or newer.
- npm's documentation does not yet say how a staged version is tagged, or whether it keeps its provenance. On the first
  staged release, check `npm stage view <stage-id>` before approving, and `npm dist-tag ls` and the package page after.

## 10. Day-two operations

- **A maintainer joins or leaves:** the organization's members list. Leaving the organization removes the right to
  publish.
- **The publishing repository moves or is renamed:** update `repository.url` in the package's `package.json`, and
  replace the trusted publisher: `npm trust list`, `npm trust revoke --id <id>`, then `npm trust github …` again. The
  same applies if the workflow file is renamed.
- **A bad version:** publish a fixed version, then deprecate the bad one:
  `npm deprecate @stability-nexus/gluon-ergo-sdk@<version> "<reason>; use <fixed version>"`. npm lets you unpublish a
  version within 72 hours if no other public package depends on it, and later only in narrow cases; a version number
  can never be reused.
- **Lost 2FA devices:** use the recovery codes. This is also why there are two owners.

## 11. Troubleshooting

| Symptom | Likely cause and fix |
|---|---|
| The workflow's publication fails with an authentication or "not found" error | The trusted publisher does not match the run (organization, repository, workflow file name, environment), the job lacks `id-token: write`, or npm is older than 11.5.1. |
| npm rejects the publication because of the repository | `repository.url` in `package.json` does not match the publishing repository exactly. |
| The first publication from CI fails although the trusted publisher looks right | It was created more than 2 days before its first use: create it again. |
| `npm trust` says the package does not exist | Publish the first version by hand first (§4). |
| `npm publish` by hand fails with `402 Payment Required` | A scoped package was published as private: keep `"access": "public"` in `publishConfig`, or pass `--access public`. |
| `npm publish` by hand asks for a one-time password, or fails without one | 2FA is required for publishing: answer the prompt, or pass `--otp=<code>`. |

## Sources

- Trusted publishing: https://docs.npmjs.com/trusted-publishers
- The `npm trust` command: https://docs.npmjs.com/cli/v11/commands/npm-trust
- Staged publishing: https://docs.npmjs.com/staged-publishing
- Creating an organization: https://docs.npmjs.com/creating-an-organization
- Creating and publishing scoped public packages: https://docs.npmjs.com/creating-and-publishing-scoped-public-packages
- Publishing access: https://docs.npmjs.com/requiring-2fa-for-package-publishing-and-settings-modification
- Two-factor authentication: https://docs.npmjs.com/configuring-two-factor-authentication
- Unpublish policy: https://docs.npmjs.com/policies/unpublish
