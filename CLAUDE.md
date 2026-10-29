# Project: dsAnalysis

## What this package is

An R package that creates DataSHIELD **analysis project** environments
(`initProject()`): a project folder with a login script, a DSLite test
setup with mock data, a `main.R`, `dependencies.R` and a `config.yml`
with a `production` and a `testing` profile. Exported functions:
`initProject()`, `initMockData()`, `update_MockData()`, and for
DataSHIELD packages `add_dsPackage()`, `remove_dsPackage()`,
`install_dsPackage()`, `list_dsPackages()`, `sync_dsPackages()`,
`check_project()`. Templates live in `inst/templates/` (`datashield/`,
`dslite/`, `utils/`).

It is also an **analyst-side helper package** in DataSHIELD terms — not
a client, not a server package — and the starting point for the
reusable workflow `datashield-analysis-suggest.yml` in
`FlorianSchw/package-workflows` (local clone:
`../package-workflows`), which drafts analysis scripts from a project's
`config/analysis-plan.yml`.

## How to work here

- Don't commit or push unless asked; leave changes in the working tree
  for review.
- One function per file in `R/`, file named after the function — helpers
  too. Functions that aren't exported are named `internal_…()` (file
  `R/internal_….R`), so they stand out (since 2026-10-04).
- Branches: work goes into `dev` (pull requests from feature branches);
  `main` only gets releases, through a `dev` → `main` pull request
  (`release-trigger.yml` checks, merges and starts `release-publish.yml`,
  semantic-release with the shared config from `package-workflows`). The
  bots' pull requests and sweeps target `dev`. Commits follow
  Conventional Commits (`commitlint.yml`), since they decide the version.
- When something depends on `package-workflows`, read the referenced file
  there instead of guessing; its `CLAUDE.md` and `dev-notes/` hold the
  design.

## To do

Agreed in the package-workflows sessions (2026-09-30 to 2026-10-02).
Cross items out when done instead of deleting them.

### A. Bot suggestions for this package (roxygen and tests)

1. ~~**Caller files** `.github/workflows/roxygen-suggest.yml` and
   `.github/workflows/test-suggest.yml`~~ (done 2026-10-02), starting from
   `../package-workflows/examples/roxygen-suggest.yml` and
   `../package-workflows/examples/test-suggest.yml`. Adjust:
   - `pull_request: branches: [dev]` (changed from `main` on 2026-10-02,
     when `dev` was added), with `paths: ['R/**']`;
   - `with: datashield: true`, `datashield-type: utility` (no
     `sweep-base`: scheduled sweeps use `dev`);
   - permissions `id-token: write`, `contents: write`,
     `pull-requests: write`, `issues: write`;
   - the `keepalive` job from the examples (scheduled workflow);
   - secrets `ANTHROPIC_ORG_ID`, `ANTHROPIC_SVAC_ID` and
     `ANTHROPIC_FDRL_ROXYGEN` / `ANTHROPIC_FDRL_TESTS`, plus optional
     `APP_CLIENT_ID` / `APP_PRIVATE_KEY`.
   Optional inputs (comma-separated strings): `accept-reasons`,
   `code-issue-confidence` (`high`, `high,medium`, … or `none`),
   `dslite-setup` (tests: `auto`, `create`, `never`). The inputs are
   documented in `../package-workflows/.github/workflows/*.yml`.
2. ~~**DESCRIPTION:**~~ (done 2026-10-02) `initMockData()` and `update_MockData()` use
   DataSHIELD connections, so their tests run against DSLite. Add
   `DSLite` and `dsBase` to `Suggests`, and `datashield/dsBase` to
   `Remotes` (not on CRAN).
3. ~~**Outside the repo (the user):** in the Anthropic Console a service
   account for dsAnalysis and one federation rule per workflow (claim
   `job_workflow_ref` naming the workflow file); the repository secrets;
   the GitHub App or "Allow GitHub Actions to create and approve pull
   requests"~~ (done). See `../package-workflows/docs/claude-setup.qmd` and
   `docs/getting-started.qmd`.

Test setup: nothing to change. `tests/testthat/setup.R` reads
`config-testing.yml` and doesn't connect, so the test workflow
(`dslite-setup: auto`) creates its own `setup-dslite.R` next to it.

### B. The analysis template (`initProject()`)

Checklist from `../package-workflows/dev-notes/analysis-suggest.md`,
section "What dsAnalysis needs":

4. ~~Remove the placeholders~~ (done 2026-10-02) `inst/templates/datashield/02_QualityCheck.R`,
   `03_DescriptiveStatistics.R` and `99_DSLiteLearning.R` (and their
   copying in `initProject()`). `99_DSLiteLearning.R` was restored on
   request: keep it (`99_` scripts don't clash with the bot's `02_`, …).
5. ~~Add a plan template~~ (done 2026-10-02) that `initProject()` puts at
   `config/analysis-plan.yml`, from
   `../package-workflows/examples/analysis-plan.yml`, keeping its header
   that the content is sent to the Claude API.
6. ~~Add the caller `.github/workflows/datashield-analysis-suggest.yml`~~ (done 2026-10-02) to
   the project template, from
   `../package-workflows/examples/datashield-analysis-suggest.yml`.
7. ~~Optional: empty marked blocks the bot fills~~ (done 2026-10-02) —
   - in `main.R`:
     `#### bot-suggest: scripts (updated by datashield-analysis-suggest)`
     … `#### bot-suggest: scripts end`;
   - in `dependencies.R`:
     `#### bot-suggest: packages (updated by datashield-analysis-suggest)`
     … `#### bot-suggest: packages end`.
   Without them the bot appends its own. The marker texts are the
   `markers:` settings in `../package-workflows/config/analysis-suggest.yml`.
8. ~~The project's `.gitignore` must not exclude `utils/mock_data/`~~ (holds, tested 2026-10-02) — the
   bot commits its mock data there.

**Interface — keep stable** (the bot reads and edits these; paths in
the `paths:` settings of `config/analysis-suggest.yml`):
- the step markers `#### Step 1: …` to `#### Step 7: …` in
  `inst/templates/dslite/01_DSLite_Setup.R` (used by `add_dsPackage()` /
  `update_MockData()`; the bot looks for `#### Step 2: Import of mock
  data files`);
- exactly one `symbol = "..."` in that login call, and the
  `<name> <<- DSI::datashield.login(` form — the bot sets the symbol to
  the plan's and may append an alias line
  `<name> <<- conns  # bot-suggest: ...`;
- the `production` / `testing` profiles in the project `config.yml`.

### C. Functions

9. ~~**Bug in `add_dsPackage()`**~~ (fixed 2026-10-02: early return) (`R/add_dsPackage.R`): when every given
   package is already present, `new_dsPackage_length` (line ~73) is 0,
   and the `1:new_dsPackage_length` loops (lines ~91, ~112, ~144) run
   for 1 and 0 — they write `library(Client)` and break block 4. It
   should be a no-op (`seq_len()`, or return early).
10. ~~`update_MockData()` suits CI~~ (checked 2026-10-02, unchanged): pass `table_names` so it doesn't parse
    `01_DS_Login.R`; each `.rda` holds an object named after its server
    (`study1.rda` → `study1`). Keep it that way.
17. ~~**Citation script**~~ (done 2026-10-02; `renv::init()` picks up
    `grateful` from the script itself, tested via `renv.lock`) in the project template, as in mepr
    (`../mepr/inst/templates/scripts/99_package_citations.R`, copied by
    `../mepr/R/initialize_project.R` to `R/99_package_citations.R`): it
    writes the citations of the packages used into `citations/` with
    `grateful::cite_packages()`. Bring it into `initProject()`, and make
    sure `grateful` is installed in the project (`dependencies.R`).

### D. README

11. ~~A data protection note~~ (done 2026-10-02): the analysis plan's content (variable names,
    categories, study names, steps) goes to the Claude API; data,
    credentials and `R/01_DS_Login.R` never do.
12. ~~A setup guide for analysis projects~~ (done 2026-10-02): the three secrets
    (`ANTHROPIC_ORG_ID`, `ANTHROPIC_SVAC_ID`, `ANTHROPIC_FDRL_ANALYSIS`)
    and "Allow GitHub Actions to create and approve pull requests".
13. ~~How to try the scripts~~ (done 2026-10-02): `R_CONFIG_ACTIVE = 'testing'` in
    `.Renviron`, restart R, run `R/main.R`.

### E. Later

14. ~~`DESCRIPTION`: CRAN releases instead of~~ (done 2026-10-02: dsBaseClient and dsBase 6.3.5 on CRAN; dsSupportClient moved to nfdi4health/dsSupportClient)
    `Remotes: datashield/dsBaseClient`, once available.
15. ~~`.github/workflows/metadata_extraction.yaml` calls~~ (dropped 2026-10-02)
    `FlorianSchw/datashield-workflows@master`, which is to be removed —
    replace or drop it then.
16. ~~Optional: `R-CMD-check.yaml` (r-lib's, daily) could become a short~~ (done 2026-10-02)
    caller of `../package-workflows/.github/workflows/r-cmd-check.yml`.
18. **First release** (`dev` → `main` flow, set up 2026-10-02): remove
    `dry-run: true` from `.github/workflows/release-publish.yml` once a
    dry run has shown the expected version. With no tags, semantic-release
    starts at `1.0.0`; to start at 0.x, tag `main` as `0.0.0` first (no `v`
    prefix). Outside the repo (the user): branch protection comes from
    the rulesets in `../repo-governance/rulesets/` (dsAnalysis added to the
    targets 2026-10-02; its `RULESET_ADMIN_PAT` must reach the
    `FlorianSchw` account, not only `nfdi4health`); the secret
    `API_TOKEN_GITHUB` of the old release workflows is no longer used.

### F. Package management, renv and README (agreed 2026-10-02)

Facts behind these items:
- The DataSHIELD package catalogue (`https://packages.datashield.org/packages.json`,
  built from FederatedMethods/packages) lists 72 packages; only 5 are on
  CRAN, almost all have `input.github_link`; `input.status` is
  production / development / retired / empty.
- Client names don't always follow `<server>Client` (`dsMTLBase` ↔
  `dsMTLClient`, `dsQueryLibrary` ↔ `dsQueryLibraryServer`), repo names
  can differ from package names (`molgenis/ds-tidyverse` for
  `dsTidyverse`), and entries can be stale (`sofiasiamp/dsSupportClient`).
- **The analysis bot loads all of dsAnalysis' `R/`** with `sys.source()`
  from a dsAnalysis checkout
  (`../package-workflows/R/functions/analysis/update_dslite_setup.R`;
  until 2026-10-04 only `add_dsPackage.R` and `update_MockData.R`) and
  calls `add_dsPackage(missing)` and `update_MockData()`. So files in
  `R/` must only define functions (one per file, the user's convention),
  and those two must stay usable without network installs in the bot's
  run.
- DSI 1.8.0 has `datashield.profiles(conns)`, `datashield.pkg_status(conns)`,
  and `builder$append(..., profile = )`.

19. ~~**`add_dsPackage()` writes `dependencies.R`:**~~ (done 2026-10-02; block markers in `R/add_dsPackage.R`, step 1/4 now parsed) `library(<server>)` and
    `library(<client>)` in a marked block of its own (e.g.
    `#### dsPackages (managed by add_dsPackage)` … `end`), never inside
    the bot's `#### bot-suggest: packages` block.
20. ~~**`remove_dsPackage()`:**~~ (done 2026-10-02; renv part with item 23) removes the package from step 1
    (`library()`) and step 4 (`include=c(...)`) of the DSLite setup and
    from the block in `dependencies.R`; refuses `dsBase`; uninstalling
    optional (`renv::remove()`). Rewrite step 4 by parsing the
    `include=c(...)` list instead of counting lines (cause of item 9).
21. ~~**Install source from the catalogue:**~~ (done 2026-10-02; `R/internal_resolve_dsPackage.R` and helpers, first in `R/dsPackage_sources.R`) analysts give only the
    package name. CRAN if `cran_link` is set, else the catalogue's
    `github_link` (`owner/repo`) via `renv::install()`; user override with
    `"owner/repo"`. The client from the catalogue's own entry, not by
    guessing the name. Fallback when the catalogue can't be reached: say
    so and accept an explicit `"owner/repo"`. The bot pairs server and
    client in package-workflows (`client_package_name.R`); keep the two
    approaches consistent.
22. ~~**`version` argument:**~~ (done 2026-10-02; without a version GitHub packages get their latest release tag, not the default branch (often a .9000 development version); the client gets the server's version if it has that tag, else its latest — dsSurvival server and client versions differ) CRAN archive (`pkg@1.2.3`) for CRAN packages,
    else the GitHub tag matching the version (tags are `v6.3.2` or
    `6.3.2` depending on the package: look them up), else stop and list
    the available versions; `ref =` as an escape hatch (commit or
    branch). Client and server versions should match the studies'.
23. ~~**renv handled by the functions:**~~ (done 2026-10-02: `install_dsPackage()`, `check_project()`, renv step in `remove_dsPackage()`; tried end to end in a scratch renv project with dsSurvival. Keep test projects on short paths on Windows: renv's staging folder hits the 260-character limit) analysts are not expected to know
    renv. Every install/remove runs install → `dependencies.R` →
    `renv::snapshot()` → `renv::status()` and reports in plain words.
    Plus `check_project()`: runs `renv::status()`, explains what is out
    of sync and offers the fix (`renv::restore()` / `renv::snapshot()`).
24. ~~**`list_dsPackages(search =, status =)`:**~~ (done 2026-10-02; column `github_version` = DESCRIPTION on the default branch, can be a development version) reads the live catalogue
    and returns name, description, status, client, CRAN/GitHub source,
    latest version, ending with the `add_dsPackage()` call to run. Same
    lookup as item 21. The README links the catalogue
    (packages.datashield.org, FederatedMethods/packages) and points to
    this function instead of listing packages.
25. ~~**DataSHIELD profiles**~~ (done 2026-10-02: `sync_dsPackages()`, profile comment in the login template; `pkg_status()` / `profiles()` tried on DSLite, which answers like a server; not yet tried on a real Opal/Armadillo server) (bundles of server packages: a Rock cluster
    in Opal, an image such as `default` / `xenon` in Armadillo; defined
    by the server admins, no central list): first research what
    `datashield.profiles()` / `datashield.pkg_status()` return on Opal
    and Armadillo (demo servers?). Then `sync_dsPackages(conns)`, which
    makes the DSLite setup and `dependencies.R` match production
    (uses 21–23), and an optional `profile = "..."` per server in the
    `01_DS_Login.R` template.
26. ~~**README (here and in the project template):**~~ (done 2026-10-02; template: `inst/templates/utils/README.md`)
    - `.Renviron`: holds server URLs, users, passwords and
      `R_CONFIG_ACTIVE`; `initProject()` puts it in `.gitignore` — never
      remove that or force-add it; restart R after editing.
    - Credentials: never in `R/01_DS_Login.R` or any committed file;
      anything pushed to GitHub counts as public, also in private repos
      (history keeps it) — if it happens, change the password, deleting
      the commit isn't enough. Real data never in the repo (only
      generated mock data in `utils/mock_data/`; `results/` ignored).
    - The GitHub workflow never needs server credentials (testing mode,
      DSLite, mock data): no DataSHIELD passwords in GitHub secrets, only
      the three Anthropic ones.
    - A step-by-step setup (install, `initProject()`, `.Renviron`,
      GitHub repo, secrets, "Allow GitHub Actions to create and approve
      pull requests"), and for help: open an issue on
      https://github.com/FlorianSchw/dsAnalysis/issues.
27. **Move the repository to `nfdi4health`** (planned, not before
    2026-10-02). The GitHub App was installed on `FlorianSchw/dsAnalysis`
    on 2026-10-02 (the token step had failed with 404: no installation
    for this repo). After the move, update:
    - `FlorianSchw/dsAnalysis` in `DESCRIPTION` (URL, BugReports),
      `README.md`, `inst/templates/utils/README.md` (issue links) and
      `R/initProject.R` (`renv::install("FlorianSchw/dsAnalysis")`);
    - `../repo-governance/rulesets/*.targets.json` (owner), and the
      GitHub App installation (on `nfdi4health` instead of `FlorianSchw`);
    - the repository secrets (they don't move with a transfer) and the
      Anthropic federation rules if they name the repository;
    - the catalogue entry's `github_link` (FederatedMethods/packages).
    GitHub redirects the old URLs, so nothing breaks at once.
28. ~~**`initProject()` must not touch the caller's library**~~ (fixed
    2026-10-02): it installed dsBaseClient, dsSupportClient and
    dsAnalysis into the active library before `renv::init()`. In CI that
    replaced the loaded dsBaseClient (6.3.6.9000 from dsSupportClient's
    `Remotes`) with CRAN's 6.3.5, and `library(dsBaseClient)` in
    `test-initMockData.R` failed; locally it corrupted `stringi`. Now
    `renv::init(bare = TRUE)`, then install / `renv::hydrate()` /
    `renv::snapshot()` into the project library. Checked: the active
    library is unchanged after `initProject()` (all package versions and install times). Local test
    runs need `GITHUB_PAT` (60 unauthenticated GitHub API calls per hour
    run out after about two full test runs).
29. ~~**`initMockData()` (then `initMockdata()`) ignores `datasources` in places:**~~ (fixed 2026-10-02, with the folder path, the per-server size of categorical values, and a DSLite test with DSLite's CNSIM data instead of the OBiBa demo login)
    `dsSupportClient::ds.summaryVars(df)` (and possibly further calls) are
    made without `datasources = datasources`, so they look for
    connections in the global environment ("Are you logged in to any
    server?" in `test-initMockData.R`, line 46, where `conns` is local).
    `test-initMockData.R` is also still work in progress (uses
    `error_message` and `cfg_dir_overwrite` without defining them, logs in
    to opal-demo).
