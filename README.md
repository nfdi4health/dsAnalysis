# dsAnalysis

dsAnalysis creates DataSHIELD **analysis project** environments. A new
project comes with a login script for the live servers, a DSLite test
setup with mock data, `renv`, and a `config.yml` to switch between the
two. A GitHub workflow can draft starter analysis scripts from an
analysis plan you write.

## Getting started

1. **Install dsAnalysis:**

   ``` r
   # install.packages("remotes")
   remotes::install_github("FlorianSchw/dsAnalysis")
   ```

2. **Create a project:**

   ``` r
   dsAnalysis::initProject(path = "~/projects", name = "my-analysis")
   ```

3. **Fill in `.Renviron`** in the new project with your servers' URLs,
   users and passwords, and restart R. See
   [`.Renviron` and credentials](#renviron-and-credentials) — read this
   before you put the project on GitHub.

4. **Try it locally** in testing mode, without any server: set
   `R_CONFIG_ACTIVE = 'testing'` in `.Renviron`, restart R and run
   `R/main.R`. See [Production and testing](#production-and-testing).

5. **Put the project on GitHub** (optional, but needed for the analysis
   scripts workflow): create a repository and push the project. Check
   first that `.Renviron` is not among the files to commit.

6. **Set up the analysis scripts workflow** (optional): see
   [Setup](#setup).

If you'd rather not set this up yourself, or get stuck,
[open an issue](https://github.com/FlorianSchw/dsAnalysis/issues).

## What a project contains

| Path | What it is |
|------|------------|
| `R/main.R` | Runs the analysis: logs in, then sources the analysis scripts |
| `R/01_DS_Login.R` | Login to the live DataSHIELD servers (production) |
| `R/99_DSLiteLearning.R` | Looking at the server-side data in testing mode |
| `R/99_package_citations.R` | Writes the citations of the packages used to `citations/` (with `grateful`) |
| `utils/setup/01_DSLite_Setup.R` | Login to a local DSLite instance with mock data (testing) |
| `utils/mock_data/` | Mock data for DSLite |
| `config.yml` | The `production` and `testing` profiles |
| `config/analysis-plan.yml` | Your analysis plan, read by the analysis scripts workflow |
| `.github/workflows/datashield-analysis-suggest.yml` | The analysis scripts workflow |
| `dependencies.R` | Packages tracked by `renv` |
| `.Renviron` | Credentials and the active profile; **never committed** |
| `README.md` | The project's own README, with these rules for everyone working on it |

## DataSHIELD packages

Which DataSHIELD packages exist, and where they come from, is listed in
the [DataSHIELD package catalogue](https://packages.datashield.org)
(maintained in
[FederatedMethods/packages](https://github.com/FederatedMethods/packages)).
Most of them are on GitHub, not on CRAN. You don't need to know where:
dsAnalysis looks it up in the catalogue. Run these functions from
within your project:

``` r
# browse the catalogue
list_dsPackages(search = "survival", status = "production")

# install a server package and its client, e.g. in the version your
# studies' servers run
install_dsPackage("dsSurvival")
install_dsPackage("dsSurvival", version = "2.1.3")

# remove it again (uninstall = TRUE also uninstalls it)
remove_dsPackage("dsSurvival")

# check that the installed packages match the project's renv.lock
check_project()

# after logging in to the live servers: install the packages (and
# versions) the servers have, and add them to the DSLite test setup
sync_dsPackages()
```

`install_dsPackage()` takes care of everything around it: the client
package that belongs to the server package, the DSLite test setup,
`dependencies.R`, and `renv.lock`, which records the exact versions so
that everyone working on the project gets the same ones. You don't need
to use `renv` yourself; if something gets out of sync (e.g. after
pulling changes from GitHub), `check_project()` tells you what and offers
to fix it.

A package that isn't in the catalogue can be installed from its GitHub
repository: `install_dsPackage("dsMyPackage", source = "owner/repo")`.

**Which packages do the servers have?** The data managers of each study
decide that. A server can offer several *DataSHIELD profiles*, each a
set of server packages; without a profile in the login, you get the
server's default one. `sync_dsPackages()` reads what the servers you
are logged in to offer, installs the matching packages locally (if the
servers differ in a version, the lowest, so that it works with all of
them) and tells you which profiles are in use. To log in with a
specific profile, add `profile = "<name>"` to the server's
`builder$append()` call in `R/01_DS_Login.R`.

Other helpers: `add_dsPackage()` and `remove_dsPackage()` only change the
DSLite setup and `dependencies.R` (no installing), and
`update_MockData()` points the DSLite setup at other mock data.

## `.Renviron` and credentials

`.Renviron` in the project folder holds everything that must not be
shared:

- the servers' URLs, users and passwords (`OBIBA1_URL`, `OBIBA1_USER`,
  `OBIBA1_PWD`, …), which `R/01_DS_Login.R` reads with `Sys.getenv()`;
- `R_CONFIG_ACTIVE`, which decides whether you work on the live servers
  or in testing mode.

R reads it when it starts: **restart R after every change**.

`initProject()` fills it with the public OBiBa demo servers, so a new
project works out of the box. Replace them with your own servers.

**Keep credentials out of GitHub:**

- `initProject()` adds `.Renviron` to `.gitignore`, so git never commits
  it. Don't remove that line and don't force-add the file.
- Never write a URL with a password, a user name with a password, or a
  token into `R/01_DS_Login.R` or any other script. Scripts are
  committed, and the login reads everything from `.Renviron` for exactly
  that reason.
- Treat anything you push to GitHub as public, even in a private
  repository: its history keeps every version. If a password was ever
  pushed, **change the password** — deleting the file or the commit is
  not enough.
- Real data never goes into the project. The only data in it is
  generated mock data in `utils/mock_data/`, and `results/` is ignored by
  git except for its placeholders.

## Production and testing

`R_CONFIG_ACTIVE` in the project's `.Renviron` decides where `R/main.R`
logs in:

- `R_CONFIG_ACTIVE = 'production'` (the default): the live servers, via
  `R/01_DS_Login.R`;
- `R_CONFIG_ACTIVE = 'testing'`: the local DSLite instance with mock
  data, via `utils/setup/01_DSLite_Setup.R`.

Restart R after changing it.

## Analysis scripts from your plan

Describe your analysis in `config/analysis-plan.yml`: the studies, the
variables and the steps. Each push that changes the plan runs the
`datashield-analysis-suggest` workflow from
[package-workflows](https://github.com/FlorianSchw/package-workflows).
It drafts a script per step, tests all of them on mock data built from
your variables, and proposes them as a pull request. The scripts are a
starting point: read and adapt them. The analysis stays your
responsibility.

Details:
[DataSHIELD analysis scripts](https://github.com/FlorianSchw/package-workflows/blob/main/docs/datashield-analysis-suggest.qmd).

### Data protection

The **content of the analysis plan is sent to the Claude API**: variable
names and labels, categories, study names and the steps. Don't put
anything confidential in the plan.

Your **data, credentials and `R/01_DS_Login.R` are never sent**. The
scripts are tested on generated mock data only.

The workflow **never needs your server credentials**: it runs in testing
mode, on DSLite with mock data. Don't put DataSHIELD users or passwords
into GitHub secrets — the only secrets it needs are the Anthropic ones
below.

### Setup

Once per project repository on GitHub:

1. **Claude access.** In the Anthropic Console, a service account for
   the repository and a federation rule for the workflow (claim
   `job_workflow_ref`, naming
   `FlorianSchw/package-workflows/.github/workflows/datashield-analysis-suggest.yml@refs/heads/main`).
   See
   [Claude setup](https://github.com/FlorianSchw/package-workflows/blob/main/docs/claude-setup.qmd).
2. **Repository secrets** (Settings → Secrets and variables → Actions):

   | Secret | Value |
   |--------|-------|
   | `ANTHROPIC_ORG_ID` | Your Anthropic organization ID |
   | `ANTHROPIC_SVAC_ID` | The service account of this repository |
   | `ANTHROPIC_FDRL_ANALYSIS` | The federation rule of the workflow |

3. **Pull requests.** Turn on "Allow GitHub Actions to create and
   approve pull requests" (Settings → Actions → General). Alternatively,
   set up a GitHub App with the secrets `APP_CLIENT_ID` and
   `APP_PRIVATE_KEY`; then the bot's pull requests also trigger your
   other checks.

No Anthropic account, or not sure about any of this?
[Open an issue](https://github.com/FlorianSchw/dsAnalysis/issues) and
we'll set it up with you.

### Trying the scripts

The bot's pull request adds its mock data to `utils/mock_data/` and
points the DSLite setup at it. To run the scripts the way the workflow
tested them:

1. Check out the pull request's branch.
2. Set `R_CONFIG_ACTIVE = 'testing'` in `.Renviron`.
3. Restart R.
4. Run `R/main.R`.

Set it back to `'production'` to run the analysis on the live servers.

## Help

Questions, problems, or want help with the setup?
[Open an issue](https://github.com/FlorianSchw/dsAnalysis/issues).
