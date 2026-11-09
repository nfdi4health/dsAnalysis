# DataSHIELD analysis project

<!-- Replace this paragraph with what this analysis is about. -->

This project was set up with
[dsAnalysis](https://github.com/FlorianSchw/dsAnalysis). It runs the
analysis on the live DataSHIELD servers, or locally on mock data for
testing.

## Before you start: `.Renviron` and credentials

`.Renviron` in this folder holds everything that must not be shared:

- the servers' URLs, users and passwords, which `R/01_DS_Login.R` reads
  with `Sys.getenv()`;
- `R_CONFIG_ACTIVE`, which decides whether you work on the live servers
  (`'production'`) or in testing mode (`'testing'`).

R reads it when it starts: **restart R after every change**.

**Keep credentials out of GitHub:**

- `.Renviron` is in `.gitignore`, so git never commits it. Don't remove
  that line and don't force-add the file. Each person working on the
  project keeps their own `.Renviron`.
- Never write passwords, user names or tokens into `R/01_DS_Login.R` or
  any other script. Scripts are committed.
- Treat anything pushed to GitHub as public, even in a private
  repository: its history keeps every version. If a password was ever
  pushed, **change the password** — deleting the file or the commit is
  not enough.
- Real data never goes into this project. The only data in it is
  generated mock data in `utils/mock_data/`; `results/` is ignored by git
  except for its placeholders.

## Running the analysis

Open the project in RStudio. The first time, if R reports that packages
are missing, run `renv::restore()` once to install the versions this
project uses. Then run `R/main.R`. Where it logs in depends on
`R_CONFIG_ACTIVE` in `.Renviron`:

- `'production'`: the live servers, via `R/01_DS_Login.R`;
- `'testing'`: a local DSLite instance with mock data, via
  `utils/setup/01_DSLite_Setup.R`. No server or credentials needed.

## Packages

To use another DataSHIELD package, install it with dsAnalysis rather
than `install.packages()`: `dsAnalysis::install_dsPackage("dsSurvival")`
installs the client package too, adds the package to the testing setup
and records it for everyone in `renv.lock`.
`dsAnalysis::list_dsPackages()` shows which packages exist. If R
reports missing or changed packages (e.g. after pulling changes),
`dsAnalysis::check_project()` explains what is out of sync and offers to
fix it.

## Analysis scripts from the plan

`config/analysis-plan.yml` describes the analysis: the studies, the
variables and the steps. Each push that changes it runs the workflow in
`.github/workflows/datashield-analysis-suggest.yml`, which drafts a
script per step, tests them on mock data and proposes them as a pull
request. The scripts are a starting point: read and adapt them.

- **What is sent to the Claude API:** the plan's content (variable
  names, categories, study names, steps). Don't put anything
  confidential in it.
- **What is never sent:** your data, credentials and `R/01_DS_Login.R`.
- **No server credentials on GitHub:** the workflow runs in testing mode
  on mock data. It only needs the Anthropic secrets `ANTHROPIC_ORG_ID`,
  `ANTHROPIC_SVAC_ID` and `ANTHROPIC_FDRL_ANALYSIS`, and "Allow GitHub
  Actions to create and approve pull requests" (Settings → Actions →
  General). Setup details are in the
  [dsAnalysis README](https://github.com/FlorianSchw/dsAnalysis#setup).

To try the bot's scripts: check out its pull request's branch, set
`R_CONFIG_ACTIVE = 'testing'`, restart R and run `R/main.R`.

## Help

Questions, problems, or want help with the setup?
[Open an issue on dsAnalysis](https://github.com/FlorianSchw/dsAnalysis/issues).
