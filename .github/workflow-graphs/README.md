# Workflows

The GitHub Actions workflows of this repository, and how they connect.
The table and diagrams are generated from the workflow files by
[workflow-graphs](https://github.com/FlorianSchw/package-workflows) and
updated when the workflows change. For the details of a workflow, see
its file in `.github/workflows/`. Text outside the marked block is yours
and never overwritten.

<!-- workflow-graphs:overview:start -->
## All workflows

| Name | File | Runs on | Calls | Produces |
|---|---|---|---|---|
| DESCRIPTION Author Suggestions | `authors-suggest.yml` | PR into dev | nfdi4health/datashield-workflows › authors-suggest.yml@main | • Suggestion PR<br>• Comment on the PR — *only if: pull request* |
| Commitlint | `commitlint.yml` | PR into dev | nfdi4health/datashield-workflows › commitlint.yml@main | Check: commit messages follow Conventional Commits |
| R-CMD-check | `R-CMD-check.yaml` | • PR into dev<br>• Weekly (Mon)<br>• Manual run | • nfdi4health/datashield-workflows › r-cmd-check.yml@main<br>• nfdi4health/datashield-workflows › workflow-keepalive.yml@main | • Check: R CMD check passes<br>• Scheduled workflows kept enabled — *only if: scheduled run* |
| Roxygen Doc Suggestions | `roxygen-suggest.yml` | • PR into dev (only if: R/\*\* changed)<br>• Manual run | • nfdi4health/datashield-workflows › roxygen-suggest.yml@main<br>• nfdi4health/datashield-workflows › workflow-keepalive.yml@main | • Suggestion PR<br>• Comment on the PR — *only if: pull request*<br>• Scheduled workflows kept enabled — *only if: scheduled run* |
| Test Suggestions | `test-suggest.yml` | • PR into dev (only if: R/\*\* changed)<br>• Manual run | • nfdi4health/datashield-workflows › test-suggest.yml@main<br>• nfdi4health/datashield-workflows › workflow-keepalive.yml@main | • Suggestion PR<br>• Comment on the PR — *only if: pull request*<br>• Issue for a likely bug in the code<br>• Scheduled workflows kept enabled — *only if: scheduled run* |
| Cleanup Suggestion Branch | `cleanup-suggestion-branch.yml` | PR closed | nfdi4health/datashield-workflows › cleanup-suggestion-branch.yml@main | Bot branch deleted — *only if: github.event.pull_request.head.repo.full_name == github.repository && (startsWith(github.event.pull_request.head.ref, 'bot-suggest/') \|\| startsWith(github.event.pull_request.head.ref, 'agent-suggest/'))* |
| Release Issues | `release-issues.yml` | PR closed | nfdi4health/datashield-workflows › release-issues.yml@main | — |
| Release | `release-trigger.yml` | PR into main | • nfdi4health/datashield-workflows › r-cmd-check.yml@main<br>• nfdi4health/datashield-workflows › release-issues.yml@main<br>• nfdi4health/datashield-workflows › merge-pull-request.yml@main<br>• nfdi4health/datashield-workflows › create-issue.yml@main<br>• nfdi4health/datashield-workflows › trigger-release-publish.yml@main | • Check: R CMD check passes — *only if: PR comes from dev*<br>• PR merged — *only if: needs.check.result == 'success' && needs.link-issues.result == 'success'*<br>• Issue for the PR author — *if check fails*<br>• starts **Publish Release** |
| Publish Release | `release-publish.yml` | Dispatch: release-publish | nfdi4health/datashield-workflows › package-release.yml@main | Version commit, tag and draft release |
| Require Head Branch | `require-head-branch.yml` | PR into main | nfdi4health/datashield-workflows › require-head-branch.yml@main | Check: PR comes from dev |
| Suggestion Choices | `suggestion-choices.yml` | PR edited | nfdi4health/datashield-workflows › suggestion-choices.yml@main | Bot branch matches the checkboxes — *only if: (startsWith(github.head_ref, 'bot-suggest/tests/') \|\| startsWith(github.head_ref, 'bot-suggest/docs/')) && github.event.sender.type != 'Bot'* |
| Workflow Graphs | `workflow-graphs.yml` | • Push to dev (only if: .github/workflows/\*\* changed)<br>• Manual run | nfdi4health/datashield-workflows › workflow-graphs.yml@main | • Commit on the branch — *only if: push*<br>• Note on the PR: runs on push only — *only if: pull request* |
| Implementer Timer | `implementer-timer.yml` | Dispatch: implementer-timer | nfdi4health/datashield-workflows › implementer-timer.yml@main | — |
| Issue Label Check | `issue-label-check.yml` | issues | nfdi4health/datashield-workflows › issue-label-check.yml@main | Approval label removed, with a comment |
| Issue Triage | `issue-triage.yml` | • issues<br>• issue_comment | nfdi4health/datashield-workflows › issue-triage.yml@main | Triage comment on the issue |

## Events

What each event starts: the workflows (bold), what they produce (dashed boxes) and the workflows they start in turn (dotted arrows). **only if:** — happens only under this condition, e.g. when these files changed; **skipped:** — doesn't happen in this case. A label on an arrow applies to that arrow only ("if check fails").

### PR into dev

*When a pull request into dev is opened or updated.*

```mermaid
%%{init: {"flowchart": {"rankSpacing": 140, "nodeSpacing": 45}}}%%
flowchart LR
  ev(["PR into dev"])
  wf_authors_suggest_yml["<b>DESCRIPTION Author<br/>Suggestions</b>"]
  ev --> wf_authors_suggest_yml
  wf_commitlint_yml["<b>Commitlint</b>"]
  ev --> wf_commitlint_yml
  wf_R_CMD_check_yaml["<b>R-CMD-check</b>"]
  ev --> wf_R_CMD_check_yaml
  wf_roxygen_suggest_yml["<b>Roxygen Doc Suggestions</b><br/>only if: R/#42;#42; changed"]
  ev --> wf_roxygen_suggest_yml
  wf_test_suggest_yml["<b>Test Suggestions</b><br/>only if: R/#42;#42; changed"]
  ev --> wf_test_suggest_yml
  out_1(["Suggestion PR"]):::outcome
  out_2(["Comment on the PR"]):::outcome
  out_3(["Check: commit messages<br/>follow Conventional<br/>Commits"]):::outcome
  out_4(["Check: R CMD check passes"]):::outcome
  out_5(["Issue for a likely bug in<br/>the code"]):::outcome
  wf_authors_suggest_yml --> out_1
  wf_authors_suggest_yml --> out_2
  wf_commitlint_yml --> out_3
  wf_R_CMD_check_yaml --> out_4
  wf_roxygen_suggest_yml --> out_1
  wf_roxygen_suggest_yml --> out_2
  wf_test_suggest_yml --> out_1
  wf_test_suggest_yml --> out_2
  wf_test_suggest_yml --> out_5
  classDef outcome stroke-dasharray: 4 3
```

### PR closed

*When a pull request is closed.*

```mermaid
%%{init: {"flowchart": {"rankSpacing": 140, "nodeSpacing": 45}}}%%
flowchart LR
  ev(["PR closed"])
  wf_cleanup_suggestion_branch_yml["<b>Cleanup Suggestion Branch</b><br/>only if:<br/>github.event.pull_request.head.repo.full_name<br/>== github.repository &&<br/>(startsWith(github.event.pull_request.head.ref,<br/>'bot-suggest/') ||<br/>startsWith(github.event.pull_request.head.ref,<br/>'agent-suggest/'))"]
  ev --> wf_cleanup_suggestion_branch_yml
  wf_release_issues_yml["<b>Release Issues</b>"]
  ev --> wf_release_issues_yml
  out_1(["Bot branch deleted"]):::outcome
  wf_cleanup_suggestion_branch_yml --> out_1
  classDef outcome stroke-dasharray: 4 3
```

### PR into main

*When a pull request into main is opened or updated.*

```mermaid
%%{init: {"flowchart": {"rankSpacing": 70, "nodeSpacing": 40}}}%%
flowchart TD
  ev(["PR into main"])
  wf_release_trigger_yml["<b>Release</b>"]
  ev --> wf_release_trigger_yml
  wf_require_head_branch_yml["<b>Require Head Branch</b>"]
  ev --> wf_require_head_branch_yml
  wf_release_publish_yml["<b>Publish Release</b>"]
  out_release_trigger_yml_check_1(["Check: R CMD check passes"]):::outcome
  out_release_trigger_yml_on_success_2(["PR merged"]):::outcome
  out_1(["Issue for the PR author"]):::outcome
  out_2(["Check: PR comes from dev"]):::outcome
  out_3(["Version commit, tag and<br/>draft release"]):::outcome
  wf_release_trigger_yml -- "only if: PR comes<br/>from dev" --> out_release_trigger_yml_check_1
  out_release_trigger_yml_check_1 -- "only if:<br/>needs.check.result<br/>== 'success' &&<br/>needs.link-<br/>issues.result ==<br/>'success'" --> out_release_trigger_yml_on_success_2
  wf_release_trigger_yml -- "only if:<br/>needs.check.result<br/>== 'success' &&<br/>needs.link-<br/>issues.result ==<br/>'success'" --> out_release_trigger_yml_on_success_2
  out_release_trigger_yml_check_1 -- "if check fails" --> out_1
  out_release_trigger_yml_on_success_2 -. "dispatch: release-publish" .-> wf_release_publish_yml
  wf_require_head_branch_yml --> out_2
  wf_release_publish_yml --> out_3
  classDef outcome stroke-dasharray: 4 3
```

### PR edited

*When a pull request is edited.*

```mermaid
%%{init: {"flowchart": {"rankSpacing": 140, "nodeSpacing": 45}}}%%
flowchart LR
  ev(["PR edited"])
  wf_suggestion_choices_yml["<b>Suggestion Choices</b><br/>only if:<br/>(startsWith(github.head_ref,<br/>'bot-suggest/tests/') ||<br/>startsWith(github.head_ref,<br/>'bot-suggest/docs/')) &&<br/>github.event.sender.type<br/>!= 'Bot'"]
  ev --> wf_suggestion_choices_yml
  out_1(["Bot branch matches the<br/>checkboxes"]):::outcome
  wf_suggestion_choices_yml --> out_1
  classDef outcome stroke-dasharray: 4 3
```

### Push to dev

*On a push to dev.*

```mermaid
%%{init: {"flowchart": {"rankSpacing": 140, "nodeSpacing": 45}}}%%
flowchart LR
  ev(["Push to dev"])
  wf_workflow_graphs_yml["<b>Workflow Graphs</b><br/>only if:<br/>.github/workflows/#42;#42;<br/>changed"]
  ev --> wf_workflow_graphs_yml
  out_1(["Commit on the branch"]):::outcome
  wf_workflow_graphs_yml --> out_1
  classDef outcome stroke-dasharray: 4 3
```

### Dispatch: implementer-timer

*When the implementer-timer event is sent.*

```mermaid
%%{init: {"flowchart": {"rankSpacing": 140, "nodeSpacing": 45}}}%%
flowchart LR
  ev(["Dispatch:<br/>implementer-timer"])
  wf_implementer_timer_yml["<b>Implementer Timer</b>"]
  ev --> wf_implementer_timer_yml
  classDef outcome stroke-dasharray: 4 3
```

### On a schedule

*On a schedule.*

```mermaid
%%{init: {"flowchart": {"rankSpacing": 140, "nodeSpacing": 45}}}%%
flowchart LR
  ev(["On a schedule"])
  wf_R_CMD_check_yaml["<b>R-CMD-check</b><br/>weekly (Mon)"]
  ev --> wf_R_CMD_check_yaml
  out_1(["Check: R CMD check passes"]):::outcome
  out_2(["Scheduled workflows kept<br/>enabled"]):::outcome
  wf_R_CMD_check_yaml --> out_1
  wf_R_CMD_check_yaml --> out_2
  classDef outcome stroke-dasharray: 4 3
```

### issues

*issues.*

```mermaid
%%{init: {"flowchart": {"rankSpacing": 140, "nodeSpacing": 45}}}%%
flowchart LR
  ev(["issues"])
  wf_issue_label_check_yml["<b>Issue Label Check</b>"]
  ev --> wf_issue_label_check_yml
  wf_issue_triage_yml["<b>Issue Triage</b>"]
  ev --> wf_issue_triage_yml
  out_1(["Approval label removed,<br/>with a comment"]):::outcome
  out_2(["Triage comment on the<br/>issue"]):::outcome
  wf_issue_label_check_yml --> out_1
  wf_issue_triage_yml --> out_2
  classDef outcome stroke-dasharray: 4 3
```

### issue_comment

*issue_comment.*

```mermaid
%%{init: {"flowchart": {"rankSpacing": 140, "nodeSpacing": 45}}}%%
flowchart LR
  ev(["issue_comment"])
  wf_issue_triage_yml["<b>Issue Triage</b>"]
  ev --> wf_issue_triage_yml
  out_1(["Triage comment on the<br/>issue"]):::outcome
  wf_issue_triage_yml --> out_1
  classDef outcome stroke-dasharray: 4 3
```
<!-- workflow-graphs:overview:end -->
