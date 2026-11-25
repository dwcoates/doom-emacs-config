# Topbar summary: long and short, links on the name, no warnings

The owner asked (2026-11-25) that the topbar's summary always be drawn in its purple, never underlined, and never change color for any reason; that hovering it show a popup with a longer description of the workspace; that the workspace's links move from the summary to the workspace name beneath it, which stays white and is underlined only while the workspace shows at least one link; and that `agent-repld workspace summary` take the two parts as `-long <text> -short <text>`, long first, so an agent distills the short summary from the long one. The long summary is two to four sentences at most. Asked where the topbar's warnings should go once neither heading line may change color, the owner ruled that they go nowhere.

Design mode: autonomous.

## Core design principles

- **The heading's two lines never change appearance with state.** The summary is always the summary's purple; the name is always the foreground white. Consequence: no state (warnings, errors) may recolor either line, so the warning strip that used them as its control has no surface. It does not claim that the name never changes at all: it is underlined while links show, which is the one affordance the owner asked for.

## Assumptions

- The text the daemon appends to a workspace's prompt while it owes a summary is the `workspace-summary-owed` brief (`prompts/workspace-summary-owed.md`, read by `daemon/internal/wssummary`). It is the "initial prompt" text the owner referred to. Evidence tier: code.
- A workspace whose agent set a short summary before this change has no long summary. It keeps its short summary on the topbar and owes a summary again, so its next prompt carries the owed brief and the agent sets both parts. Evidence tier: decision.

## Iteration sequence

1. `agentrepl.v1.SetWorkspaceSummary` request, success and refusal.
2. The read sides: `agentrepl.v1.WorkspaceStatus` and `agentrepl.v1.FoundWorkspace`.
3. `frontend.v1.TopbarTitle` and `frontend.v1.TopbarWorkspaceName`.
4. Retiring `frontend.v1.TopbarView.warnings`.

## Landed changes

### 1. SetWorkspaceSummary takes a long and a short summary

- WHAT: `SetWorkspaceSummaryRequest.summary` became `short_summary` (tag 2), and `long_summary` (tag 3) was added, declared first. The success echoes both. `SetWorkspaceSummaryInvalidSummary` became a oneof naming the refused part (`long_summary` or `short_summary`), each arm carrying the refused text and the reason; its old tags 1 and 2 are reserved.
- WHY: the owner wants a hover description, and wants the long summary written first so the short one is distilled from it.
- Rules: the long summary is one paragraph (no line break), two to four sentences, at most eighty words, ending on a sentence end, with no whitespace at either end. The eighty-word cap is the daemon's own bound so the popup stays a popup; the owner set only the sentence bounds. The short summary's rules are unchanged. The long summary is judged first.
- Consequences: the CLI verb's `-text` flag is replaced by `-long` and `-short` (both required). The owed brief, the `agent-repl-workspaces` skill's summary runbook, and the fake daemon used by ERT change with it.

### 2. Status and find answer both parts

- WHAT: `WorkspaceStatus.summary` (16) became `short_summary`, and `long_summary` (18) was added, since master took 17 for `close_request`. `FoundWorkspace.summary` (5) became `short_summary`, and `long_summary` (16) was added. Find's text filter matches either part.
- WHY: an agent judging which workspace owns some work reads the long summary to judge it better than the title alone allows.
- Consequences: protojson output of `agentrepl.v1.WorkspaceStatus` and `agentrepl.v1.FoundWorkspace` now spells `shortSummary`, so any harness text reading `.summary` from them changes.

### 3. The summary describes, the name links

- WHAT: `TopbarTitle.links` (3) was retired with `link` (2), and `TopbarTitle.description` (4, `TopbarTitleDescription`) was added: the popup a hover shows. `TopbarWorkspaceName.links` (3) was added. The link family was renamed from `TopbarTitleLink*` to `TopbarWorkspaceNameLink*`, and its icon enum's values to `TOPBAR_WORKSPACE_NAME_LINK_ICON_*`, because the links are now the name's element.
- WHY: the owner's layout. Under figma-to-idl the links are the name element's props now, so they ride the name's message.
- Consequences: a workspace with no summary still shows its links, because the name is always drawn; before, links rode the title and a workspace with no summary had nothing clickable.

### 4. The topbar draws no warnings

- WHAT: `TopbarView.warnings` (5) is retired, and `TopbarWarningStrip` with every `TopbarWarning*` message and every warning detail message is deleted.
- WHY: owner ruling, 2026-11-25: the warnings are shown nowhere.
- Consequences, accepted: the daemon's warning producer (`daemon/internal/resolve/topbar/warnings.go`) and its raise API (`Resolver.RaiseWarning`, `RaiseDaemonWarning`, `RetractDaemonWarning`) go with the surface. Their callers (`internal/workspace/remotecontrol.go`, `parkedbridge.go`, `effort.go`, `internal/resolve/feed/resolver.go`, `cmd/agent-repld/graph.go`'s fault surfaces) keep recording each condition through their own canonical log calls. The webapp's client-local failures (`webapp/src/failure/local.ts`), which the topbar listed, are recorded in the page's log only.
