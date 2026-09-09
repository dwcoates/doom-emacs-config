# Playtest plan — headless, screenshot-reviewed, mocked-SDK only

Owner ruling (2026-09-09): playtests run before any merge to master. They are
a scripted playbook of user actions against the REAL application — real Doom
on Xvfb, real daemon/shim/store/sidecar — with the fake SDK as the sole vendor
so no real agent response is ever generated, a frame capture after every step,
and a manifest line per capture stating what the reviewer should see.

## Parallelism

Every playbook is its own world: its own Emacs on its own Xvfb, its own
quartet, its own scratch root. Playbooks share nothing, so the only ordering
constraint is the sandbox's container gate (memory-budgeted) and the layer's
two Emacs slots per container. Throughput is 2 × containers. Sections D–H are
table-driven over the fake's scenario registry and must be ONE loop each, not
hand-written per scenario.

## Functional first, pictures second

Every step has a PROGRAMMATIC assertion, and it is mandatory: the verb
returned, the buffer exists, the roster reports the tab's arm, the daemon's
feed page holds the row. A step that cannot even start — a void function on
opening agent-repl, a binding that is not there, a daemon that never answers
— fails RIGHT THERE, loudly, with the elisp error text, the *Messages* tail
and the world's daemon/shim/store/sidecar logs attached. That is an ordinary
red test; it needs no picture and no reviewer.

A capture is OPTIONAL and taken only where the step's subject is visual: the
webapp's rendering (feed families, cards, footer, topbar, overlays) and the
tab bar's painted state. It is taken only AFTER the step's functional
assertion passed, so a reviewer never looks at a picture of a broken world.
Most steps in sections A, C, I, J and K are functional-only; sections B
(tab arms), D–H (webapp visuals) and the panel/fullscreen steps of I capture.

## What counts as reviewed

Each capture has a manifest sentence. The mechanical gate is: every capture
exists, is a valid PNG of the declared geometry, and is not blank. The visual
gate is a vision-capable review of the PNGs against the manifest by the lead;
a capture whose picture does not match its sentence is a defect.

## Who runs a playbook

A section group is owned END TO END by one fable agent: it authors the
playbooks (delegating rote table-driven writes if it chooses), runs them in
its own worlds, remediates whatever blocks a step from producing its
evidence — production fixes included, under the standing rules — reruns,
then inspects its own captures against the manifest and files every
mismatch. Run, remediate and inspect are never split across agents; the
agent that hit the blocker fixes it and the one that fixed it judges the
picture. The lead merges the sections, triages what they file, and keeps the
sandbox gate honest. Concurrency is bounded by the sandbox (about two
containers of two Emacsen), not by agent count.

## Playbooks

A. Boot and roster (tab bar)
 1. cold start: no daemon → build+spawn; first tab init→booting→ready; tray empty
 2. adopt an already-answering daemon → ready with no rebuild
 3. build failure → modeline, tab `failed`, no wedge
 4. add project from directory (SPC TAB C-n) → second tab, roster order
 5. new workspace (SPC TAB n) and child-of-current (C-u) → nesting in tab names
 6. fork workspace + conversation (SPC TAB f) → forked tab, parent history in feed
 7. switch (SPC p p / SPC TAB R) → selected tab highlights, webview swaps, composer follows
 8. priority set/clear (SPC TAB p); deprio close shuffles tab to the end
 9. close, re-open (SPC TAB o), close with held prompt (tab stays), kill (never blocks)
10. copy workspace name / copy reference → echo only, negative capture

B. Tab-bar arms, one playbook per transition
11. idle → thinking on submit → done on conclusion (prose)
12. attention on permission ask; clears on answer
13. attention on question (!ask-single); clears on answer
14. failed on !fail-execution; persists until next submit
15. hibernated after idle cutoff; ready again on revival
16. merging → done through SPC TAB M (self-repo); parked on scripted conflict
17. detached indicator while !bash-detach runs; clears on settle
18. link severed → degraded → recovered (kill the shim under the daemon)

C. Composer and delivery
19. type/submit → prompt bubble; SPC o v focuses; discard clears
20. held prompt during live turn → tray; discard from tray
21. deferred prompt drains on finish edge
22. line/region/hunk prompt (SPC TAB e) and canned (E) → reference in prompt
23. attach clipboard image → attachment chip
24. history search recall → last prompt restored

D. Feed families (webapp visuals) — table over scenarios, settled feed per row
25. !md markdown: headings, fence, list
26. !interrupt then SPC o C-c → interrupted terminal
27. !query-eof / !query-fail / !query-eof-mid-ask → failure overlay, ask denied
28. !rotate → separator, new session line
29. !slash, shape-a, shape-a-unnamed
30. !compact / compact-auto / compact-failed → context-cut row, footer budget
31. !model-fallback → topbar model `fake-sonnet-5`
32. !fast-on/off/cooldown → topbar fast cell, three states
33. !rate-limit, five-hour, seven-day → footer allowance line; overage
34. five !usage-* → footer unread caveat beside standing figures
35. !context-tip, !tokens-reminder, !context-budget-warning → footer status
36. !mcp-healthy, !mcp-all → sidebar rows with health

E. Permissions and questions
37. !perm-allow-once / -standing / -standing-mode → card, answer, standing offered vs not
38. !perm-deny-user / -deny-policy / -undecidable / -no-standing → each wording
39. !perm-hold → ask survives a workspace switch and back
40. !ask-single / -multi / -free / -unanswered → card shape, answered state
41. topbar permission-mode picker → change, arm shown

F. Tools — table over scenarios
42. shell: !bash, -hold, -fail, -timeout, -spill, -image
43. detached shell: !bash-detach, -poll, -fail, -live → row, sub-feed, settled
44. files: read(-head/-range/-truncated/-image), write-create/-update, edit, both diagnostics, grep/glob
45. web: fetch, redirect, search
46. skills: !skill, -fail, memory, injected
47. hooks: success, blocked, failed, cancelled
48. automation: plan, findings, worktree keep/remove, cron, monitor deadline/persistent, wakeup schedule/stop (footer chip), artifact publish/list, unmodeled

G. Subagents and tasks
49. !subagent → bubble once, sub-feed opens
50. subagent detached / live / utterance off top level / failed
51. !cancel-all → fan-wide cancel with count
52. tasks create/change/reject; send-message queued / resumed / refused (refused drawn apart)

H. Failure arms — every !fail-* and every !api-*, one capture of the exact headline each

I. Panels and layout
53. open each panel kind into the main area; close; plain close leaves the tab alone
54. fullscreen toggle and restore
55. reload webview (SPC o l), rescue webview (SPC o L) → state intact
56. visit a file → routes to its owning workspace; unroutable file records a refusal

J. Daemon lifecycle
57. scheduled shutdown → page-wide drain banner; cancel removes it
58. shutdown now → every tab goes down; no wedge
59. graceful restart holds prompts; forced restart interrupts the turn
60. handover at freeness → tabs survive, adopted session continues; daemon down surfaces and reconnects

K. Multi-workspace concurrency (one Emacs, several workspaces)
61. two workspaces thinking at once; each arm its own
62. attention in a background workspace while foreground idle → background tab paints attention; switching shows the ask
63. merge one workspace while another runs a turn

~63 playbooks, ~250 captures.

## Partition — twenty owners, one worktree each

| owner | playbooks | subject |
|---|---|---|
| 1 | A1–3 | cold start, adopt, build failure |
| 2 | A4–6 | add project, new + child, fork |
| 3 | A7–10 | switch, priority, close/reopen/kill, copy |
| 4 | B11–13 | thinking→done, attention on ask, attention on question |
| 5 | B14–16 | failed, hibernated, merging/parked |
| 6 | B17–18 | detached indicator, link severed/recovered |
| 7 | C19–21 | composer, held prompt, deferred drain |
| 8 | C22–24 | region prompts, clipboard image, history recall |
| 9 | D25–28 | markdown, interrupt, query death, rotate |
| 10 | D29–32 | slash, compaction, model fallback, fast mode |
| 11 | D33–36 | rate limits, usage outcomes, context tips, mcp |
| 12 | E37–41 | permissions, questions, mode picker |
| 13 | F42–43 | shell, detached shell |
| 14 | F44–45 | files, web |
| 15 | F46–48 | skills, hooks, automation |
| 16 | G49–52 | subagents, tasks, send-message |
| 17 | H | every failure and API arm |
| 18 | I53–56 | panels, fullscreen, reload/rescue, visit-file routing |
| 19 | J57–60 | scheduled/now shutdown, restarts, handover |
| 20 | K61–63 | multi-workspace concurrency |

Each owner: branch `overhaul/int-play-NN`, worktree under integration-agents/play-NN,
playbook files `e2e/playtest_NN_<subject>_test.go`, artifacts under
`playtest/NN-<subject>/`. Owners share the substrate and nothing else.
