# Playtest stand-down — 2026-09-09

RETIRED 2026-09-10 by the owner's ruling: the layer this manifest resumes no
longer exists, and `docs/REALTEST-PLAN.md` replaces it. Nothing here is to be
resumed. It is kept as the record of where the stand-down left things.

All playtest owners are STOPPED. None was alive at stand-down (TaskStop'd during
the pause; owners 8–11 died on the vendor's 429). Every owner's state lives in a
`STATE.md` at its worktree root; four were reconstructed by the lead from git
(owners 3, 4, 6, 10) because those owners left none. Uncommitted work in
owners 6 and 10 was committed as `wip(...)` so nothing is lost. Owner 4's
untracked `e2e/playtest_zz_diag_test.go` is scratch, deliberately left untracked.

## Ruling on resumption (owner, 2026-09-09)

Every fable orchestrator is REPLACED by an **opus-medium** agent when work
resumes. Fable is not re-woken. Each replacement reads its `STATE.md`, rebases
onto `overhaul/integration`, and continues the loop the brief prescribes
(run whole section → fix every in-section defect at the source with a unit test
and an integration test → rerun → two consecutive green runs → report once).
The replacement may edit directly; the fable-delegates-to-opus rule is moot.

Integration tip at stand-down: `2207d7ae5`.

## Owners

| owner | playbooks | branch / worktree (integration-agents/) | tip | ahead | STATE.md | status |
|---|---|---|---|---|---|---|
| 1 | A1–3 | `overhaul/int-play-01` / `play-01` | `8965ec238` | 10 | `play-01/STATE.md` | DONE twice-green; merge first (substrate: settle redisplay) |
| 2 | A4–6 | merged (cb085cba9) | — | 0 | — | DONE; residue is fork-history |
| 3 | A7–10 | `overhaul/int-play-03` / `play-03` | `98175f4bd` | 10 | `play-03/STATE.md` | mid-loop; last run outcome unknown |
| 4 | B11–13 | `overhaul/int-play-04` / `play-04` | `85dc3a735` | 4 | `play-04/STATE.md` | mid-loop; blocked on capture staleness |
| 5 | B14–16 | `overhaul/int-play-05` / `play-05` | `6db208d2d` | 10 | `play-05/STATE.md` | mid-loop |
| 6 | B17–18 | `overhaul/int-play-06` / `play-06` | `990536576` | 8 | `play-06/STATE.md` | mid-loop; WIP settle-window commit |
| 7 | C19–21 | `overhaul/int-play-07` / `play-07` | `57eac02e9` | 6 | `play-07/STATE.md` | DONE; merge (substrate: rAF paint gate) |
| 8 | C22–24 | `overhaul/int-play-08` / `play-08` | `940010969` | 7 | `play-08/STATE.md` | mid-loop (429) |
| 9 | D25–28 | `overhaul/int-play-09` / `play-09` | `928fe8874` | 8 | `play-09/STATE.md` | mid-loop (429) |
| 10 | D29–32 | `overhaul/int-play-10` / `play-10` | `df43e7168` | 9 | `play-10/STATE.md` | mid-loop (429); WIP shim commit |
| 11 | D33–36 | `overhaul/int-play-11` / `play-11` | `ebfbfe631` | 2 | `play-11/STATE.md` | parked (429) |
| 12 | E37–41 | `overhaul/int-play-12` / `play-12` | `26d4025d5` | 2 | `play-12/STATE.md` | parked |
| 13 | F42–43 | `overhaul/int-play-13` / `play-13` | `f3638b134` | 2 | `play-13/STATE.md` | parked; needs Landing 16 image arm + bash-spill item |
| 14 | F44–45 | `overhaul/int-play-14` / `play-14` | `629230992` | 2 | `play-14/STATE.md` | parked |
| 15 | F46–48 | `overhaul/int-play-15` / `play-15` | `43d0467ec` | 2 | `play-15/STATE.md` | parked |
| 16 | G49–52 | `overhaul/int-play-16` / `play-16` | `ecdd74cee` | 1 | `play-16/STATE.md` | parked; subagent caret toggle open |
| 17 | H | `overhaul/int-play-17` / `play-17` | `c3b64a788` | 2 | `play-17/STATE.md` | parked |
| 18 | I53–56 | `overhaul/int-play-18` / `play-18` | `8db778d7a` | 2 | `play-18/STATE.md` | parked |
| 19 | J57–60 | `overhaul/int-play-19` / `play-19` | `b6e215a79` | 1 | `play-19/STATE.md` | parked; needs drain-banner main-column ruling |
| 20 | K61–63 | `overhaul/int-play-20` / `play-20` | `a0b1dd7fd` | 4 | `play-20/STATE.md` | parked |
| — | Landing 17 / ruling 2 | `overhaul/int-fork-history` / `fork-history` | `61bcf66d2` | 3 | `fork-history/STATE.md` | ruling 2 DONE, mergeable; Landing 17 (fork mints new AgentId, PortTranscript re-mints uuids) unbuilt |

## Lead's order of work on resume

1. Reconcile the three capture-settle changes into one substrate commit on integration:
   owner 7 rAF paint gate, owner 1 redisplay-between-reads, owner 6 50ms unchanged window.
2. Merge owners 1 and 7 and fork-history's ruling-2 half; delete those worktrees.
3. Build Landing 17 (proto + daemon) with an opus-medium agent.
4. Re-wake the rest as opus-medium replacements in waves of ten, each with a per-owner
   scratch log dir, foreground runs, and the four bracketed rules from the brief.
5. Unassigned filed defects stay listed in RESUME-2026-09-03.md's pause checkpoint.
