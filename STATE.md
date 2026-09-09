# Owner 1 (A1-A3: cold start, adopt, build failure) -- paused state

Branch overhaul/int-play-01, rebased onto overhaul/integration @ 86f038026.
Worktree clean; every change is committed with the session trailer.

## Authored
- e2e/playtest_scenario_test.go: newColdPlaytestScenario + ensureDaemon (substrate split, callers unchanged).
- e2e/playtest_01_cold_start_test.go: TestPlaytestColdStartAndFirstTab (A.1, records the roster arm walk),
  TestPlaytestAdoptsAnAnsweringDaemon (A.2), TestPlaytestBuildFailureSurfaces (A.3).
- e2e/playtest_capture_test.go: settleFrame drives a redisplay between reads and both reads follow
  their own eval an interval apart (pgtk flushes pixels only from its main loop).
- daemon/internal/promptqueue/submit.go: roster takes a parked prompt's turn at acceptance (+2 unit tests).
- daemon/internal/sessionwatcher/watcher.go: replayed bring-up `dialing` refused (+1 unit test, 2 amended).
- daemon/integration/roster_test.go: TestRosterBringUpWalkIsMonotoneFromAColdSubmit (fails without both fixes).

## Ran (all green at HEAD dca8b949b)
- playbooks A1-A3: runs 7 and 8 consecutive green, every capture inspected against its manifest.
- daemon unit ./... , daemon integration ./integration/..., host e2e package, gofmt/vet (playtest + integration tags).

## Filed, not fixed
- store warns `store.rpc.open-agent-session refused: unknown agent` twice on every fresh bring-up (store/sidecar owner).
- Doom's +workspaces-load-tab-bar-data-h calls tab-bar--update-tab-bar-lines on persp activation and
  frame-notice-user-settings resets tab-bar-lines 2 -> 1 at boot; the module's two-row bar renders one row (status.el owner).
- Plan A.3 "tab paints failed": no roster arm exists for a build failure (no daemon, no roster); the modeline segment is the surface.

## Next
- Nothing queued for this section. Lead: merge, triage the filed items, and note the settle change touches every owner's captures.
