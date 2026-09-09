# Owner 19 (J57–J60, daemon lifecycle) — parked state

Branch `overhaul/int-play-19`, rebased onto `overhaul/integration` 86f038026 (no
own commits before this file). Worktree confirmed at
`/Users/dodgecoates/.config/doom-overhaul/integration-agents/play-19`.

## Authored
- Nothing new yet. `e2e/playtest_19_daemon_lifecycle_test.go` still holds only
  the seed (J57 schedule half, no cancel). Two attempts to dispatch the
  opus-medium writer hit the 20-subagent ceiling; per the lead's ruling the
  owner authors nothing directly, so the file is untouched.

## Ran
- `bin/suite-slot.sh bin/playtest.sh -run TestPlaytestScheduledDrainBanner`
  against the current image (id 37b7acd5b457): PASS, 12.06s, one capture
  `19-drain/03-drain-scheduled.png` (1524 distinct colors, settled).
- Inspection of that capture: the banner IS drawn — a yellow bar reading
  "daemon restart scheduled · maintenance · in 4m 59s" — but it spans only the
  MAIN column (sidebar's right edge to the page's right edge), not the sidebar.
  Plan J57 says "page-wide". Open question for the lead: is main-column-wide the
  intended layout, or is "page-wide" (over the sidebar too) the contract? Not
  filed as a defect pending that ruling.
- The coordinator's item (banner never drawn in TestWebappLayerRoster) did NOT
  reproduce in the real webview: drain banner drawn 24ms after the push.
- Teardown trail worth noting: `emacs phase daemon-exit took 6.036s (bound 6s)`
  with a five-minute drain schedule standing when the stop was issued. The
  daemon exited AT the bound rather than in one poll interval. Not dismissed:
  to be measured again on the J57 rerun (with the cancel step the schedule will
  no longer be standing at teardown, which will say whether the standing
  schedule is what held the exit).

## Next (when re-woken)
1. Dispatch the opus-medium writer with the prepared brief (J57 cancel step;
   J58 shutdown-now with two tabs, link-down/reconnect-timer/pid-gone/kept-view/
   daemonUnreachable-card assertions, armPaint-derived sentences; J59 graceful
   restart with a composer-held prompt in the daemon's hold tray + forced
   restart of `!interrupt` with the interrupted terminal; J60 the scenario-40
   handover arrangement with an OPEN "play" workspace, tabs before/after,
   adopted session continues, then stop/ensure for degraded/reconnected).
   Design notes: requireSandbox is per-call and cheap — derive scratch paths
   from a first call, then pass WithEmacsEnv options into newPlaytestScenario.
   Selectors: `[data-component="drain-banner"]` + `[data-drain-scheduled]`,
   `[data-component="hold-tray"] [data-held-turn]`, `.turn-ended[data-arm="interrupted"]`,
   `.failure-card[data-arm="daemonUnreachable"]`. Bounds: daemonStopBound,
   handoverAnnounceBound, handoverPromoteBound, daemonLinkBound, playtestPageBound.
2. Run each playbook twice under `bin/suite-slot.sh bin/playtest.sh -run ...`,
   inspect every PNG against its manifest, file/fix mismatches.
3. Gates: gofmt/go vet -tags playtest in e2e; ordinary host e2e package green.

## Processes
- Nothing of mine survives: the run's container exited, no gate slot held by
  this owner (the held slots belong to play-02).
