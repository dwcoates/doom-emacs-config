# Owner 13 (F42-F43, shell and detached shell) — stand-down state

Branch `overhaul/int-play-13`, cut from overhaul/integration at b5d4c0b18. Not yet rebased onto 86f038026 (the coordinator asked for that before final gates).

## Authored (committed, d803234b4)
- `modules/app/agent-repl/e2e/playtest_13_shell_test.go`: `TestPlaytestShellFamily` (F42, six rows, one loop) and `TestPlaytestDetachedShellFamily` (F43, four rows, one loop). Written by the owner before the "delegate every edit" ruling arrived; it stands per that ruling.
- gofmt clean, `go vet -tags playtest ./...` clean in e2e.

## Ran (one sandbox run, image 37b7acd5b457, run log in the session scratchpad `play13-run1.log`)
- `TestPlaytestDetachedShellFamily`: PASS. 7 captures under `e2e/.playtest-out/playtest/13-detached-shell/` (detach row+settled exit 0, poll row+settled exit 0, fail row+settled exit 3, live row). Not yet inspected with the Read tool.
- `TestPlaytestShellFamily`: FAIL at the `!bash-timeout` row. 4 captures landed first (bash settled, bash-hold live, bash-hold settled, bash-fail settled). The failing wait: the `Bash` card for `sleep 600` exists but has no `[data-output-body]` within 2s — the row's settled predicate assumed the daemon draws the "timed out after ..." text on the foreground card; the actual card state was not read. Next step: read the card's `data-state`/`data-verdict`/`data-output-form` (and whether the detached row for `sleep 600` is drawn) to decide whether this is a playbook assumption to correct or a production defect (daemon toolcall.go's `bashOutcomeText` timed-out lead never reaching the card).
- Observed in captured manifest: after the `!bash-hold` interrupt the card stays `data-state="running"` while the turn-ended row reads interrupted — a possible daemon defect (an interrupted foreground unit never settles its card); to be judged at inspection.
- Ordinary host e2e package (`go test ./e2e`, untagged): RED on this tip independent of my file — `TestApiRequestFailedArms/{VendorUnmodeled,PermissionDenied,InvalidRequest}` (failurearms_e2e_test.go:319, headline carries a parenthetical the test does not expect). Not mine; to file with the lead (may already be fixed on 86f038026).

## Filed / to file (need a proto change, so not fixable here)
- `!bash-image`: FeedToolCallReturned.form has no image arm; card settles on `none`, no image drawn.
- `!bash-fail` / `!bash`: the foreground card draws no exit code; FeedToolCallReturned carries none. Only the detached FeedShell has an exit chip.
- Detached shell has no sub-feed by schema (cards/shell.ts); the plan's "sub-feed opened" step is asserted as a negative.

## Lost context
- The coordinator referenced a "bash-spill two-plane item" sent earlier; that message did not survive the session termination. Please resend on re-wake.

## Next, on re-wake
1. Rebase onto overhaul/integration (86f038026).
2. Dispatch an opus-medium agent to fix the `!bash-timeout` row (after reading the card's actual state) and to address the bash-spill item.
3. Rerun both playbooks twice consecutively, inspect every capture with Read, file mismatches.
4. Gates: gofmt, vet -tags playtest, host e2e package, webapp suites if touched.
