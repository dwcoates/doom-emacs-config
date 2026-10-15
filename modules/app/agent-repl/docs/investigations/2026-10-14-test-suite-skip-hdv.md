# Option E: skip a system's unit and integration suites when their inputs are unchanged

Workspace `test-suite-skip-hdv`, a child of `agent-repl-test-slimming` (the
owner's program to make the agent-repl suites cheaper and faster without
losing coverage). Branch `test-suite-skip-hdv`.

## What was built

Every run (`bin/test-all.sh`, the merge gate, a development run) now skips a
KEYED suite whose exact inputs an earlier run passed whole, and says so:

```text
[agent-repl-tests] daemon: reused a green run (inputs f08c8dfa2ea5, passed 2026-10-15 00:19 on test-suite-skip-hdv 77ace18934 with uncommitted changes)
```

- **The key is content.** `testrun/internal/reuse` hashes the following:
  - every file the suite reads (a Go suite's `go list -deps -test` graph and
    the directory listings a `./...` pattern walks; a TypeScript or Emacs
    suite's whole tree; the programs its tests run; the runner's own code;
    the files its `roster.Suite.Reuse` declares);
  - its roster entry, its toolchain (go, node, npm, emacs) and its
    environment.
  - So the skip holds for any queue order, rebase or racing head.
  - It never walks history.
- **The input set is enforced.** Every unit of a keyed suite starts under
  `testrun confine`. That command scrubs the environment to what the key
  covers, then execs the unit under a seatbelt profile. The profile refuses
  every file read under every checkout of the repository except the suite's
  input set, in the unit's whole process tree. An input left off the set is
  a loud test failure ("operation not permitted"), never a stale pass. That
  is the structural guarantee the parent asked for in place of an audit.
- **A record is written only for a whole green suite on unmoved inputs.** Three
  checks gate it:
  - the suite is re-keyed after the run;
  - every `node_modules` it read must match its `package-lock.json`;
  - a targeted run never records.
  - A record from a dirty checkout says so.
  - `--no-reuse` (implied by `--record` and `--coverage`) runs everything.
- **Every file is placed.** Each file must be in a keyed suite's set, in
  `roster.AlwaysRunRegions` or in `roster.ReadByNoSuite`. An unplaced file
  fails the run (`placement: ...`) without disabling reuse, because no keyed
  suite can read it.
- **`testrun plan [--json]`** prints the decision without running anything,
  for the governor's `/run-tests`.
- **The merge gate draws a reused suite.** `frontend.v1.FeedMergeTestSuite`
  gained the `reused` arm (`docs/protobuf-design/2026-10-14-merge-test-suite-reused.md`).
  - The Tests tab draws ✓ and "reused: passed <when> on <branch> <sha>".
  - The footer draws no edge (owner ruling).

Keyed: `daemon`, `sidecar`, `store`, `lock`, `logging`, `protohelpers`,
`webapp`, `shim`, `ert`. Each passed whole under its profile and recorded.

Always run:
- the end-to-end suites;
- the shell harnesses;
- `testrun` (its tests start real sandboxes, and a sandbox cannot nest);
- the new `repo-scans`.

## Measured

- **The share of a full run that reuse can skip.** In the last canonical run
  (`test_time.csv`), the keyed suites held about 406 of 965 unit-seconds:
  - sidecar 120;
  - daemon 95;
  - ert 64;
  - store 50;
  - shim 48;
  - webapp 26.
  - A gate, or a rerun after a fixing attempt, whose change leaves those
    suites' inputs untouched skips all of that.
  - The end-to-end suites (e2e 343, e2e-emacs 75) always run.
- **Profile cost.**
  - Per-file literals for the daemon's 1,683 files took 0.92s for the kernel
    to load on every unit start.
  - One regex per flat directory loads in about 50ms.
  - File operations under the profile ran about 10% slower on a file-heavy
    microbenchmark (0.71-0.80s against 0.79-0.87s).
- **Decision cost.** `testrun plan` over the whole roster (keys for every
  keyed suite plus the placement check) takes about 1.4s.

## Findings

1. **The kernel ranks sandbox rules by operation, not by order.**
   - A `file-read-data` deny beats a later `file-read*` allow.
   - So every allowance inside a flat directory names `file-read-data` too:
     a listed directory, a subtree, or a nested flat directory.
   - Kernel tests in `testrun/internal/reuse/profile_test.go` pin it.
2. **A sandboxed process cannot exec a setuid binary.**
   - The daemon harness's reaper runs `/bin/ps`.
   - `ps` now runs `with no-sandbox` from a reviewed allowlist. It reads no
     checkout file on a caller's behalf.
3. **Module resolvers list a file's directory** (esbuild, node). A declared file
   therefore also places its directory's listing, hashed.
4. **Tests that read far beyond their own system made keys wide.** Seven moved,
   every assertion kept, and each was mutation-checked by planting a violation:
   - Into the new always-run `repo-scans` suite (`daemon/reposcans`, a
     nested module so it can still call `merge.SelectSuites`):
     - suiteselect's go.mod and Vitest-import derivations;
     - the mock-rule scan of every system's integration sources;
     - the rosterwalk, storewrite and oneofarm whole-module scans;
     - the harness's serving-predicate scan of `e2e/`.
   - Into ERT: the webapp's scan of the Emacs sources for the retired
     Release label.
   - The daemon's key no longer covers `webapp/`, the shim, `e2e/` or the
     other integration trees, and `protohelpers` became keyable.
   - It also closed a gate gap: those scans used to run only when
     `daemon/` or `proto/` changed. A change to any system (or to `bin/`)
     now selects `repo-scans`.
5. **The sidecar's suite reads the shim's whole tree.**
   - Its mocked vendor is the shim's fake mode, bundled from source after
     `bin/ensure-deps.sh`.
   - So a shim change rightly invalidates the sidecar's key.
6. **Undeclared reads the confined runs found and declared:**
   - the merge-queue hook (`daemon`);
   - `metaprompt.md`, read when `input.el` loads (`ert`);
   - the shared `testdata/` fixtures and the fake git (`shim`);
   - `proto/ts` and `bin/require-background.mjs` (`webapp`);
   - `lib-node-store.sh` (`sidecar`).

7. **The daemon harness's stray sweep could not read a rewritten argv.**
   - `npm exec` and vitest write their process titles over their own
     argument memory, while the kernel keeps the exec's argc. A real
     `npm exec vitest run <230 files>` buffer decodes 207 of 234 strings
     and then runs out.
   - The sweep refused such buffers and, after 2s, failed every daemon
     integration and e2e test that ran beside a long vitest chunk. The
     confined units' scrubbed environment (fewer strings to run into) made
     this certain in a full run.
   - Fixed in `daemon/integration/harness/proctable.go`: a buffer whose
     strings are all complete is read as far as it goes, as `ps` reads it.
     A buffer ending mid-string, or holding no argument, is still an exec
     writing it, and is read again.
8. **A suite must not declare another suite's build output.** The sidecar
   declared the shim's whole tree, `dist/` included. The shim suite rewrites
   `dist/` in its integration build, so in a full run the sidecar always
   refused to record. It now declares the shim's sources entry by entry.
9. **One webapp test missed its 850ms bound under concurrent load.**
   - The test is `feed.test.ts` "keeps the rows under a scrolled-away reader
     still when any row above them grows".
   - It took 1,014ms, against about 28ms alone; its other runs passed.
   - It is a starvation stall, not a reuse defect, and its cause is not
     yet found.

## Verification

- **The final full run of every suite except `e2e-emacs` was green.**
  - Seven keyed suites were reused from earlier green runs of their exact
    inputs: ert, daemon, store, lock, logging, protohelpers and shim.
  - Every other suite ran and passed.
- **`e2e-emacs` is blocked on the host's Docker file-descriptor leak.** That
  fix is `test-tier-audit-e2e-edj-04`'s and goes to the front of the merge
  queue, so this branch's gate runs `e2e-emacs` only after it lands.

## Residual surfaces, stated plainly

These are inputs a test can observe that the key does not hash. Each is
improbable to change behavior; none is structurally excluded.

- **Host tools on PATH**, other than go, node, npm and emacs (bash, sed, ps).
  The PATH value is hashed; the tools' versions are not.
- **Reads outside every checkout:**
  - `$HOME`, for example a test reading `~/.claude` when `CLAUDE_CONFIG_DIR`
    is scrubbed;
  - the Go module cache, which is covered by `go.sum`.
  - The profile allows everything outside the checkouts, so the run's temp
    directories and the host's slot locks work.
- **A suite's own generated trees**, trusted because the run produces them:
  - `node_modules` is lock-checked after every recorded run;
  - the shim's `dist/` is rebuilt by its integration build before the
    integration files read it.

## Follow-ups

- `cases` in `testrun plan --json` is shaped like the merge gate's
  failing-test brief, ready for per-test selection; nothing fills it yet.
- The shell harness suites could be keyed the same way (declared trees plus
  confinement); they are small, so it was not worth it here.
- `skills/write-tests/SKILL.md` should carry one line for test authors: a
  keyed suite's tests read only declared inputs, and a test that needs
  another system's tree belongs in `repo-scans`. The runner now prints that
  hint when a keyed suite fails.

## For the owner

- **A proposed memory, not saved.** It would add
  `feedback_pgrep_self_match.md`, saying that a `pgrep -f` wait loop must use
  a pattern that cannot match its own command line. This session's first such
  loop matched itself and would never have ended.
