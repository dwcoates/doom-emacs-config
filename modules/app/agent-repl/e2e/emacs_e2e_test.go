package e2e

import (
	"encoding/json"
	"os"
	"path/filepath"
	"strings"
	"testing"

	"claude-repld/integration/harness"
)

// EmacsWorld is one world whose CLIENT IS EMACS.
//
// It differs from NewWorld in exactly one structural way, and that
// difference is the whole point of this layer: the DAEMON is not started
// here. Emacs starts it, through the module's own launcher, when a scenario
// calls EnsureDaemon. Everything else — the store, the sidecar, the built
// binaries, the scripted fake git, the build identity, the vendor-call
// prohibition — is the same machinery NewWorld assembles, reused verbatim.
type EmacsWorld struct {
	Emacs   *Emacs
	Store   *Store
	Sidecar *Sidecar

	// Git is the SCRIPTED fake git every process below inherits. No real
	// git runs anywhere in this suite, per SPEC.md's mocking ruling.
	Git *harness.GitWorld
}

// EmacsWorldOption tunes one EmacsWorld. There is exactly one option, and
// it exists for exactly one reason: the DAEMON in this layer is spawned by
// Emacs, so anything the Go layer states through `harness.Opts` can only be
// stated here as environment the EMACS process carries and its daemon child
// inherits.
type EmacsWorldOption func(*emacsWorldConfig)

// emacsWorldConfig is the accumulated options.
type emacsWorldConfig struct {
	extraEnv []string
}

// WithEmacsEnv threads one `KEY=VALUE` into the Emacs process's environment,
// and therefore into the daemon Emacs spawns. It is how a scenario states a
// daemon fact that has no other route in this layer — the daemon's own
// checkout (`AGENT_REPL_SELF_REPO_DIR`) and its merge gate
// (`AGENT_REPL_TEST_ALL_SCRIPT`) being the pair scenario 40 needs.
func WithEmacsEnv(key, value string) EmacsWorldOption {
	return func(c *emacsWorldConfig) { c.extraEnv = append(c.extraEnv, key+"="+value) }
}

// NewEmacsWorld assembles the world and brings Emacs up, but not the daemon.
func NewEmacsWorld(t *testing.T, box sandbox, options ...EmacsWorldOption) *EmacsWorld {
	t.Helper()

	var cfg emacsWorldConfig
	for _, option := range options {
		option(&cfg)
	}

	node := requireNode(t)
	shimMain := requireShimBundle(t)
	sidecarBin := requireSidecarBinary(t)
	daemonBin := harness.DaemonBinary(t)

	// START ORDER: store, then sidecar, then Emacs (which spawns the daemon,
	// which spawns the shim per session). Cleanups register in that order,
	// so t.Cleanup's LIFO unwind tears down Emacs and its daemon first and
	// leaves nothing writing to a store that has already gone away.
	logsDir := filepath.Join(box.Scratch(), "logs")
	if err := os.MkdirAll(logsDir, 0o755); err != nil {
		t.Fatalf("e2e: mkdir %s: %v", logsDir, err)
	}

	store := startStore(t,
		shortSocketPath(t, "store"),
		filepath.Join(box.Scratch(), "store.db"),
		filepath.Join(logsDir, "store.log"))

	// ONE spool root, for the same reason NewWorld gives: the fake SDK's own
	// default is the REAL vendor location /tmp/claude-<uid>, so leaving it
	// unset would write into the developer's live vendor spool.
	spoolRoot := filepath.Join(box.Scratch(), "spool")
	if err := os.MkdirAll(spoolRoot, 0o755); err != nil {
		t.Fatalf("e2e: mkdir %s: %v", spoolRoot, err)
	}

	// The scripted fake git, installed into a directory that LEADS the Emacs
	// process's PATH so the daemon and every one of its children inherit it.
	// harness.StartDaemon normally does this; Emacs is starting the daemon
	// here, so this layer does it at the Emacs process instead.
	git := harness.World(t)
	fakeBin := filepath.Join(box.Scratch(), "fakebin")
	if err := os.MkdirAll(fakeBin, 0o755); err != nil {
		t.Fatalf("e2e: mkdir %s: %v", fakeBin, err)
	}
	body, err := os.ReadFile(harness.FakeGitBinary(t))
	if err != nil {
		t.Fatalf("e2e: read the fake git: %v", err)
	}
	if err := os.WriteFile(filepath.Join(fakeBin, "git"), body, 0o755); err != nil {
		t.Fatalf("e2e: install the fake git: %v", err)
	}

	extraEnv := append([]string{
		"PATH=" + fakeBin + ":" + os.Getenv("PATH"),
		"FAKEGIT_STATE=" + git.StateFile,
		"AGENT_REPL_FAKE_SPOOL_ROOT=" + spoolRoot,
		"AGENT_REPL_SHIM_NODE=" + node,
		"AGENT_REPL_SHIM_MAIN=" + shimMain,
	}, buildIdentityEnv()...)
	// LAST, so a scenario's own statement is the one the process carries.
	extraEnv = append(extraEnv, cfg.extraEnv...)

	e := StartEmacs(t, box, EmacsOpts{
		DaemonBinary: daemonBin,
		StoreSocket:  store.Socket,
		ExtraEnv:     extraEnv,
	})

	// The sidecar watches the SAME two account roots the launcher was given.
	// SPEC.md §B's "One string per config root" invariant applies here
	// unchanged: the sidecar records cursors under the path it WALKED, which
	// is symlink-resolved, so the roots are resolved before they are handed
	// over or a cursor is never seen to advance.
	sidecar := startSidecar(t, sidecarBin, sidecarOpts{
		StoreSocket: store.Socket,
		ConfigRoots: []string{resolveOrFail(t, e.DefaultConfigDir), resolveOrFail(t, e.MultiRepoConfigDir)},
		SpoolRoot:   spoolRoot,
		LogPath:     filepath.Join(logsDir, "sidecar.log"),
	})

	// Registered LAST, so LIFO runs it FIRST: it observes what died on its
	// OWN, before this world's own teardown killed anything on purpose.
	t.Cleanup(func() {
		if store.Exited() && !store.stopped {
			t.Errorf("e2e: the store exited before test cleanup, and was never stopped by the test:\n%s", tailStoreLog(t, store))
		}
		if sidecar.Exited() && !sidecar.stopped {
			t.Errorf("e2e: the sidecar exited before test cleanup, and was never stopped by the test")
		}
	})

	e.ArtifactPaths = append(e.ArtifactPaths, git.StateFile, logsDir)
	return &EmacsWorld{Emacs: e, Store: store, Sidecar: sidecar, Git: git}
}

// resolveOrFail resolves a directory's symlinks, per the config-root
// invariant above.
func resolveOrFail(t *testing.T, dir string) string {
	t.Helper()
	resolved, err := resolvedPath(dir)
	if err != nil {
		t.Fatalf("e2e: resolve the config root %s: %v", dir, err)
	}
	return resolved
}

// TestEmacsProofOfLife is scenario 20 of EMACS-LAYER-SPEC.md, implemented
// first and alone: workspace created, agent-repl panel opened, prompt
// submitted from the composer, response row visible in EMACS'S OWN STATE.
//
// It is deliberately end-to-end through the whole client stack rather than
// broad: it proves the layer's mechanism works — Emacs boots, loads the real
// module, spawns the daemon through its own launcher, drives ordinary
// commands, and hands state back as data — so the remaining 43 scenarios are
// a matter of writing scenarios rather than of building machinery.
func TestEmacsProofOfLife(t *testing.T) {
	box := requireSandbox(t)
	w := NewEmacsWorld(t, box)
	e := w.Emacs

	// 1. EMACS spawns the daemon, through the module's own cold-start
	//    launcher. This is the step that catches an argv the launcher gets
	//    wrong, because nothing else in the suite composes that argv.
	e.EnsureDaemon()

	// A scripted fake-git worktree for the workspace to be registered
	// against. No real git runs.
	repository := harness.NewRepoAt(t, filepath.Join(box.Scratch(), "repo"))

	// 2. Register the directory through the ORDINARY command the user runs
	//    (SPC TAB C-n). The DAEMON mints the identity; Emacs echoes it.
	e.Eval(`(agent-repl-add-project-workspace ` + elispString(repository.Dir) + `)`)

	name := e.AwaitEval("the workspace to appear in Emacs's registry",
		`(let (names) (maphash (lambda (k _v) (push k names)) agent-repl--workspaces) names)`,
		func(raw json.RawMessage) bool { return len(decodeStrings(raw)) == 1 })
	wsName := decodeStrings(name)[0]

	// It must also carry a daemon-minted ref: Emacs never constructs one.
	e.AwaitTrue("the workspace to hold a daemon-minted ref",
		`(and (gethash `+elispString(wsName)+` agent-repl-host--by-name) t)`)

	// 3. Open the agent-repl panel through the ordinary command.
	e.Eval(`(agent-repl-frontend-open-panel)`)
	e.AwaitEval("the agent-repl panel windows to appear",
		`(mapcar (lambda (w) (buffer-name (window-buffer w))) (window-list))`,
		func(raw json.RawMessage) bool {
			for _, b := range decodeStrings(raw) {
				if strings.HasPrefix(b, "*agent-frontend-") || strings.HasPrefix(b, "*agent-panel") {
					return true
				}
			}
			return false
		})

	// 4. Submit a prompt FROM THE COMPOSER: put text in the input buffer the
	//    way a user types it, then PRESS RET. The fake SDK answers; a plain
	//    prompt with no "!" prefix falls through to the default prose
	//    scenario.
	//
	//    RET is pressed rather than `agent-repl-send` called, and that is a
	//    deliberate consequence of booting real Doom: the composer's RET
	//    comes from `map! :map agent-repl-input-mode-map :ni "RET"` in
	//    `input.el`, which under the old `-Q` boot expanded to nothing at
	//    all. Pressing it asserts the binding AND the command; calling the
	//    command would have asserted only half of what a user does.
	inputBuffer := e.EvalString(`(buffer-name (agent-repl--input-buffer ` + elispString(wsName) + `))`)
	e.Eval(`(with-current-buffer (agent-repl--input-buffer ` + elispString(wsName) + `)
                 (erase-buffer)
                 (insert "hello from the emacs client layer")
                 t)`)
	if want, got := "agent-repl-send", e.BindingForIn(inputBuffer, "RET"); got != want {
		t.Fatalf("composer RET resolves to %q, want %q: the Doom `map!' for agent-repl-input-mode-map did not take", got, want)
	}
	e.KeysIn(inputBuffer, "RET")

	// 5. The response is visible in EMACS's own state: the roster row for
	//    this workspace settles on a settled arm, which is the finish edge
	//    Emacs's four reactions all ride.
	e.AwaitEval("the roster row to settle after the turn",
		`(let (arms)
                   (maphash (lambda (_id row)
                              (push (format "%s" (plist-get row :status)) arms))
                            agent-repl-roster--rows-by-id)
                   arms)`,
		func(raw json.RawMessage) bool {
			for _, arm := range decodeStrings(raw) {
				for _, settled := range []string{":ready", ":done", ":idle-async"} {
					if arm == settled {
						return true
					}
				}
			}
			return false
		})

	// 6. Cross-check the DAEMON's own frame at the address Emacs's launcher
	//    published, so a green test cannot mean "Emacs drew something the
	//    daemon never sent".
	client := harness.DialAt(t, e.DaemonAddr())
	if client == nil {
		t.Fatal("e2e: could not dial the daemon Emacs launched")
	}
}

// elispString renders a Go string as an elisp string literal.
func elispString(s string) string {
	quoted := make([]rune, 0, len(s)+2)
	quoted = append(quoted, '"')
	for _, r := range s {
		if r == '"' || r == '\\' {
			quoted = append(quoted, '\\')
		}
		quoted = append(quoted, r)
	}
	return string(append(quoted, '"'))
}

func decodeStrings(raw json.RawMessage) []string {
	if isJSONNull(raw) {
		return nil
	}
	var xs []string
	if err := json.Unmarshal(raw, &xs); err != nil {
		return nil
	}
	return xs
}
