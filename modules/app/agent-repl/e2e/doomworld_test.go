package e2e

// doomworld_test.go — THE DOOM TIER: a sandboxed, Doom-booted GUI Emacs
// against the FAKE daemon (lisp/testsupport/fakedaemon), for what only a
// booted Doom shows -- `map!` leader bindings, popup placement, persp-mode's
// workspace recycling -- with no real daemon, shim, store or sidecar
// (docs/investigations/2026-10-14-test-tier-audit-e2e-edj.md, "The Doom
// tier"). It is an Emacs INTEGRATION tier: one real system, Emacs, plus the
// fake of its one neighbor, run as the `emacs-doom` suite (TestDoom*).
//
// The fake answers every unary call with a default success and serves the
// three standing streams; a test scripts what it needs and pushes the roster
// the daemon would author. Every request Emacs makes is recorded, decoded into
// the GENERATED types, so an assertion reads the wire the fake saw.

import (
	"bytes"
	"context"
	"encoding/json"
	"fmt"
	"io"
	"net/http"
	"os"
	"os/exec"
	"path/filepath"
	"sync"
	"testing"
	"time"

	"google.golang.org/protobuf/encoding/protojson"
	"google.golang.org/protobuf/proto"

	"agent-repld/integration/harness"
)

var fakeDaemonProgram = &programOnce{name: "fakedaemon", build: func() (string, error) {
	return goBuildOnce(repo.fakeDaemonDir, filepath.Join(e2eBinDir(), "fakedaemon"))
}}

// requireFakeDaemon builds lisp/testsupport/fakedaemon, once per run.
func requireFakeDaemon(t *testing.T) string {
	t.Helper()
	return fakeDaemonProgram.require(t, "the Doom tier runs against the FAKE daemon, which does not build")
}

// fakeDaemonPublishBound is how long the fake may take to publish daemon.addr
// after it starts: a Go binary binding one loopback port.
const fakeDaemonPublishBound = 5 * time.Second

// fakeDaemon is one running fake daemon and its control plane.
type fakeDaemon struct {
	t      *testing.T
	cmd    *exec.Cmd
	addr   string
	exited chan error
	stop   sync.Once
}

// startDoomFakeDaemon starts the fake on STATE_DIR, where it publishes the
// daemon.addr Emacs adopts, and stops it at cleanup through its own orderly
// exit (so daemon.addr is removed on the path production takes).
func startDoomFakeDaemon(t *testing.T, stateDir string) *fakeDaemon {
	t.Helper()
	if err := os.MkdirAll(stateDir, 0o700); err != nil {
		t.Fatalf("doom: prepare the state root %s: %v", stateDir, err)
	}
	cmd := exec.Command(requireFakeDaemon(t))
	cmd.Env = append(os.Environ(), "AGENT_REPL_STATE_DIR="+stateDir, "AGENT_REPL_FORBID_VENDOR_CALLS=1")
	var stderr bytes.Buffer
	cmd.Stderr = &stderr
	if err := cmd.Start(); err != nil {
		t.Fatalf("doom: start the fake daemon: %v", err)
	}
	exited := make(chan error, 1)
	go func() { exited <- cmd.Wait() }()
	f := &fakeDaemon{t: t, cmd: cmd, exited: exited}
	// A scenario whose Emacs never linked stops the fake here; one that did
	// stopped it already, in its Emacs's teardown (EmacsOpts.StopDaemon).
	t.Cleanup(f.Stop)
	path := filepath.Join(stateDir, "daemon.addr")
	bound := time.NewTimer(fakeDaemonPublishBound)
	defer bound.Stop()
	ticker := time.NewTicker(pollInterval)
	defer ticker.Stop()
	for {
		if body, err := os.ReadFile(path); err == nil && len(bytes.TrimSpace(body)) > 0 {
			f.addr = harness.AddrLine(string(body))
			return f
		}
		select {
		case err := <-f.exited:
			t.Fatalf("doom: the fake daemon exited before publishing %s: %v\n%s", path, err, stderr.String())
		case <-bound.C:
			t.Fatalf("doom: the fake daemon published no %s within %s\n%s", path, fakeDaemonPublishBound, stderr.String())
		case <-ticker.C:
		}
	}
}

// Stop asks the fake to exit through its own control plane, so it removes
// daemon.addr as production does, and waits for it; once, however often it is
// asked.
func (f *fakeDaemon) Stop() {
	f.stop.Do(func() {
		if _, err := f.post("/_fake/exit", struct{}{}); err != nil {
			f.t.Errorf("doom: ask the fake daemon to exit: %v", err)
		}
		select {
		case <-f.exited:
		case <-time.After(fakeDaemonPublishBound):
			_ = f.cmd.Process.Kill()
			<-f.exited
			f.t.Errorf("doom: the fake daemon did not exit within %s of being asked; killed it", fakeDaemonPublishBound)
		}
	})
}

// post sends BODY, JSON-encoded, to the control plane at PATH.
func (f *fakeDaemon) post(path string, body any) (json.RawMessage, error) {
	raw, err := json.Marshal(body)
	if err != nil {
		return nil, err
	}
	return f.do(http.MethodPost, path, raw)
}

func (f *fakeDaemon) do(method, path string, body []byte) (json.RawMessage, error) {
	ctx, cancel := context.WithTimeout(context.Background(), DefaultTimeout)
	defer cancel()
	req, err := http.NewRequestWithContext(ctx, method, "http://"+f.addr+path, bytes.NewReader(body))
	if err != nil {
		return nil, err
	}
	req.Header.Set("Content-Type", "application/json")
	resp, err := http.DefaultClient.Do(req)
	if err != nil {
		return nil, err
	}
	defer resp.Body.Close()
	out, err := io.ReadAll(resp.Body)
	if err != nil {
		return nil, err
	}
	if resp.StatusCode != http.StatusOK {
		return nil, fmt.Errorf("%s %s answered %d: %s", method, path, resp.StatusCode, out)
	}
	return out, nil
}

// protoJSON encodes M as the protojson the fake validates against the
// generated types.
func protoJSON(t *testing.T, m proto.Message) json.RawMessage {
	t.Helper()
	raw, err := protojson.Marshal(m)
	if err != nil {
		t.Fatalf("doom: encode %T: %v", m, err)
	}
	return raw
}

// Script makes METHOD answer RESPONSE from now on.
func (f *fakeDaemon) Script(method string, response json.RawMessage) {
	f.t.Helper()
	if _, err := f.post("/_fake/script", map[string]any{"method": method, "response": response}); err != nil {
		f.t.Fatalf("doom: script %s: %v", method, err)
	}
}

// Push delivers MESSAGE on STREAM ("host", "daemon" or "roster"); SNAPSHOT
// also stores it for later subscribers.
func (f *fakeDaemon) Push(stream string, message json.RawMessage, snapshot bool) {
	f.t.Helper()
	if _, err := f.post("/_fake/push", map[string]any{"stream": stream, "message": message, "snapshot": snapshot}); err != nil {
		f.t.Fatalf("doom: push on %s: %v", stream, err)
	}
}

// fakeCall is one request Emacs made, as the fake recorded it.
type fakeCall struct {
	Method string          `json:"method"`
	Body   json.RawMessage `json:"body"`
}

// Calls is every request Emacs made of METHOD, in order.
func (f *fakeDaemon) Calls(method string) []fakeCall {
	f.t.Helper()
	raw, err := f.do(http.MethodGet, "/_fake/calls", nil)
	if err != nil {
		f.t.Fatalf("doom: read the recorded calls: %v", err)
	}
	var all []fakeCall
	if err := json.Unmarshal(raw, &all); err != nil {
		f.t.Fatalf("doom: decode the recorded calls: %v", err)
	}
	var out []fakeCall
	for _, c := range all {
		if c.Method == method {
			out = append(out, c)
		}
	}
	return out
}

// AwaitCall waits until Emacs has made COUNT requests of METHOD and answers
// the last.
func (f *fakeDaemon) AwaitCall(method string, count int, bound time.Duration) fakeCall {
	f.t.Helper()
	ctx, cancel := context.WithTimeout(context.Background(), bound)
	defer cancel()
	ticker := time.NewTicker(pollInterval)
	defer ticker.Stop()
	for {
		if calls := f.Calls(method); len(calls) >= count {
			return calls[count-1]
		}
		select {
		case <-ticker.C:
		case <-ctx.Done():
			f.t.Fatalf("doom: Emacs made %d %s request(s) within %s, want %d", len(f.Calls(method)), method, bound, count)
		}
	}
}

// DoomWorld is one Doom-tier scenario: a booted Doom Emacs linked to a fake
// daemon.
type DoomWorld struct {
	Emacs *Emacs
	Fake  *fakeDaemon
}

// NewDoomWorld boots a sandboxed Doom Emacs against a fake daemon it adopts.
//
// THE FAKE STARTS FIRST, on the state root StartEmacs will use, so its
// cleanup is registered first and runs LAST: Emacs and its link come down
// while the fake still answers, as with a real daemon. The launcher's daemon
// command refuses to start anything, so a link that came up by spawning a
// daemon instead of adopting the fake fails rather than passing on another.
func NewDoomWorld(t *testing.T, box sandbox) *DoomWorld {
	t.Helper()
	takeEmacsSlot(t)
	fake := startDoomFakeDaemon(t, emacsStateDir(emacsRoot(box)))
	refuser := filepath.Join(box.Scratch(), "doom-tier-spawns-no-daemon.sh")
	if err := os.WriteFile(refuser, []byte("#!/bin/sh\necho 'the Doom tier adopts its fake daemon and never spawns one' >&2\nexit 1\n"), 0o755); err != nil {
		t.Fatalf("doom: write the refusing daemon command: %v", err)
	}
	e := StartEmacs(t, box, EmacsOpts{
		DaemonBinary: refuser,
		ExtraEnv:     []string{"AGENT_REPL_FORBID_VENDOR_CALLS=1"},
		StopDaemon:   fake.Stop,
	})
	e.EnsureDaemon()
	return &DoomWorld{Emacs: e, Fake: fake}
}

// rosterRowJSON is one RosterRow as the daemon authors it: every
// non-optional field populated (the ERT suite's fixture shape,
// test-integration-roster.el), the workspace ID at DIR named NAME.
func rosterRowJSON(id, dir, name string, current bool) map[string]any {
	return map[string]any{
		"workspace":    map[string]any{"workspace": map[string]any{"id": id, "dir": dir}},
		"name":         map[string]any{"text": name},
		"idle":         map[string]any{},
		"current":      map[string]any{"current": current},
		"when":         map[string]any{"noTurnRunning": map[string]any{}},
		"detail":       map[string]any{},
		"closed":       map[string]any{"closed": false},
		"availability": map[string]any{"available": map[string]any{}},
		"lastUpdated":  map[string]any{"atMs": "1"},
	}
}

// rosterJSON is one WatchWorkspaceRosterResponse carrying ROWS in one
// repository section at REPO_DIR, the tab bar drawing them in that order (as
// the daemon always sends one), and CURRENT (a row's id) as the current
// workspace when set.
func rosterJSON(repoDir string, rows []map[string]any, current string) json.RawMessage {
	tabs := make([]any, 0, len(rows))
	var currentRef any
	for _, row := range rows {
		ref := row["workspace"].(map[string]any)["workspace"]
		tabs = append(tabs, map[string]any{"workspace": ref, "drawn": map[string]any{}})
		if current != "" && ref.(map[string]any)["id"] == current {
			currentRef = ref
		}
	}
	roster := map[string]any{
		"repository": map[string]any{
			"sections": []any{map[string]any{
				"key":        map[string]any{"repository": map[string]any{"id": "repo-doom", "dir": repoDir}},
				"header":     map[string]any{"label": map[string]any{"text": filepath.Base(repoDir)}, "count": map[string]any{"workspaces": len(rows)}},
				"rows":       map[string]any{"rows": rows},
				"divergence": map[string]any{"pending": map[string]any{}},
				"expanded":   map[string]any{},
			}},
			"openCount": map[string]any{"workspaces": len(rows)},
		},
		"open":           map[string]any{"label": map[string]any{"text": "Open"}, "expanded": map[string]any{}},
		"hidden":         map[string]any{"header": map[string]any{"label": map[string]any{"text": "Hidden"}, "count": map[string]any{"workspaces": 0}}, "rows": map[string]any{"rows": []any{}}, "expanded": map[string]any{}},
		"recentlyMerged": map[string]any{"header": map[string]any{"label": map[string]any{"text": "recently merged"}, "count": map[string]any{"workspaces": 0}}, "groups": []any{}, "collapsed": map[string]any{}},
		"tabBar":         map[string]any{"tabs": tabs},
	}
	if currentRef != nil {
		roster["current"] = map[string]any{"workspace": currentRef}
	}
	raw, _ := json.Marshal(map[string]any{"roster": roster})
	return raw
}

// registerAs scripts the daemon's answer to Emacs's next RegisterWorkspace:
// the ref ID at DIR.
func (w *DoomWorld) registerAs(t *testing.T, id, dir string) {
	t.Helper()
	ref, _ := json.Marshal(map[string]any{"success": map[string]any{"workspace": map[string]any{"id": id, "dir": dir}}})
	w.Fake.Script("RegisterWorkspace", ref)
}
