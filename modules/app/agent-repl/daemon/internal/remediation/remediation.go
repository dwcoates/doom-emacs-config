// Package remediation launches a one-shot headless Claude analyst when a
// frontend discovers its session has vanished from the daemon (the
// webapp's "session gone" state).
//
// "Session gone" is not a user-facing condition anyone should have to
// live with: it means the daemon no longer knows an id its frontend is
// still holding, so the conversation is unreachable. Rather than merely
// painting the topbar red, the daemon dispatches an analyst that
// diagnoses the termination, reports the fastest recovery, and opens a
// workspace whose job is to engineer that failure class out of the
// system entirely.
package remediation

import (
	"fmt"
	"strings"
	"sync"
	"time"

	"claude-repld/internal/protocol"
)

// DefaultBin is the CLI invoked when Config.Bin is empty.
const DefaultBin = "claude"

// analystSubject names the analyst in every ungated log record, so the boot
// note and the dispatch note read as one another's halves.
const analystSubject = "the headless remediation analyst"

// Model is the analyst model. Diagnosing a lost session and designing
// the durable fix is the hardest reasoning the daemon ever asks for, so
// it is pinned to the strongest tier rather than the session's model.
const Model = "opus"

// StartFunc launches the analyst process. It MUST return promptly (the
// caller is an HTTP handler), so implementations start the process and
// reap it asynchronously. Injected so tests never exec a real CLI.
type StartFunc func(argv []string, dir string) error

// Config assembles a Runner.
type Config struct {
	// Bin is the claude CLI path; empty means DefaultBin from PATH.
	Bin string
	// Dir is the analyst's working directory: the checkout it diagnoses
	// and opens the resilience workspace against. Required.
	Dir string
	// PermissionMode is the analyst's --permission-mode. Empty omits the
	// flag entirely, which leaves the analyst gated: nobody is at the
	// keyboard to answer a headless prompt, so every tool call it makes
	// is auto-denied and it can only report, never remediate. Loosening
	// this is the operator's decision, so it is never assumed here.
	//
	// An UNGATED mode (protocol.UngatedPermissionMode) additionally
	// requires AllowUngated; New refuses without it.
	PermissionMode string
	// AllowUngated is the operator's DELIBERATE consent to run the analyst
	// with no permission gate at all, and is required whenever
	// PermissionMode names an ungated mode.
	//
	// THE TRADEOFF this consent acknowledges: the analyst is a headless
	// `claude -p` with nobody at the keyboard, so under any gated mode
	// every tool call it makes is auto-denied and it can do nothing but
	// narrate a diagnosis into the daemon log. Reading the logs, running
	// git, and driving the workspace-creation skill — the whole reason the
	// analyst exists — all need it ungated. So ungated is the only mode in
	// which the feature WORKS, and this flag is where an operator says they
	// understand what that buys: a `claude` running unattended, against a
	// real checkout, approving its own tool calls.
	//
	// Requiring it earns something concrete rather than ceremony: nothing
	// that spawns claude-repld can arrive at an ungated analyst by passing
	// a mode string alone.
	AllowUngated bool
	// Start launches the process. Required.
	Start StartFunc
	// Logf records dispatches. Required.
	Logf func(format string, args ...any)
}

// Runner dispatches at most one analyst per vanished session id.
type Runner struct {
	bin            string
	dir            string
	permissionMode string
	allowUngated   bool
	start          StartFunc
	logf           func(format string, args ...any)
	// bootedAt anchors the daemon-uptime datum handed to the analyst: a
	// session missing from a daemon that booted seconds ago points at a
	// daemon restart, while one missing from a long-lived daemon points
	// at a per-session teardown.
	bootedAt time.Time

	mu         sync.Mutex
	dispatched map[string]struct{}
}

// New builds a Runner. bootedAt is the daemon's start time.
func New(cfg Config, bootedAt time.Time) (*Runner, error) {
	if cfg.Dir == "" {
		return nil, fmt.Errorf("remediation: Dir is required")
	}
	if cfg.Start == nil {
		return nil, fmt.Errorf("remediation: Start is required")
	}
	if cfg.Logf == nil {
		return nil, fmt.Errorf("remediation: Logf is required")
	}
	// The ungated-analyst consent gate, checked HERE — at construction, from
	// the daemon's own flags — rather than at dispatch.
	//
	// A refusal at dispatch would land at the worst possible moment: a session
	// has just been lost, the frontend is showing "session gone", and the one
	// thing that could recover it declines over a configuration fact that was
	// knowable at boot. This is a misconfiguration, and the daemon already
	// exits on those (-accounts, --shim), so it fails the same way and at the
	// same time.
	//
	// Refused, never downgraded to a gated mode: a silently gated analyst
	// still spawns, still costs an Opus run, and can only narrate — an
	// operator would see "remediation dispatched" and get nothing.
	if protocol.UngatedPermissionMode(cfg.PermissionMode) && !cfg.AllowUngated {
		return nil, fmt.Errorf(
			"remediation: permission mode %q runs the headless analyst with NO permission gate (it auto-approves its own tool calls against %s); pass the ungated-analyst consent to confirm that is intended, or choose a gated mode",
			cfg.PermissionMode, cfg.Dir)
	}
	bin := cfg.Bin
	if bin == "" {
		bin = DefaultBin
	}
	// The boot-time half of the record: an ungated analyst is configured now,
	// long before any session is lost, and the operator learns it at the
	// moment the daemon adopts the configuration rather than at 3am when it
	// finally fires.
	if note := protocol.UngatedNote(analystSubject, cfg.PermissionMode, cfg.AllowUngated); note != "" {
		cfg.Logf("remediation: analyst configured for %s%s", cfg.Dir, note)
	}
	return &Runner{
		bin:            bin,
		dir:            cfg.Dir,
		permissionMode: cfg.PermissionMode,
		allowUngated:   cfg.AllowUngated,
		start:          cfg.Start,
		logf:           cfg.Logf,
		bootedAt:       bootedAt,
		dispatched:     map[string]struct{}{},
	}, nil
}

// Argv is the analyst command line for a brief. An empty permissionMode
// omits --permission-mode, leaving the CLI's own default in force.
func Argv(bin, prompt, permissionMode string) []string {
	argv := []string{bin, "-p", prompt, "--model", Model}
	if permissionMode != "" {
		argv = append(argv, "--permission-mode", permissionMode)
	}
	return argv
}

// Prompt is the analyst's brief for a session that vanished from a
// daemon whose uptime is uptime.
func Prompt(sessionID string, uptime time.Duration) string {
	return strings.Join([]string{
		fmt.Sprintf("The claude-repl frontend has lost session %s.", sessionID),
		fmt.Sprintf("Its webapp reconnected, probed GET /sessions, and the claude-repld daemon (up %s at the time of the probe) no longer listed that id, so the topbar now reads \"session gone\" and the conversation is unreachable.", uptime.Round(time.Second)),
		"",
		"You are the automated remediation for that failure. Work in this checkout (modules/app/agent-repl is the frontend: webapp/ is the browser client, daemon/ is claude-repld, agent-shim/claude/shim/ is the per-session SDK subprocess).",
		"",
		"Do all of the following, in order:",
		"",
		"1. Diagnose why the session was terminated. A short daemon uptime points at a daemon restart or crash (the session map is in-memory and does not survive one); a long uptime points at a per-session teardown. Read the daemon log buffer, ~/.claude-emacs/doom-agent-repl.log, and every path in daemon/internal that can drop or fail to register a session id.",
		"2. Report the fastest recovery for THIS session, naming the concrete commands. The durable CLI session uuid is the resume target, and POST /sessions accepts a `resume` field.",
		"3. Then create a workspace, via the create-or-update-workspace skill, whose prompt implements the durable fix. Prefer making the daemon preemptively resilient to this failure class (so \"session gone\" can never be reached) over merely making it recoverable after the fact. Persisting the session registry across daemon restarts and rebinding a live frontend to a resumed session are the shapes to weigh.",
		"",
		"Do not stop at the diagnosis: the workspace is the deliverable.",
	}, "\n")
}

// Start dispatches the analyst for sessionID, reporting whether this
// call is the one that launched it. A repeat call for an id already
// dispatched is a no-op reporting false: every reconnect attempt by
// every open tab funnels here, and one lost session warrants exactly one
// analyst.
func (r *Runner) Start(sessionID string) (bool, error) {
	if sessionID == "" {
		return false, fmt.Errorf("remediation: empty session id")
	}
	r.mu.Lock()
	if _, seen := r.dispatched[sessionID]; seen {
		r.mu.Unlock()
		return false, nil
	}
	r.dispatched[sessionID] = struct{}{}
	r.mu.Unlock()

	argv := Argv(r.bin, Prompt(sessionID, time.Since(r.bootedAt)), r.permissionMode)
	if err := r.start(argv, r.dir); err != nil {
		// The dispatch record stays: a spawn that fails once (missing
		// binary, fork failure) will fail identically on every one of
		// the retries the reconnect loop would otherwise drive, and the
		// error is surfaced to the caller either way.
		return false, fmt.Errorf("remediation: launch analyst for %s: %w", sessionID, err)
	}
	// The dispatch half of the record: every ungated analyst SPAWN is named,
	// with the consent that admitted it, exactly as an ungated session create
	// is. The boot-time note says one is configured; this says one is running.
	r.logf("remediation: dispatched %s analyst for lost session %s in %s%s",
		Model, sessionID, r.dir,
		protocol.UngatedNote(analystSubject, r.permissionMode, r.allowUngated))
	return true, nil
}
