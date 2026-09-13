//go:build realtest

package realtest

import (
	"context"
	"encoding/json"
	"fmt"
	"os"
	"os/exec"
	"path/filepath"
	"strings"
	"time"
)

// ELISP IS READ-ONLY HERE. A realtest's input is real key events into the real
// Emacs process (keys.go); emacsclient is for READING — state, logs,
// timestamps — and nothing in this file may perform a user act. That is the
// contract in docs/REALTEST-PLAN.md and it is what makes a realtest evidence:
// an elisp call that performs the act tests the function, not the editor.
//
// The transport is deliberately not emacsclient's printed return value. Two
// reasons, both practical: a printed value is the LISP reader's spelling of
// the result, which needs a lisp reader in Go to consume, and it is truncated
// and re-quoted for large strings — and *Messages* is a large string. So every
// probe writes its answer as JSON to a file and this side reads the file. The
// e2e Emacs layer reaches the same conclusion for the same reason
// (e2e/emacs_test.go), but arrives there through a helper INSTALLED in the
// sandbox profile, which is exactly what a realtest may not do: the owner's
// config gets nothing added to it, so the wrapper travels in the form itself.

// EmacsBinDir is where Emacs.app keeps its command-line tools on macOS.
const EmacsBinDir = "/Applications/Emacs.app/Contents/MacOS/bin"

// EmacsClientPath is the emacsclient a realtest uses. Explicitly the one inside
// Emacs.app rather than whatever is on PATH: a Homebrew emacsclient of a
// different version can fail to speak to this server, and the failure reads as
// "Emacs is not answering".
//
// AGENT_REPL_REALTEST_EMACSCLIENT overrides it, and bin/realtest.sh exports the
// client it used, so the script's refusals and the run's probes can never end
// up talking to two different clients.
var EmacsClientPath = envOr("AGENT_REPL_REALTEST_EMACSCLIENT", filepath.Join(EmacsBinDir, "emacsclient"))

func envOr(name, fallback string) string {
	if value := os.Getenv(name); value != "" {
		return value
	}
	return fallback
}

// probeBound is how long ONE read-only probe may take.
//
// It is a bound on the ROUND TRIP, not on Emacs's work: a probe here reads a
// variable or a buffer, which is microseconds of lisp behind an emacsclient
// connect. It is set at 5s because the thing it must tolerate is an Emacs busy
// in its command loop during startup — the probe waits for the command loop
// like any other client — and the thing it must catch is an Emacs that has
// stopped answering at all. Sized as a bound on a startup that the phases
// themselves measure, so a probe slower than this is reported as a probe
// failure with the elapsed time, never silently retried into a pass.
const probeBound = 5 * time.Second

// probeRingSize caps how many probe-answer files a run leaves behind.
//
// Every read-only probe writes its answer to a file this side reads back, and a
// startup poll fires a probe a second. Naming each answer by a monotonic
// sequence left run 3 with 126 `probe-*.json` files and a run directory nobody
// could read. The names cycle through a small ring instead, so a run keeps a
// short tail for debugging — including the last probe's content — without
// littering. It is small because the answers are transient: a probe reads its
// file back immediately, so a name is free to be reused a few probes later.
const probeRingSize = 8

// probeWrapper is the elisp one probe is evaluated as.
//
// Its own function so the two things that keep an answer honest — the `seq`
// stamp and the handler that catches a `quit` — are testable without a running
// editor. Both were absent, and their absence is what a dropped `C-g` hid
// behind.
func probeWrapper(answer string, seq int, form string) string {
	// `json-encode` rather than `json-serialize`: it accepts any lisp value at
	// top level, where `json-serialize` requires an object or array, and every
	// probe here legitimately answers a bare string or number.
	//
	// THE HANDLER CATCHES `quit` AS WELL AS `error`, and that is not tidiness.
	// A `C-g` that lands while this form is running is handed to
	// `handle_interrupt` rather than stored as a key, and the `quit-flag` it
	// arms is taken by whatever runs next — which, while a realtest is
	// confirming a press, is this probe. `error` does not catch `quit`, so the
	// old form let that press vanish: the probe unwound, wrote nothing, and the
	// caller read the ring slot's previous occupant. Caught here it comes back
	// as a named probe failure, which is a reading nobody is blamed on.
	return fmt.Sprintf(`(progn
  (require 'json)
  (with-temp-file %q
    (insert (condition-case err
                (json-encode (list (cons 'ok t) (cons 'seq %d) (cons 'value (progn %s))))
              ((quit error)
               (json-encode (list (cons 'ok :json-false) (cons 'seq %d)
                                  (cons 'error (error-message-string err))))))))
  t)`, answer, seq, form, seq)
}

// Client is a read-only connection to a running Emacs.
type Client struct {
	// Socket is the server socket path (`--socket-name`). On this machine it
	// is $TMPDIR/emacs<uid>/server; see AGENTS.md "Logs".
	Socket string
	// Scratch is a directory this side owns, where probe answers land.
	Scratch string
	seq     int
}

// probeAnswerPath is where the probe numbered `seq` writes its answer. The name
// cycles through `probeRingSize` slots so a long run of probes leaves a bounded
// set of files rather than one per probe.
func probeAnswerPath(scratch string, seq int) string {
	return filepath.Join(scratch, fmt.Sprintf("probe-%02d.json", seq%probeRingSize))
}

// probeResult is the envelope every probe answers in.
//
// `Seq` IS WHAT MAKES THE ANSWER THIS PROBE'S ANSWER. The file names cycle
// through `probeRingSize` slots, so the file a probe is about to read already
// exists with somebody else's answer in it; a probe whose form never finished
// writing therefore reads the answer of the probe `probeRingSize` earlier and
// cannot tell. That is not a hypothetical: a `C-g` posted into an Emacs that is
// executing a probe's own elisp is handed to `handle_interrupt` and quits the
// probe (delivery.go), the `with-temp-file` never writes, and the caller reads a
// stale `(recent-keys)` as a fresh one — which is exactly the reading behind
// the 2026-09-13 12:13 sweep's six "C-g DID NOT REACH EMACS" findings. The
// stamp is carried in the answer and checked against the probe that asked for
// it, so a stale file is a probe failure and never a reading.
type probeResult struct {
	OK    bool            `json:"ok"`
	Seq   int             `json:"seq"`
	Value json.RawMessage `json:"value"`
	Error string          `json:"error"`
}

// checkProbeAnswer reads one answer file's bytes as the answer to probe `seq`.
//
// A pure function so the staleness check is testable without a running editor,
// which is the whole reason the previous version of it — no check at all —
// survived as long as it did.
func checkProbeAnswer(seq int, form string, body []byte) (json.RawMessage, error) {
	var res probeResult
	if err := json.Unmarshal(body, &res); err != nil {
		return nil, fmt.Errorf("decode the answer to probe %s (%q): %w", summarize(form), string(body), err)
	}
	if res.Seq != seq {
		return nil, fmt.Errorf("the answer file for probe %d (%s) carries the stamp of probe %d, so probe %d "+
			"never wrote it and this is the answer of an earlier probe that happened to land in the same ring "+
			"slot: the probe was interrupted before it could write, and its reading says nothing about the "+
			"editor now", seq, summarize(form), res.Seq, seq)
	}
	if !res.OK {
		return nil, fmt.Errorf("probe %s signalled in emacs: %s", summarize(form), res.Error)
	}
	return res.Value, nil
}

// Alive reports whether the server answers at all.
func (c *Client) Alive(ctx context.Context) bool {
	callCtx, cancel := context.WithTimeout(ctx, probeBound)
	defer cancel()
	cmd := exec.CommandContext(callCtx, EmacsClientPath, "--socket-name", c.Socket, "--eval", "(emacs-pid)")
	return cmd.Run() == nil
}

// Read evaluates one READ-ONLY form and decodes its JSON answer.
//
// The form must produce a value `json-encode` can serialize — a string, a
// number, a list, an alist. That is not a limitation in practice: the rule this
// layer works by is to read the VARIABLE that holds the state, and a form that
// wants a hash table maps over it. Returning a buffer, window or process object
// is a defect in the probe.
//
// The form is evaluated inside a `condition-case`, so a probe that signals
// comes back as an error rather than as a missing file, and the elisp error
// message travels with it.
func (c *Client) Read(ctx context.Context, form string) (json.RawMessage, error) {
	c.seq++
	seq := c.seq
	answer := probeAnswerPath(c.Scratch, seq)

	// THE SLOT IS EMPTIED BEFORE IT IS ASKED FOR. The stamp below catches a
	// stale answer on its own, but leaving the previous occupant of the slot on
	// disk means a probe that dies can be followed by one that reads bytes
	// nobody wrote for it; removing it first makes the ordinary failure a
	// MISSING file, which says plainly that the probe never answered.
	_ = os.Remove(answer)

	wrapper := probeWrapper(answer, seq, form)

	callCtx, cancel := context.WithTimeout(ctx, probeBound)
	defer cancel()
	started := time.Now()
	cmd := exec.CommandContext(callCtx, EmacsClientPath, "--socket-name", c.Socket, "--eval", wrapper)
	out, err := cmd.CombinedOutput()
	if err != nil {
		return nil, fmt.Errorf("read-only probe %s took %s and failed: %w; emacsclient said: %s",
			summarize(form), time.Since(started).Round(time.Millisecond), err, strings.TrimSpace(string(out)))
	}

	body, err := os.ReadFile(answer)
	if err != nil {
		return nil, fmt.Errorf("read the answer to probe %s: %w", summarize(form), err)
	}
	return checkProbeAnswer(seq, form, body)
}

// ReadString is Read for a probe that answers a string.
func (c *Client) ReadString(ctx context.Context, form string) (string, error) {
	raw, err := c.Read(ctx, form)
	if err != nil {
		return "", err
	}
	var s string
	if err := json.Unmarshal(raw, &s); err != nil {
		return "", fmt.Errorf("probe %s answered %s, which is not a string: %w", summarize(form), raw, err)
	}
	return s, nil
}

// ReadInt is Read for a probe that answers a number.
func (c *Client) ReadInt(ctx context.Context, form string) (int, error) {
	raw, err := c.Read(ctx, form)
	if err != nil {
		return 0, err
	}
	var n int
	if err := json.Unmarshal(raw, &n); err != nil {
		return 0, fmt.Errorf("probe %s answered %s, which is not a number: %w", summarize(form), raw, err)
	}
	return n, nil
}

// Messages reads Emacs's whole *Messages* buffer.
//
// `buffer-substring-no-properties` rather than `buffer-string`: text properties
// would travel as lisp structure through the JSON encoder and the harvester
// reads lines, not faces.
func (c *Client) Messages(ctx context.Context) (string, error) {
	return c.ReadString(ctx, `(with-current-buffer "*Messages*"
    (buffer-substring-no-properties (point-min) (point-max)))`)
}

// MessagesSize reads the *Messages* buffer's size, for a run that must read
// only the tail added afterwards.
func (c *Client) MessagesSize(ctx context.Context) (int, error) {
	return c.ReadInt(ctx, `(with-current-buffer "*Messages*" (buffer-size))`)
}

// Warnings reads Emacs's whole *Warnings* buffer — the one the owner actually
// sees, because `display-warning` pops it up.
//
// A BUFFER THAT DOES NOT EXIST IS AN EMPTY STRING, not an error. Emacs creates
// *Warnings* lazily, on the first warning, so its absence is the healthiest
// possible answer and reporting it as a failed probe would turn a clean editor
// into a scan that could not run.
func (c *Client) Warnings(ctx context.Context) (string, error) {
	return c.ReadString(ctx, `(let ((buffer (get-buffer "*Warnings*")))
    (if buffer
        (with-current-buffer buffer
          (buffer-substring-no-properties (point-min) (point-max)))
      ""))`)
}

// WarningsSize is the *Warnings* buffer's size, for a scan that must read only
// what was added after a recorded point. A buffer that does not exist yet is
// zero, for the reason Warnings gives.
func (c *Client) WarningsSize(ctx context.Context) (int, error) {
	return c.ReadInt(ctx, `(let ((buffer (get-buffer "*Warnings*")))
    (if buffer (with-current-buffer buffer (buffer-size)) 0))`)
}

// IdleSeconds is how long Emacs has been idle, which is the closest thing to
// "is a human using this editor" that the editor itself can answer.
//
// `current-idle-time` answers nil when Emacs is NOT idle — it is mid-command —
// which reads here as zero: not idle at all is the most human state there is.
func (c *Client) IdleSeconds(ctx context.Context) (float64, error) {
	raw, err := c.Read(ctx, `(let ((idle (current-idle-time)))
    (if idle (float (float-time idle)) 0.0))`)
	if err != nil {
		return 0, err
	}
	var f float64
	if err := json.Unmarshal(raw, &f); err != nil {
		return 0, fmt.Errorf("the idle-time probe answered %s, which is not a number: %w", raw, err)
	}
	return f, nil
}

// Kill quits the Emacs process through its own server.
//
// This is the ONE write this file performs, it exists only for the takeover
// path, and bin/realtest.sh is the only thing that may reach it — after the
// backups, and only with AGENT_REPL_REALTEST_TAKEOVER=1. It is `kill-emacs`
// rather than `save-buffers-kill-emacs` deliberately: the second one PROMPTS,
// and a prompt on a headless takeover hangs the run holding the owner's editor
// open on a modal question nobody will answer.
//
// It does not save. A realtest that would lose the owner's unsaved work must
// not run, which is what the human-in-Emacs refusal upstream of it is for.
func (c *Client) Kill(ctx context.Context) error {
	callCtx, cancel := context.WithTimeout(ctx, probeBound)
	defer cancel()
	cmd := exec.CommandContext(callCtx, EmacsClientPath, "--socket-name", c.Socket, "--eval", "(kill-emacs)")
	out, err := cmd.CombinedOutput()
	if err != nil {
		// A server that dies mid-call cannot answer the call that killed it,
		// so a non-zero exit here is expected and is not itself the verdict:
		// whether Emacs is gone is decided by polling the socket afterwards.
		return fmt.Errorf("emacsclient (kill-emacs) exited %w; it said: %s", err, strings.TrimSpace(string(out)))
	}
	return nil
}

// summarize shortens a form for an error message.
func summarize(form string) string {
	flat := strings.Join(strings.Fields(form), " ")
	if len(flat) > 90 {
		return flat[:87] + "..."
	}
	return flat
}
