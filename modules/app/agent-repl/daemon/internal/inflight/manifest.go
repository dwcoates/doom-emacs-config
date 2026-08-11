// manifest.go — THE BEFORE/AFTER BOUNCE MANIFEST.
//
// # What it is for
//
// A bounce promises not to interrupt anyone's work. Before this file the only
// evidence for or against that promise was a process count, and a count is
// satisfied by a complete kill-and-respawn. bounceledger already fixed that for
// SHIM PROCESSES, by writing down which pid was left behind and judging it
// against the kernel's answer afterwards. This is the same discipline one layer
// in: the manifest writes down which WORK ITEMS were in flight, by identity,
// and judges each one afterwards.
//
// # The vocabulary is bounceledger's, extended — not a new taxonomy
//
// bounceledger rules PRESERVED / ROLLED / DIED / UNKNOWN over processes. A work
// item's outcomes are the same shape with one substitution: a work item has no
// "ordered kill", it has a legitimate COMPLETION.
//
//   - PRESERVED — the same identity is still live on the far side.
//   - COMPLETED — the item is gone AND its terminal result was actually
//     recorded. The recording is required: "it is not in the after set" is
//     equally true of a completion and of a death, and only the terminal
//     record tells them apart.
//   - INTERRUPTED — the item was live, is gone, and no legitimate completion
//     was recorded. This is the incident. It is bounceledger's DIED, named for
//     work rather than for a process.
//   - UNKNOWN — it cannot be determined. It NEVER collapses into COMPLETED,
//     for the same reason a failed lock probe never collapses into DIED:
//     reporting a completion nobody witnessed is a claim the evidence does not
//     carry, and it is exactly how "nothing else in flight" came to be printed
//     beside a running bubble.
//
// # Why it is written where it is
//
// The daemon's workspace `daemon.log` symlink is RE-POINTED at a fresh target
// on every daemon restart (see logging-contract.md, "Persistence layout"), so
// the log covering a death is no longer reachable through the canonical path
// after the restart that killed it. A manifest that describes a bounce and is
// unreadable after that bounce is useless.
//
// So the manifest is its own append-only regular file beside those links —
// `<workspace>/.claude/emacs/inflight-manifest.jsonl` — which nothing re-points
// and nothing truncates on restart. It is opened with O_APPEND|O_NOFOLLOW per
// write and closed again, so the outgoing daemon's START line and the incoming
// daemon's END lines land in ONE file in order, across the process boundary the
// bounce puts between them. O_NOFOLLOW keeps the contract's rule that a runtime
// never follows a workspace-provided symlink as a sink.
package inflight

import (
	"encoding/json"
	"errors"
	"fmt"
	"os"
	"path/filepath"
	"time"
)

// The per-item dispositions. See the file comment for why these four and why
// UNKNOWN is not foldable.
const (
	DispositionPreserved   = "PRESERVED"
	DispositionCompleted   = "COMPLETED"
	DispositionInterrupted = "INTERRUPTED"
	DispositionUnknown     = "UNKNOWN"
)

// Phase names which half of a bounce a manifest record belongs to.
type Phase string

const (
	// PhaseStart is written by the daemon that is about to bounce, before it
	// touches anything.
	PhaseStart Phase = "start"
	// PhaseEnd is written once the bounce has landed and every item has been
	// judged.
	PhaseEnd Phase = "end"
)

// Judgement is one work item's fate across the bounce.
type Judgement struct {
	Item        Item   `json:"item"`
	Disposition string `json:"disposition"`
	// Reason is REQUIRED for every disposition, including PRESERVED. A verdict
	// that cannot say what evidence produced it is the account this whole
	// mechanism exists to stop accepting.
	Reason string `json:"reason"`
}

// Record is one manifest line. A start record carries the set; an end record
// carries the set AND the judgements.
type Record struct {
	Timestamp  string      `json:"timestamp"`
	Phase      Phase       `json:"phase"`
	BounceID   string      `json:"bounce_id"`
	Workspace  string      `json:"workspace"`
	SessionID  string      `json:"session_id,omitempty"`
	Cause      string      `json:"cause"`
	Known      bool        `json:"known"`
	Unknown    string      `json:"unknown_reason,omitempty"`
	Items      []Item      `json:"items"`
	Judgements []Judgement `json:"judgements,omitempty"`
	// Tally is a convenience for a human scanning the file. It is derived from
	// Judgements and is never the authority: the identities above are.
	Tally map[string]int `json:"tally,omitempty"`
}

// CompletionOracle answers, for one item that is no longer live, whether its
// terminal result was actually recorded.
//
// ITS ERROR IS NOT A FALSE. A completion the oracle could not read is UNKNOWN,
// never a completion and never an interruption: the caller failed to observe,
// which is a different fact from either outcome.
type CompletionOracle func(Item) (completed bool, reason string, err error)

// Reconcile judges every item that was in flight before the bounce against the
// set observed after it.
//
// The two unknown arms are handled FIRST and they are not symmetrical:
//
//   - An unknown BEFORE set means nothing can be judged at all, because the
//     population being judged is itself unknown. It yields no judgements and a
//     loud record, rather than an empty list that would read as "nothing was in
//     flight, so nothing was hurt".
//   - An unknown AFTER set means the population is known and its fate is not,
//     so every item is judged UNKNOWN individually.
func Reconcile(before, after Set, oracle CompletionOracle) ([]Judgement, error) {
	if oracle == nil {
		return nil, errors.New("inflight: reconciling a bounce requires a completion oracle; without one a vanished item cannot be told from a finished one")
	}
	if !before.Known() {
		return nil, fmt.Errorf("%w: %s", ErrBeforeUnknown, before.Reason())
	}
	items := before.Items()
	out := make([]Judgement, 0, len(items))
	for _, item := range items {
		if !after.Known() {
			out = append(out, Judgement{
				Item:        item,
				Disposition: DispositionUnknown,
				Reason:      "this item was in flight before the bounce and the post-bounce set is UNKNOWN, so whether it survived is unobserved: " + after.Reason(),
			})
			continue
		}
		if after.Has(item) {
			out = append(out, Judgement{
				Item:        item,
				Disposition: DispositionPreserved,
				Reason:      "the same identity is still live after the bounce",
			})
			continue
		}
		completed, why, err := oracle(item)
		switch {
		case err != nil:
			out = append(out, Judgement{
				Item:        item,
				Disposition: DispositionUnknown,
				Reason:      fmt.Sprintf("the item is gone and its terminal record could not be read, so its fate is unobserved: %v", err),
			})
		case completed:
			out = append(out, Judgement{
				Item:        item,
				Disposition: DispositionCompleted,
				Reason:      "the item ended legitimately and its terminal result is recorded: " + why,
			})
		default:
			out = append(out, Judgement{
				Item:        item,
				Disposition: DispositionInterrupted,
				Reason:      "the item was live before the bounce, is gone after it, and NO terminal result was ever recorded — nobody decided this and its work is lost: " + why,
			})
		}
	}
	return out, nil
}

// ErrBeforeUnknown marks the reconciliation that cannot be attempted because
// the pre-bounce population was never observed. It is a sentinel so a caller
// can record it as the blind spot it is rather than as a clean bounce.
var ErrBeforeUnknown = errors.New("inflight: the pre-bounce in-flight set is UNKNOWN, so no item can be judged and this bounce makes no claim about what it interrupted")

// Tally counts dispositions for the human summary line.
func Tally(judgements []Judgement) map[string]int {
	out := map[string]int{}
	for _, j := range judgements {
		out[j.Disposition]++
	}
	return out
}

// ManifestFileName is the manifest's stable basename inside
// `<workspace>/.claude/emacs/`. It is a plain file and never a link, so the
// symlink re-pointing every daemon restart performs cannot detach a bounce's
// END record from its own START record.
const ManifestFileName = "inflight-manifest.jsonl"

// ManifestPath resolves the manifest for a workspace directory.
func ManifestPath(workspaceDir string) string {
	return filepath.Join(workspaceDir, ".claude", "emacs", ManifestFileName)
}

// Append writes one record to the workspace's manifest.
//
// It opens, appends and closes per call rather than holding a descriptor,
// because the two records a bounce produces are written by TWO DIFFERENT
// DAEMON PROCESSES: a held descriptor could not span them, and that span is
// the entire value of the file.
//
// A failure is returned, never swallowed. The caller records it against the
// workspace, because a manifest that could not be written is a bounce with no
// account — and an unaccounted bounce must read as a defect, not as a clean one.
func Append(workspaceDir string, rec Record) error {
	if workspaceDir == "" {
		return errors.New("inflight: appending a manifest record requires a workspace directory")
	}
	if rec.Workspace == "" {
		rec.Workspace = workspaceDir
	}
	if rec.Timestamp == "" {
		rec.Timestamp = Timestamp(time.Now())
	}
	if rec.Items == nil {
		rec.Items = []Item{}
	}
	path := ManifestPath(workspaceDir)
	if err := os.MkdirAll(filepath.Dir(path), 0o700); err != nil {
		return fmt.Errorf("inflight: preparing the manifest directory for %q: %w", workspaceDir, err)
	}
	payload, err := json.Marshal(rec)
	if err != nil {
		return fmt.Errorf("inflight: encoding the manifest record for %q: %w", workspaceDir, err)
	}
	// O_NOFOLLOW keeps the logging contract's rule that a runtime never follows
	// a workspace-provided symlink as a durable sink, and O_APPEND is what lets
	// the incoming daemon's END record join the outgoing daemon's START record
	// in one file.
	f, err := os.OpenFile(path, os.O_WRONLY|os.O_CREATE|os.O_APPEND|syscallNoFollow, 0o600)
	if err != nil {
		return fmt.Errorf("inflight: opening the manifest for %q: %w", workspaceDir, err)
	}
	defer f.Close()
	if _, err := f.Write(append(payload, '\n')); err != nil {
		return fmt.Errorf("inflight: writing the manifest record for %q: %w", workspaceDir, err)
	}
	return nil
}

// Read returns the manifest's records oldest-first. A missing manifest is an
// empty one and not an error: a workspace that has never been bounced has
// nothing to say.
func Read(workspaceDir string) ([]Record, error) {
	payload, err := os.ReadFile(ManifestPath(workspaceDir))
	if err != nil {
		if os.IsNotExist(err) {
			return nil, nil
		}
		return nil, fmt.Errorf("inflight: reading the manifest for %q: %w", workspaceDir, err)
	}
	var out []Record
	for _, line := range splitLines(payload) {
		if len(line) == 0 {
			continue
		}
		var rec Record
		if err := json.Unmarshal(line, &rec); err != nil {
			return nil, fmt.Errorf("inflight: decoding a manifest record for %q: %w", workspaceDir, err)
		}
		out = append(out, rec)
	}
	return out, nil
}

func splitLines(payload []byte) [][]byte {
	var out [][]byte
	start := 0
	for i, b := range payload {
		if b == '\n' {
			out = append(out, payload[start:i])
			start = i + 1
		}
	}
	if start < len(payload) {
		out = append(out, payload[start:])
	}
	return out
}

// Timestamp renders an instant in the shared representation every agent-repl
// runtime uses (logging-contract.md, "Timestamp representation"): RFC 3339,
// local zone, exactly six fractional digits, numeric offset. Fixed width is
// what lets manifest lines sort lexically beside log lines.
func Timestamp(at time.Time) string {
	return at.Format("2006-01-02T15:04:05.000000-07:00")
}
