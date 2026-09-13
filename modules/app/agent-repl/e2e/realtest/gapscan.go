//go:build realtest

package realtest

import (
	"encoding/json"
	"fmt"
	"os"
	"path/filepath"
	"sort"
	"strings"
	"time"
)

// THE HOURS BETWEEN SWEEPS ARE NOT HARVESTED, AND THEY ARE WHERE THE OWNER
// LIVES.
//
// Every harvest in this package reads ONE realtest's own window: the snapshot
// is taken when the test starts and the window closes when it ends. That is
// the right window for judging a realtest. It is the wrong window for judging
// the module, because the editor keeps running after the sweep and the owner
// keeps using it — and a deploy restart, a boot catch-up, a stale registry row
// warning arriving twenty minutes later all land in a gap no run reads.
//
// The 2026-09-13 complaint is exactly that gap. The warning the owner saw
// ("cannot host a durable log sink ... [MISSING]") was written by their editor
// between sweeps, and every sweep since had reported a clean harvest.
//
// So a sweep now begins by reading the gap it is standing at the end of: from
// the moment the PREVIOUS sweep finished to now, across every source the
// in-window harvest already knows, plus the live editor's own *Messages* and
// *Warnings* buffers. Every warning, error, malformed record and stray stderr
// line in that span is written verbatim under "## Between sweeps" and makes
// the sweep exit non-zero, exactly like an in-window finding. There is no
// allowlist here either.
//
// THE HIGH-WATER MARK IS A SNAPSHOT, NOT JUST A TIMESTAMP. Two of the sources
// carry no timestamps at all — the services' `.err.log` and Emacs's own
// buffers — so a time alone could only ever report them whole, every sweep,
// forever. The mark therefore holds the same Snapshot the in-window harvest
// uses (inode -> size, so a rotation is not a loss) and the two buffer sizes,
// and the gap scan reads each source from where the last sweep left it.

// sweepMarkName is the file the high-water mark lives in, beside the run
// directories it bounds.
const sweepMarkName = "last-sweep-end"

// SweepMark is where every source stood when the last sweep finished.
type SweepMark struct {
	// EndedAt is the moment the last sweep finished. It is the START of the
	// next sweep's between-sweeps window.
	EndedAt time.Time `json:"ended_at"`
	// Snapshot is every log source's identity and size at that moment, so a
	// timestamped source is read from where the last sweep left it rather
	// than from the beginning.
	Snapshot Snapshot `json:"snapshot"`
	// MessagesSize and WarningsSize are the two Emacs buffers' sizes, which
	// serve the same purpose for the two sources that have no timestamps and
	// no inodes. A NEGATIVE value means the buffer could not be read at all
	// when the mark was written — an editor that was not answering — which is
	// different from an empty buffer and is carried through as such.
	MessagesSize int `json:"messages_size"`
	WarningsSize int `json:"warnings_size"`
}

// SweepMarkPath is where the mark lives: beside the run directories, under the
// state root, so it travels with the thing it is about.
func SweepMarkPath(stateDir string) string {
	return filepath.Join(stateDir, "realtest", sweepMarkName)
}

// WriteSweepMark persists the mark.
//
// Written through a temporary file and renamed, because a sweep can be
// interrupted mid-write and a half-written mark would be read next time as a
// corrupt one — which sends the gap scan to its fallback and silently widens
// the window rather than reporting anything.
func WriteSweepMark(path string, mark SweepMark) error {
	if err := os.MkdirAll(filepath.Dir(path), 0o755); err != nil {
		return fmt.Errorf("create the directory for the sweep mark %s: %w", path, err)
	}
	body, err := json.MarshalIndent(mark, "", "  ")
	if err != nil {
		return fmt.Errorf("render the sweep mark: %w", err)
	}
	temp := path + ".writing"
	if err := os.WriteFile(temp, append(body, '\n'), 0o644); err != nil {
		return fmt.Errorf("write the sweep mark %s: %w", temp, err)
	}
	if err := os.Rename(temp, path); err != nil {
		return fmt.Errorf("put the sweep mark in place at %s: %w", path, err)
	}
	return nil
}

// ReadSweepMark reads the mark. A mark that is not there is reported as
// os.ErrNotExist so the caller can fall back deliberately rather than by
// mistaking an absent mark for a zero one — a zero EndedAt would make the
// window the whole of recorded history.
func ReadSweepMark(path string) (SweepMark, error) {
	body, err := os.ReadFile(path)
	if err != nil {
		return SweepMark{}, err
	}
	var mark SweepMark
	if err := json.Unmarshal(body, &mark); err != nil {
		return SweepMark{}, fmt.Errorf("decode the sweep mark %s: %w", path, err)
	}
	if mark.EndedAt.IsZero() {
		return SweepMark{}, fmt.Errorf("the sweep mark %s carries no end time, so it bounds nothing", path)
	}
	return mark, nil
}

// NewestManifestTime is the FALLBACK start for the between-sweeps window: the
// modification time of the newest MANIFEST.md under the realtest root.
//
// It exists because the mark is new and the run directories are not. The first
// sweep after this lands has no mark to read, and starting from "the newest
// manifest" reads the gap since the last sweep anyone actually ran instead of
// reporting nothing at all. It is deliberately the manifest's mtime rather
// than the run directory's: a directory is created when a sweep STARTS, and
// starting the window there would re-report every finding the sweep itself
// already reported in window.
func NewestManifestTime(realtestRoot string) (time.Time, bool) {
	var newest time.Time
	_ = filepath.WalkDir(realtestRoot, func(path string, entry os.DirEntry, err error) error {
		if err != nil || entry.IsDir() || entry.Name() != "MANIFEST.md" {
			return nil //nolint:nilerr // an unreadable subtree is not a reason to abandon the walk
		}
		info, statErr := entry.Info()
		if statErr != nil {
			return nil
		}
		if info.ModTime().After(newest) {
			newest = info.ModTime()
		}
		return nil
	})
	return newest, !newest.IsZero()
}

// warningsBufferSource is the name findings from Emacs's *Warnings* buffer are
// reported under.
const warningsBufferSource = "*Warnings*"

// HarvestWarningsBuffer makes a finding of EVERY line of Emacs's *Warnings*
// buffer at or after tailFrom.
//
// There is no pattern set here, and that is not laziness. *Messages* is a
// mixed buffer where a severity shape is what separates a problem from an echo
// (messages.go carries those patterns and the reason for each). *Warnings*
// holds nothing else: a line is in it because `display-warning` put it there,
// which is a report of a problem by construction. Matching patterns against it
// could only ever drop warnings whose wording nobody anticipated, and the
// harvest has no allowlist.
//
// tailFrom is the buffer size the previous sweep recorded. A buffer SMALLER
// than that means Emacs restarted (or the buffer was cleared) and every line
// in it is new, so the whole buffer is read — the same rule the inode snapshot
// applies to a file that shrank.
func HarvestWarningsBuffer(text string, tailFrom int, workspaces []Workspace) []Finding {
	if tailFrom > 0 && tailFrom <= len(text) {
		text = text[tailFrom:]
	}
	byName := make(map[string]string, len(workspaces))
	for _, ws := range workspaces {
		if ws.Name != "" {
			byName[ws.Name] = ws.ID
		}
	}

	var findings []Finding
	for i, line := range strings.Split(text, "\n") {
		if strings.TrimSpace(line) == "" {
			continue
		}
		findings = append(findings, Finding{
			Kind:      KindMessagesLine,
			Source:    warningsBufferSource,
			Path:      "(emacs buffer)",
			Line:      i + 1,
			Workspace: messagesWorkspace(line, byName),
			Raw:       line,
			Note: "Emacs's own *Warnings* buffer holds this, which means `display-warning` reported it. " +
				"Nothing else is ever written there",
		})
	}
	return findings
}

// GapScan is one between-sweeps reading: the window it covered, what it read,
// and everything it found.
type GapScan struct {
	// From and To bound the window. From is the previous sweep's end.
	From, To time.Time
	// FromSource says where From came from — the mark, or the newest manifest
	// — because a reader has to know whether the window is exact.
	FromSource string
	// Findings is every warning, error, malformed record, stray stderr line
	// and *Warnings* line inside the window.
	Findings []Finding
	// Notes are the scan's own statements: sources it could not read, an
	// editor that was not answering.
	Notes []string
}

// Count is how many findings the gap held. The sweep exits non-zero when it is
// non-zero.
func (g GapScan) Count() int { return len(g.Findings) }

// betweenSweepsDirName is the subdirectory of the run directory the gap scan
// writes to.
//
// ITS OWN SUBDIRECTORY, like every realtest after the first, and for the same
// reason: realtest 1 writes MANIFEST.md to the run directory itself, so a gap
// scan writing there too would have its report overwritten by the first
// realtest that ran.
const betweenSweepsDirName = "between-sweeps"

// WriteGapScanManifest writes the gap scan's own MANIFEST.md, with the section
// the sweep's output points at, and the full harvest beside it.
func WriteGapScanManifest(runDir string, scan GapScan) (string, error) {
	dir := filepath.Join(runDir, betweenSweepsDirName)
	if err := os.MkdirAll(dir, 0o755); err != nil {
		return "", fmt.Errorf("create the between-sweeps directory %s: %w", dir, err)
	}
	path := filepath.Join(dir, "MANIFEST.md")
	if err := os.WriteFile(path, []byte(scan.Render()), 0o644); err != nil {
		return "", fmt.Errorf("write %s: %w", path, err)
	}
	if _, err := WriteFullHarvest(dir, scan.Findings); err != nil {
		return "", err
	}
	return path, nil
}

// Render is the "## Between sweeps" section, verbatim, in the same shape the
// in-window harvest is reported in — a class per (source, operation, level,
// kind) with its count and one sample, and every record kept in
// HARVEST-FULL.jsonl beside it.
//
// The shape is shared deliberately: a reader who can read one manifest can
// read this one, and a finding that appears in both reads the same in both.
func (g GapScan) Render() string {
	var b strings.Builder

	b.WriteString("# Between sweeps\n\n")
	fmt.Fprintf(&b, "Window: `%s` to `%s` (%s), taken from %s.\n\n",
		g.From.Format(time.RFC3339Nano), g.To.Format(time.RFC3339Nano),
		g.To.Sub(g.From).Round(time.Second), g.FromSource)
	b.WriteString("Everything below was written by the module while NO realtest was running: after the\n")
	b.WriteString("previous sweep ended, in the hours the owner has the editor to themselves. No\n")
	b.WriteString("realtest's own window covers it, which is why it is read here.\n\n")

	if len(g.Notes) > 0 {
		b.WriteString("## What the scan could and could not read\n\n")
		for _, note := range g.Notes {
			fmt.Fprintf(&b, "- %s\n", note)
		}
		b.WriteString("\n")
	}

	b.WriteString("## Between sweeps\n\n")
	fmt.Fprintf(&b, "%d finding(s). There is no allowlist: these are held to the same bar as a\n", len(g.Findings))
	b.WriteString("finding inside a realtest's own window, and the sweep exits non-zero for them.\n\n")
	if len(g.Findings) == 0 {
		b.WriteString("No warning, error, malformed record, stray stderr line or *Warnings* line\nbetween the sweeps.\n")
		return b.String()
	}

	fmt.Fprintf(&b, "A finding class repeated inside one workspace is reported ONCE, with its count\n")
	fmt.Fprintf(&b, "and one sample record. Every record is kept verbatim in `%s` beside this\n", fullHarvestName)
	b.WriteString("file; nothing is dropped and no count is hidden.\n\n")

	byWorkspace := make(map[string][]Finding)
	for _, finding := range g.Findings {
		byWorkspace[finding.Workspace] = append(byWorkspace[finding.Workspace], finding)
	}
	keys := make([]string, 0, len(byWorkspace))
	for key := range byWorkspace {
		keys = append(keys, key)
	}
	sort.Strings(keys)
	for _, key := range keys {
		fmt.Fprintf(&b, "### %s\n\n", key)
		for _, class := range CollapseFindings(byWorkspace[key]) {
			renderClass(&b, class)
		}
		b.WriteString("\n")
	}
	return b.String()
}
