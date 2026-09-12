//go:build realtest

package realtest

import (
	"bufio"
	"context"
	"encoding/json"
	"fmt"
	"os"
	"os/exec"
	"path/filepath"
	"regexp"
	"strings"
	"testing"
	"time"
)

// THE WORKSPACE-ACT SUBSTRATE, shared by realtest 5 (create, work, delete) and
// realtest 6 (register, close, re-open).
//
// It lives in its own file rather than in either test because both realtests
// need the same three things and neither owns them: a scratch repository to
// act against, a way to perform a workspace verb, and a cleanup that runs
// whatever the test did. It touches no file another author is working in —
// phases.go, keys.go, harvest.go and the rest are read, never edited — so
// every marker, chord and ceiling this layer needs is defined here under a
// `wsAct` prefix that cannot collide with theirs.
//
// THE SCRATCH REPOSITORY, AND WHY REAL GIT RUNS HERE (lead's standing
// decision, 2026-09-12). A realtest that creates a workspace makes the
// OWNER'S EDITOR create it, and the editor creates a workspace by asking the
// daemon for a git worktree. Real git therefore runs — that is inherent to
// driving the real product, and it is a different thing from the repo's
// no-real-git rule, which governs the unit and integration suites where git
// is mocked entirely. What the standing decision fixes is WHERE it runs: a
// dedicated scratch repository this layer creates under the run directory,
// `git init` plus one commit, and never one of the owner's repositories.
//
// AND EVERYTHING IT CREATES, IT REMOVES. Every act here is paired with a
// cleanup registered through `t.Cleanup`, so a test that fails halfway still
// tears down what it had made by then. What the product can undo, the product
// undoes: a created workspace is NUKED, which is the one verb that deletes
// the worktree, the branch AND the registry record (daemon
// internal/workspace/teardown.go). What the product cannot undo is stated
// rather than hidden — see wsActCleanupRegistered.

// wsActActCeiling bounds how long one workspace verb's effect may take to
// reach the log and the state database before the run reports that it did not
// happen.
//
// IT IS AN OBSERVATION CEILING, NOT A BUDGET, exactly like realtest 1's
// (see the OBSERVATION CEILINGS note in realtest_1_start_the_editor_test.go).
// It has no measured basis yet: a create is a daemon round trip plus a `git
// worktree add` plus a shim spawn plus a roster push, and none of those has
// been measured on this machine through this path. It is generous on purpose,
// because a ceiling that fires turns a measurable slow verb into an
// unmeasurable timeout.
const wsActActCeiling = 180 * time.Second

// wsActChordCeiling bounds how long a pressed chord may take to put its
// command's first minibuffer prompt up. A stall here is a finding about key
// delivery, not something to wait out.
const wsActChordCeiling = 30 * time.Second

// wsActFieldSep is the separator the probe forms join their fields with. It is
// state.go's own separator, so a probe form and a snapshot read cannot disagree
// about where a field ends. A unit separator rather than a pipe or a comma, for
// the reason state.go gives: a workspace name or a directory containing the
// separator would otherwise split into the wrong number of fields and read as a
// malformed answer.
const wsActFieldSep = stateFieldSep

// ---- The chords -------------------------------------------------------
//
// Defined here rather than in keys.go because parallel authors are working in
// that file; the two spellings a Chord carries are documented there.
//
// On this Emacs the Command key is `super` and Option is `meta` (the NS
// defaults, which ~/.config/doom does not override), so a bare letter chord
// needs no modifier and `C-n` is Control+n.

var (
	// wsActEscape is `<escape>`, pressed before a leader sequence so evil is
	// in normal state and `SPC` is the leader rather than a self-inserted
	// space. It is harmless: in normal state it does nothing at all.
	wsActEscape = Chord{
		Emacs:     "<escape>",
		Keycode:   53,
		Modifiers: nil,
		Why:       "returns evil to normal state so SPC is the leader; a no-op when it already is",
	}
	// wsActLeader is `SPC`, Doom's leader.
	wsActLeader = Chord{
		Emacs:     "SPC",
		Keycode:   49,
		Modifiers: nil,
		Why:       "Doom's leader key; on its own it only opens the leader map",
	}
	// wsActTab is the `TAB` of the `SPC TAB` workspace prefix.
	wsActTab = Chord{
		Emacs:     "TAB",
		Keycode:   48,
		Modifiers: nil,
		Why:       "the workspace prefix of the leader map; on its own it only opens that prefix",
	}
	// wsActNewWorkspaceKey is the `n` of `SPC TAB n`, bound to
	// `agent-repl-create-workspace` (lisp/keybindings.el).
	wsActNewWorkspaceKey = Chord{
		Emacs:     "n",
		Keycode:   45,
		Modifiers: nil,
		Why:       "completes `SPC TAB n`, which opens the create command's repository picker",
	}
	// wsActRegisterKey is the `C-n` of `SPC TAB C-n`, bound to
	// `agent-repl-add-project-workspace`.
	wsActRegisterKey = Chord{
		Emacs:     "C-n",
		Keycode:   45,
		Modifiers: []string{"control"},
		Why:       "completes `SPC TAB C-n`, which opens the register command's directory prompt",
	}
	// wsActOpenKey is the `o` of `SPC TAB o`, bound to
	// `agent-repl-open-workspace`.
	wsActOpenKey = Chord{
		Emacs:     "o",
		Keycode:   31,
		Modifiers: nil,
		Why:       "completes `SPC TAB o`, which opens the re-open command's closed-workspace picker",
	}
	// wsActQuit is `C-g`, which aborts whatever minibuffer read is standing.
	wsActQuit = Chord{
		Emacs:     "C-g",
		Keycode:   5,
		Modifiers: []string{"control"},
		Why:       "aborts the minibuffer read the chord under test opened, leaving no half-finished command",
	}
)

// ---- The markers ------------------------------------------------------
//
// Defined here for the same reason the chords are. Each is anchored at the
// start of the record's `message`, and each boundary is spelled explicitly
// rather than with `\b`, because `-` is not a word character (phases.go says
// why that matters).

var (
	// wsActTabOpenRe is `elisp.roster.tab-open: ws=NAME id=ID dir=DIR`,
	// roster.el's account of a tab being drawn.
	wsActTabOpenRe = regexp.MustCompile(`^elisp\.roster\.tab-open: ws=(\S+) id=(\S+)`)
	// wsActTabTeardownRe is `elisp.roster.tab-teardown: ws=NAME`, roster.el's
	// account of a tab being removed.
	wsActTabTeardownRe = regexp.MustCompile(`^elisp\.roster\.tab-teardown: ws=(\S+)`)
	// wsActRegisteredRe is `elisp.commands.add-project-registered dir=DIR
	// id=ID`, the register command's own account of the daemon MINTING the
	// identity for a directory.
	wsActRegisteredRe = regexp.MustCompile(`^elisp\.commands\.add-project-registered dir=(\S+) id=(\S+)`)
	// wsActRegisterRefusedRe is the other arm of the same command: the daemon
	// refused, and no workspace exists.
	wsActRegisterRefusedRe = regexp.MustCompile(`^elisp\.commands\.add-project-not-registered`)
	// wsActCreateStandardRe is `elisp.verbs.create-standard ...`, the create
	// command's own account of having been entered.
	wsActCreateStandardRe = regexp.MustCompile(`^elisp\.verbs\.create-standard(\s|$)`)
	// wsActTeardownRe is `elisp.verbs.teardown ws=NAME op=OP`, the verb layer's
	// account of tearing a tab down after close, kill or nuke succeeded.
	wsActTeardownRe = regexp.MustCompile(`^elisp\.verbs\.teardown ws=(\S+) op=(\S+)`)
)

// wsActHit is one recognized marker.
type wsActHit struct {
	At          time.Time
	Message     string
	WorkspaceID string
}

// wsActScan reads the Emacs sinks for one marker, from `since` onward.
//
// It reads the same two sink kinds ReadPhases reads and through the same
// `resolveReads` re-resolution, for the same reason: the per-workspace markers
// (`tab-open`, `tab-teardown`, `teardown`) land in each workspace's own
// `emacs.log` and never in the global module log, and a reader that opened
// only the global log would report a drawn tab as never drawn.
//
// It exists rather than a new entry in phases.go's marker table because these
// are not startup phases — there is no elapsed-from-spawn number to report for
// them — and because phases.go has parallel authors in it.
func wsActScan(sources []Source, snap Snapshot, since time.Time, re *regexp.Regexp) ([]wsActHit, error) {
	var hits []wsActHit
	for _, src := range sources {
		if !isEmacsPhaseSource(src) {
			continue
		}
		reads, _, err := resolveReads(src, snap)
		if err != nil {
			return nil, err
		}
		for _, r := range reads {
			found, err := wsActScanFile(r.path, r.offset, since, re)
			if err != nil {
				return nil, err
			}
			hits = append(hits, found...)
		}
	}
	return hits, nil
}

// wsActScanFile reads ONE file from `offset` to end for a marker.
//
// A file that does not exist is not an error, for the reason
// readPhaseRecords gives: a workspace whose sink holds no records yet has no
// file behind its link, and an early poll runs before some sinks exist at all.
func wsActScanFile(path string, offset int64, since time.Time, re *regexp.Regexp) ([]wsActHit, error) {
	file, err := os.Open(path)
	if err != nil {
		if os.IsNotExist(err) {
			return nil, nil
		}
		return nil, fmt.Errorf("open the Emacs log %s to read a workspace-act marker: %w", path, err)
	}
	defer file.Close()
	if offset > 0 {
		if _, err := file.Seek(offset, 0); err != nil {
			return nil, fmt.Errorf("seek %s to the snapshot offset %d: %w", path, offset, err)
		}
	}

	var hits []wsActHit
	scanner := bufio.NewScanner(file)
	scanner.Buffer(make([]byte, 0, 1<<20), 1<<24)
	for scanner.Scan() {
		text := scanner.Text()
		if strings.TrimSpace(text) == "" {
			continue
		}
		var rec record
		if err := json.Unmarshal([]byte(text), &rec); err != nil {
			// The harvester reports an unparseable line as a malformed record
			// in its own right; a line this reader cannot parse simply carries
			// no marker.
			continue
		}
		at, parseErr := time.Parse(time.RFC3339Nano, rec.Timestamp)
		if parseErr != nil || at.Before(since) {
			continue
		}
		if !re.MatchString(rec.Message) {
			continue
		}
		hits = append(hits, wsActHit{At: at, Message: rec.Message, WorkspaceID: rec.WorkspaceID})
	}
	if err := scanner.Err(); err != nil {
		return nil, fmt.Errorf("read the Emacs log %s: %w", path, err)
	}
	return hits, nil
}

// wsActHitNaming reports whether any hit's message names `token` in the
// capture position the marker's regexp put it in.
//
// It matches on the whole message rather than only the capture because the two
// spellings of a workspace a log carries — its daemon id and its registered
// name — appear in different markers, and a caller that has one of them should
// not have to know which marker spells which.
func wsActHitNaming(hits []wsActHit, token string) (wsActHit, bool) {
	if token == "" {
		return wsActHit{}, false
	}
	for _, hit := range hits {
		if strings.Contains(hit.Message, token) || (hit.WorkspaceID != "" && strings.HasPrefix(hit.WorkspaceID, token)) {
			return hit, true
		}
	}
	return wsActHit{}, false
}

// ---- The scratch repository -------------------------------------------

// wsActScratchRepo creates a dedicated git repository under `parent` and
// returns its canonical path.
//
// ONE COMMIT, because a repository with no commit has no branch to cut a
// worktree from, and `git worktree add` on an unborn HEAD fails with a message
// about the repository rather than about the product.
//
// The identity and the signing setting travel as `-c` overrides rather than
// as repository configuration: the owner's global git config is the owner's,
// and a realtest that wrote to it would be changing the machine it is
// measuring. A configured commit signer would otherwise stall the commit on a
// passphrase nobody will type.
//
// The path is passed through EvalSymlinks because the daemon normalizes the
// directories it is handed, and on macOS the same directory reached through a
// symlinked parent compares unequal to itself.
func wsActScratchRepo(t *testing.T, parent, name string) string {
	t.Helper()
	dir := filepath.Join(parent, name)
	if err := os.MkdirAll(dir, 0o755); err != nil {
		t.Fatalf("create the scratch repository directory %s: %v", dir, err)
	}

	readme := filepath.Join(dir, "README.md")
	body := "# " + name + "\n\nA realtest scratch repository. It is created under the run directory " +
		"and deleted when the realtest ends; nothing in it is the owner's.\n"
	if err := os.WriteFile(readme, []byte(body), 0o644); err != nil {
		t.Fatalf("write the scratch repository's one file %s: %v", readme, err)
	}

	steps := [][]string{
		{"init", "-b", "master"},
		{"-c", "user.name=agent-repl realtest", "-c", "user.email=realtest@localhost", "add", "README.md"},
		{"-c", "user.name=agent-repl realtest", "-c", "user.email=realtest@localhost",
			"-c", "commit.gpgsign=false", "commit", "-m", "the scratch repository's one commit"},
	}
	for _, args := range steps {
		cmd := exec.Command("git", args...)
		cmd.Dir = dir
		if out, err := cmd.CombinedOutput(); err != nil {
			t.Fatalf("prepare the scratch repository (`git %s` in %s): %v; git said:\n%s",
				strings.Join(args, " "), dir, err, strings.TrimSpace(string(out)))
		}
	}

	canonical, err := filepath.EvalSymlinks(dir)
	if err != nil {
		t.Fatalf("canonicalize the scratch repository %s: %v", dir, err)
	}
	t.Logf("scratch repository: %s (git init, one commit on master)", canonical)
	return canonical
}

// wsActRemoveScratchRepo deletes the scratch repository.
//
// It REPORTS a failure rather than failing the test: it runs from a cleanup,
// where the test's verdict is already decided, and a cleanup that turns a
// green run red says nothing about the product. A residue the owner has to
// remove by hand is a finding they must be told about either way, so the path
// is named.
func wsActRemoveScratchRepo(t *testing.T, dir string) {
	t.Helper()
	if dir == "" {
		return
	}
	if err := os.RemoveAll(dir); err != nil {
		t.Errorf("the scratch repository %s could not be removed and is LEFT ON DISK: %v", dir, err)
		return
	}
	t.Logf("cleanup: removed the scratch repository %s", dir)
}

// ---- Reading the editor's own view ------------------------------------

// wsActRow is one roster row, as Emacs last decoded it.
type wsActRow struct {
	ID     string
	Name   string
	Dir    string
	Closed bool
}

// wsActRosterRows reads every roster row Emacs holds.
//
// READ-ONLY, through the same probe transport realtest 1 reads state with. The
// roster is the daemon's document and Emacs only decodes it, so this is
// reading what the editor was told, which is exactly what an assertion about
// a tab needs to be checked against.
func wsActRosterRows(ctx context.Context, client *Client) ([]wsActRow, error) {
	raw, err := client.Read(ctx, `(mapcar
  (lambda (row)
    (format "%s\037%s\037%s\037%s"
            (or (plist-get (agent-repl-verbs--row-ref row) :id) "")
            (or (agent-repl-verbs--row-name row) "")
            (or (plist-get (agent-repl-verbs--row-ref row) :dir) "")
            (if (agent-repl-verbs--row-closed-p row) "closed" "open")))
  (agent-repl-verbs--all-rows))`)
	if err != nil {
		return nil, err
	}
	var lines []string
	if err := json.Unmarshal(raw, &lines); err != nil {
		return nil, fmt.Errorf("the roster probe answered %s, which is not a list of strings: %w", raw, err)
	}
	rows := make([]wsActRow, 0, len(lines))
	for _, line := range lines {
		fields := strings.Split(line, wsActFieldSep)
		if len(fields) != 4 {
			return nil, fmt.Errorf("a roster row came back with %d fields, not 4: %q", len(fields), line)
		}
		rows = append(rows, wsActRow{ID: fields[0], Name: fields[1], Dir: fields[2], Closed: fields[3] == "closed"})
	}
	return rows, nil
}

// wsActRowByID finds a roster row by its workspace id.
func wsActRowByID(rows []wsActRow, id string) (wsActRow, bool) {
	for _, row := range rows {
		if row.ID == id {
			return row, true
		}
	}
	return wsActRow{}, false
}

// wsActRowByDir finds a roster row by the directory it names.
func wsActRowByDir(rows []wsActRow, dir string) (wsActRow, bool) {
	for _, row := range rows {
		if wsActSameDir(row.Dir, dir) {
			return row, true
		}
	}
	return wsActRow{}, false
}

// wsActTabOrder reads the names of the tabs Emacs has actually drawn, in the
// order the bar draws them.
//
// `agent-repl-roster--tab-order` is the ONE list the bar is drawn from
// (lisp/roster.el sets it at the end of every reconcile), so it is the
// editor's own answer to "which tabs exist" rather than a second reckoning
// this side would have to keep in step.
//
// It is read IN ADDITION to the `tab-open` and `tab-teardown` log markers, not
// instead of them: the markers carry timestamps and the list does not, and the
// list survives a workspace whose sink was deleted along with its worktree —
// which is exactly what a nuke does to the log that would have recorded its
// own teardown.
func wsActTabOrder(ctx context.Context, client *Client) ([]string, error) {
	raw, err := client.Read(ctx, `(if (boundp 'agent-repl-roster--tab-order)
    (append agent-repl-roster--tab-order nil)
  (error "agent-repl: the roster holds no tab order"))`)
	if err != nil {
		return nil, err
	}
	var names []string
	if err := json.Unmarshal(raw, &names); err != nil {
		return nil, fmt.Errorf("the tab-order probe answered %s, which is not a list of strings: %w", raw, err)
	}
	return names, nil
}

// wsActSection is one repository section of the roster: the label the create
// command's picker offers, and the directory behind it.
type wsActSection struct {
	Label string
	Dir   string
}

// wsActRepoSections reads the repository sections the create command picks
// from, so a test can name the scratch repository by the label the picker
// actually offers rather than by one it guessed.
func wsActRepoSections(ctx context.Context, client *Client) ([]wsActSection, error) {
	raw, err := client.Read(ctx, `(mapcar
  (lambda (section)
    (format "%s\037%s"
            (or (agent-repl-verbs--section-label section) "")
            (or (plist-get (agent-repl-verbs--section-ref section) :dir) "")))
  (agent-repl-verbs--repo-sections))`)
	if err != nil {
		return nil, err
	}
	var lines []string
	if err := json.Unmarshal(raw, &lines); err != nil {
		return nil, fmt.Errorf("the repository-sections probe answered %s, which is not a list of strings: %w", raw, err)
	}
	sections := make([]wsActSection, 0, len(lines))
	for _, line := range lines {
		fields := strings.Split(line, wsActFieldSep)
		if len(fields) != 2 {
			return nil, fmt.Errorf("a repository section came back with %d fields, not 2: %q", len(fields), line)
		}
		sections = append(sections, wsActSection{Label: fields[0], Dir: fields[1]})
	}
	return sections, nil
}

// wsActSectionForDir finds the repository section whose directory is `dir`.
func wsActSectionForDir(sections []wsActSection, dir string) (wsActSection, bool) {
	for _, section := range sections {
		if wsActSameDir(section.Dir, dir) {
			return section, true
		}
	}
	return wsActSection{}, false
}

// wsActSameDir compares two directory spellings.
//
// Trailing separators are dropped and both sides are canonicalized where the
// path still exists, because the daemon normalizes what it is handed and
// Emacs adds a trailing separator of its own (`file-name-as-directory` in
// `agent-repl-add-project-workspace`). A comparison that took either spelling
// literally would report a registered directory as unregistered.
func wsActSameDir(a, b string) bool {
	norm := func(p string) string {
		if p == "" {
			return ""
		}
		p = strings.TrimSuffix(p, string(filepath.Separator))
		if resolved, err := filepath.EvalSymlinks(p); err == nil {
			return strings.TrimSuffix(resolved, string(filepath.Separator))
		}
		return p
	}
	na, nb := norm(a), norm(b)
	return na != "" && na == nb
}

// ---- The acts ---------------------------------------------------------
//
// A DELIBERATE, STATED DEVIATION FROM "REAL KEYS" (authorized by the lead,
// 2026-09-12). Every command driven here is the USER-FACING command the plan
// names, entered through `call-interactively` exactly as the keymap enters it,
// with ONLY its minibuffer reads answered from this side. What is not real is
// the typing: `agent-repl-create-workspace` asks four questions in sequence,
// the first of which is a `completing-read` with `require-match`, and driving
// that with synthetic keystrokes through the completion UI would be a test of
// vertico's candidate ordering rather than of the product. The chord itself is
// still proven with real keys, separately and first (wsActProveChord): the
// test presses `SPC TAB n`, asserts the command's OWN first prompt came up —
// which no other command would produce — and aborts it with a real `C-g`
// before running the parameterized act. So the binding is tested by a key and
// the verb is tested by the command, and neither claim rests on the other.
//
// Nothing here reaches past a command into the verb layer. A test that called
// `agent-repl-verb-create` would be testing the wire call and saying nothing
// about the command the owner actually invokes.

// wsActRegisterDirectory registers `dir` through `SPC TAB C-n`'s command.
func wsActRegisterDirectory(ctx context.Context, client *Client, dir string) error {
	form := fmt.Sprintf(`(progn
  (require 'cl-lib)
  (cl-letf (((symbol-function 'read-directory-name) (lambda (&rest _) %q)))
    (call-interactively #'agent-repl-add-project-workspace))
  t)`, dir)
	_, err := client.Read(ctx, form)
	return err
}

// wsActCreateWorkspace creates a workspace through `SPC TAB n`'s command.
//
// The initial prompt and the base ref are both left BLANK, and that is a
// choice about what this realtest is for. A blank prompt is what stops the
// create from submitting a turn: the vendor is forbidden for the whole run, so
// a prompt would exercise the fake SDK and put a conversation under test in a
// realtest whose subject is the workspace's lifecycle. Conversation is
// realtest 9. A blank base ref takes the repository's own default branch
// resolution, which is the path the owner takes.
func wsActCreateWorkspace(ctx context.Context, client *Client, repositoryLabel, name string) error {
	form := fmt.Sprintf(`(progn
  (require 'cl-lib)
  (cl-letf (((symbol-function 'completing-read)
             (lambda (prompt &rest _)
               (if (string-prefix-p "Repository:" prompt)
                   %q
                 (error "realtest: unexpected completing-read prompt %%S" prompt))))
            ((symbol-function 'read-string)
             (lambda (prompt &rest _)
               (cond ((string-prefix-p "Initial prompt:" prompt) "")
                     ((string-prefix-p "Name" prompt) %q)
                     ((string-prefix-p "Base ref" prompt) "")
                     (t (error "realtest: unexpected read-string prompt %%S" prompt))))))
    (call-interactively #'agent-repl-create-workspace))
  t)`, repositoryLabel, name)
	_, err := client.Read(ctx, form)
	return err
}

// wsActCloseWorkspace closes the named workspace through the close command.
//
// The name is passed as the command's own optional argument, which is the
// argument that exists so a caller with a workspace in hand does not go
// through the picker; nothing is stubbed.
func wsActCloseWorkspace(ctx context.Context, client *Client, name string) error {
	_, err := client.Read(ctx, fmt.Sprintf(`(progn (agent-repl-close-workspace %q) t)`, name))
	return err
}

// wsActOpenWorkspace re-opens a closed workspace through `SPC TAB o`'s
// command, choosing `name` from its picker.
func wsActOpenWorkspace(ctx context.Context, client *Client, name string) error {
	form := fmt.Sprintf(`(progn
  (require 'cl-lib)
  (cl-letf (((symbol-function 'completing-read) (lambda (&rest _) %q)))
    (call-interactively #'agent-repl-open-workspace))
  t)`, name)
	_, err := client.Read(ctx, form)
	return err
}

// wsActNukeWorkspace destroys the named workspace through the nuke command.
//
// The confirmation is answered, and nothing else is: `agent-repl-nuke-workspace`
// asks `yes-or-no-p` before it acts because this is the one verb that destroys
// data, and a headless run has nobody to answer it. Every other step is the
// command's own.
func wsActNukeWorkspace(ctx context.Context, client *Client, name string) error {
	form := fmt.Sprintf(`(progn
  (require 'cl-lib)
  (cl-letf (((symbol-function 'yes-or-no-p) (lambda (&rest _) t)))
    (agent-repl-nuke-workspace %q))
  t)`, name)
	_, err := client.Read(ctx, form)
	return err
}

// ---- Proving the chord ------------------------------------------------

// wsActMinibufferPrompt reads the prompt of the minibuffer that is standing,
// or the empty string when none is.
func wsActMinibufferPrompt(ctx context.Context, client *Client) (string, error) {
	return client.ReadString(ctx, `(let ((window (active-minibuffer-window)))
    (if window
        (with-current-buffer (window-buffer window) (or (minibuffer-prompt) ""))
      ""))`)
}

// wsActProveChord presses one leader sequence with REAL KEY EVENTS and proves
// it reached the command the plan says it is bound to.
//
// THE PROOF IS THE COMMAND'S OWN FIRST PROMPT. `SPC TAB n` is
// `agent-repl-create-workspace` and nothing else asks "Repository: " first, so
// that prompt standing in the minibuffer is evidence the keymap resolved the
// chord — evidence that an elisp call performing the same act could never
// produce. `(recent-keys)` is read alongside it, exactly as realtest 1's key
// self-test reads it, because it is Emacs's own account of its INPUT and is
// the only thing that separates "the chord arrived" from "something called the
// command".
//
// It then aborts with a real `C-g`, so the chord under test leaves no
// half-finished command standing and the parameterized act that follows starts
// from a clean minibuffer.
//
// A FAILURE HERE DOES NOT STOP THE RUN. The chord and the verb are separate
// claims: a chord that did not arrive is a finding in its own right, and the
// act that follows is still worth performing and asserting, because a run that
// stopped here would tell the owner nothing about whether creating a workspace
// works at all. Everything the failure needs to be diagnosed — the keys Emacs
// saw, the buffer it saw them in, and evil's state — is reported with it.
func wsActProveChord(ctx context.Context, t *testing.T, client *Client, driver *KeyDriver,
	sequence []Chord, wantPrompt string, manifest *Manifest) bool {
	t.Helper()

	if driver == nil {
		note := fmt.Sprintf("CHORD NOT PRESSED: the key driver is unavailable, so `%s` was never sent; "+
			"the act below was driven through the command instead", wsActSpell(sequence))
		manifest.Notes = append(manifest.Notes, note)
		t.Errorf("%s", note)
		return false
	}

	// The escape is not part of the chord under test: it puts evil in normal
	// state so `SPC` is the leader rather than a self-inserted space, and in
	// normal state it does nothing at all. It is pressed through
	// `wsActClearPendingInput` so the run also SAYS what it cleared: an
	// operator or a prefix standing here would have eaten the first key of
	// the sequence below (inputstate.go says why that is not theoretical).
	wsActClearPendingInput(ctx, t, client, driver,
		fmt.Sprintf("pressing `%s`", wsActSpell(sequence)), manifest)

	for _, chord := range sequence {
		if err := driver.Press(ctx, chord); err != nil {
			note := fmt.Sprintf("CHORD NOT DELIVERED: pressing %s of `%s` (%s) failed: %v",
				chord.Emacs, wsActSpell(sequence), chord.Why, err)
			manifest.Notes = append(manifest.Notes, note)
			t.Errorf("%s", note)
			return false
		}
	}

	var prompt string
	waitUntil(ctx, t, fmt.Sprintf("`%s` to put %q up in the minibuffer", wsActSpell(sequence), wantPrompt),
		wsActChordCeiling,
		func() bool {
			read, err := wsActMinibufferPrompt(ctx, client)
			if err != nil {
				return false
			}
			prompt = read
			return strings.HasPrefix(prompt, wantPrompt)
		})

	keys, keysErr := RecentKeys(ctx, client)
	if keysErr != nil {
		t.Errorf("read Emacs's own (recent-keys) after `%s`: %v", wsActSpell(sequence), keysErr)
	}

	reached := strings.HasPrefix(prompt, wantPrompt)
	if !reached {
		where, _ := client.ReadString(ctx, `(format "buffer=%s evil-state=%s major-mode=%s"
        (buffer-name) (or (bound-and-true-p evil-state) "none") major-mode)`)
		note := fmt.Sprintf("CHORD DID NOT REACH ITS COMMAND: `%s` should have put %q up and the "+
			"minibuffer holds %q instead. Emacs's own (recent-keys) ends with: %s. It was pressed at %s",
			wsActSpell(sequence), wantPrompt, prompt, tail(keys, 120), where)
		manifest.Notes = append(manifest.Notes, note)
		t.Errorf("%s", note)
	} else {
		note := fmt.Sprintf("real key events `%s` reached Emacs's keymap: its command's own prompt %q is up, "+
			"and (recent-keys) ends with %s", wsActSpell(sequence), prompt, tail(keys, 60))
		manifest.Notes = append(manifest.Notes, note)
		t.Logf("%s", note)
	}

	wsActAbortMinibuffer(ctx, t, client, driver, manifest)

	// LEAVE NOTHING BEHIND. A sequence whose leading keys did not land leaves
	// its tail pending — a bare `d` out of `SPC j d` is an evil operator, and
	// an operator does not expire. The next act, the next realtest, and the
	// owner's next keystroke all inherit it, because runs adopt a standing
	// Emacs rather than starting one. inputstate.go carries the whole story.
	wsActClearPendingInput(ctx, t, client, driver,
		fmt.Sprintf("leaving `%s`", wsActSpell(sequence)), manifest)
	return reached
}

// wsActClearPendingInput presses a real `<escape>` and reports what it cleared.
//
// WHY IT READS FIRST AND PRESSES SECOND. The state BEFORE the escape is the
// finding — it says the editor was standing where the next key would be eaten
// — and it is unrecoverable once the escape has landed. Reading it first is
// what makes the difference between reporting an inherited operator and
// silently sweeping one up.
//
// A probe that will not answer is NOT treated as a clean editor: an editor
// that cannot say what it would do with the next key has said nothing, and the
// run reports that rather than pressing on as if it had.
func wsActClearPendingInput(ctx context.Context, t *testing.T, client *Client, driver *KeyDriver,
	why string, manifest *Manifest) {
	t.Helper()

	if driver == nil {
		note := fmt.Sprintf("PENDING INPUT NOT CLEARED: the key driver is unavailable, so no `<escape>` was "+
			"sent before %s and whatever the editor was standing at is still standing", why)
		manifest.Notes = append(manifest.Notes, note)
		t.Errorf("%s", note)
		return
	}

	before, err := wsActReadInputState(ctx, client)
	if err != nil {
		t.Fatalf("read what the editor would do with the next key before %s: %v", why, err)
	}

	if err := driver.Press(ctx, wsActEscape); err != nil {
		note := fmt.Sprintf("ESCAPE COULD NOT BE DELIVERED before %s: %v. The editor was standing at %s and "+
			"nothing has changed that", why, err, before)
		manifest.Notes = append(manifest.Notes, note)
		t.Errorf("%s", note)
		return
	}

	after := before
	if !before.Pending() {
		// Nothing was standing, so there is nothing to wait for: the escape
		// is a no-op in every state that reads the next key as itself, and
		// polling for a change that cannot come would only cost the ceiling.
		t.Logf("%s", wsActInputAlreadyCleanNote(why, before))
		return
	}

	// Poll rather than wait a fixed span: the escape has already been posted
	// and acknowledged, so the only question is when Emacs's command loop
	// dispatched it, and a run must report a failure in seconds rather than
	// spend the whole ceiling on the ordinary case. A probe that errors counts
	// as still pending — an editor that cannot answer is not a clean one.
	deadline := time.Now().Add(wsActInputClearCeiling)
	for {
		if read, readErr := wsActReadInputState(ctx, client); readErr == nil {
			after = read
			if !after.Pending() {
				break
			}
		}
		if ctx.Err() != nil || time.Now().After(deadline) {
			break
		}
		select {
		case <-ctx.Done():
		case <-time.After(wsActInputClearPollInterval):
		}
	}

	if after.Pending() {
		note := wsActInputNotClearedNote(why, before, after)
		manifest.Notes = append(manifest.Notes, note)
		t.Errorf("%s", note)
		return
	}
	note := wsActInputInheritedNote(why, before, after)
	manifest.Notes = append(manifest.Notes, note)
	t.Errorf("%s", note)
}

// wsActReadInputState asks the editor what it would do with the next key.
func wsActReadInputState(ctx context.Context, client *Client) (wsActInputState, error) {
	raw, err := client.ReadString(ctx, wsActPendingInputForm())
	if err != nil {
		return wsActInputState{}, err
	}
	return parseWsActInputState(raw)
}

// wsActAbortMinibuffer clears whatever minibuffer read is standing, so nothing
// a chord opened is left over the acts that follow.
//
// TWO CHANNELS, IN ORDER, AND THE SECOND ONE IS ALWAYS REPORTED. The real `C-g`
// goes first, because dismissing the prompt with the key the owner would press
// is part of what proves the chord. If it has not closed the prompt inside
// wsActChordDismissCeiling, the read channel aborts it instead — and that is a
// FINDING, not a fallback that quietly rescues the run. minibuffer.go carries
// the whole reasoning, including why the previous single-channel version turned
// a failed C-g into a 30s stall per prompt on 2026-09-12.
//
// NEITHER CHANNEL WORKING IS FATAL HERE. The old version reported it and
// carried on, which is how a run came back with "every act after this one ran
// with a minibuffer still up": every assertion downstream was then about an
// editor in a state no owner would be in. A run that cannot clear the
// minibuffer has nothing left to say, so it stops immediately and says why.
func wsActAbortMinibuffer(ctx context.Context, t *testing.T, client *Client, driver *KeyDriver, manifest *Manifest) {
	t.Helper()

	prompt, err := wsActMinibufferPrompt(ctx, client)
	if err != nil {
		t.Fatalf("read whether a minibuffer is standing before dismissing it: %v", err)
	}
	if prompt == "" {
		t.Logf("%s", wsActDismissNote(wsActDismissNothingStanding, "", 0))
		return
	}

	// CHANNEL ONE: the real chord.
	//
	// The editor's account of the press is taken AROUND it, not after it:
	// `(recent-keys)` only answers "did the quit character arrive" against a
	// reading from before the key was posted.
	reported := false
	evidence := wsActQuitEvidence{}
	if driver != nil {
		evidence = wsActReadQuitEvidenceBefore(ctx, client)
		if pressErr := driver.Press(ctx, wsActQuit); pressErr != nil {
			note := fmt.Sprintf("C-g COULD NOT BE DELIVERED while %q was standing: %v. keydriver.swift refuses "+
				"to post a key event to an Emacs it could not make the active application with a focused "+
				"window, because AppKit dispatches a key event only to a key window and drops such a post "+
				"silently; the read channel is used below and this delivery failure is the finding",
				prompt, pressErr)
			manifest.Notes = append(manifest.Notes, note)
			t.Errorf("%s", note)
			reported = true
		} else if wsActMinibufferGone(ctx, client, wsActChordDismissCeiling) {
			note := wsActDismissNote(wsActDismissByChord, prompt, 0)
			manifest.Notes = append(manifest.Notes, note)
			t.Logf("%s", note)
			return
		} else {
			evidence = wsActReadQuitEvidenceAfter(ctx, client, evidence)
		}
	}

	// CHANNEL TWO: the read channel, entered only after the chord had its turn.
	for attempt := 1; attempt <= wsActEvalDismissAttempts; attempt++ {
		if _, evalErr := client.Read(ctx, wsActAbortMinibufferForm()); evalErr != nil {
			t.Logf("eval abort %d of the standing minibuffer %q did not answer: %v", attempt, prompt, evalErr)
		}
		if wsActMinibufferGone(ctx, client, wsActEvalDismissCeiling) {
			stage := wsActDismissByEval
			if driver != nil && !reported && !evidence.Arrived() {
				stage = wsActDismissChordNeverArrived
			}
			note := wsActDismissNote(stage, prompt, attempt)
			if driver != nil && !reported {
				note += ". " + wsActQuitEvidenceNote(evidence)
			}
			manifest.Notes = append(manifest.Notes, note)
			if reported {
				t.Logf("%s", note)
			} else {
				t.Errorf("%s", note)
			}
			return
		}
	}

	note := wsActDismissNote(wsActDismissFailed, prompt, wsActEvalDismissAttempts)
	manifest.Notes = append(manifest.Notes, note)
	t.Fatalf("%s", note)
}

// wsActMinibufferGone polls until no minibuffer is standing, and answers
// whether it went away inside `ceiling`.
//
// Its own tight poll rather than waitUntil: waitUntil polls once a second and
// logs a line when its ceiling expires, and this is called up to four times in
// a row on a path whose whole point is to reach a verdict in seconds. A probe
// that errors counts as "still standing" — an editor that cannot answer is not
// an editor with a clear minibuffer.
func wsActMinibufferGone(ctx context.Context, client *Client, ceiling time.Duration) bool {
	deadline := time.Now().Add(ceiling)
	for {
		prompt, err := wsActMinibufferPrompt(ctx, client)
		if err == nil && prompt == "" {
			return true
		}
		if time.Now().After(deadline) {
			return false
		}
		select {
		case <-ctx.Done():
			return false
		case <-time.After(wsActDismissPollInterval):
		}
	}
}

// wsActReadQuitEvidenceBefore takes the reading a `C-g` press is judged against.
//
// A probe that will not answer is recorded as a probe failure rather than as an
// empty reading: an editor that said nothing must not be able to make a press
// look like it never arrived.
func wsActReadQuitEvidenceBefore(ctx context.Context, client *Client) wsActQuitEvidence {
	keys, err := RecentKeys(ctx, client)
	if err != nil {
		return wsActQuitEvidence{ProbeFailure: fmt.Sprintf("(recent-keys) before the press: %v", err)}
	}
	return wsActQuitEvidence{KeysBefore: keys}
}

// wsActReadQuitEvidenceAfter completes the account once the chord has had its
// chance, and never overwrites a probe failure already recorded.
func wsActReadQuitEvidenceAfter(ctx context.Context, client *Client, before wsActQuitEvidence) wsActQuitEvidence {
	after := before
	keys, err := RecentKeys(ctx, client)
	if err != nil {
		if after.ProbeFailure == "" {
			after.ProbeFailure = fmt.Sprintf("(recent-keys) after the press: %v", err)
		}
	} else {
		after.KeysAfter = keys
	}
	flag, flagErr := client.ReadString(ctx, wsActQuitFlagForm())
	if flagErr != nil {
		if after.ProbeFailure == "" {
			after.ProbeFailure = fmt.Sprintf("quit-flag after the press: %v", flagErr)
		}
	} else {
		after.QuitFlagArmed = flag == "armed"
	}
	return after
}

// wsActSpell renders a chord sequence the way a reader would type it.
func wsActSpell(sequence []Chord) string {
	parts := make([]string, 0, len(sequence))
	for _, chord := range sequence {
		parts = append(parts, chord.Emacs)
	}
	return strings.Join(parts, " ")
}

// wsActKeyDriver builds the key driver, or reports why it could not be built
// and answers nil.
//
// SURFACED, NOT WORKED AROUND, exactly as realtest 1 does it: there is no
// elisp fallback for a key, and the owner rules on the alternative. The nil
// answer is what makes every chord in the run report itself as not pressed
// rather than silently skipped.
//
// Building it is also what brings Emacs forward for the first time, which is
// the FOCUS EDGE the parked pre-creation queue is waiting on. It is called
// before the acts for that reason as well as for the chords: a workspace
// created while Emacs was never shown could not paint a panel, and the
// assertion that it does would be asserting a bug that isn't one (owner
// ruling, 2026-09-11).
func wsActKeyDriver(ctx context.Context, t *testing.T, client *Client, runDir string, manifest *Manifest) *KeyDriver {
	t.Helper()
	pid, err := client.ReadInt(ctx, `(emacs-pid)`)
	if err != nil {
		t.Fatalf("read the Emacs pid for the key driver: %v", err)
	}
	driver := &KeyDriver{Pid: pid, Scratch: runDir}
	if err := driver.Build(ctx); err != nil {
		note := fmt.Sprintf("KEY DRIVER UNAVAILABLE: %v", err)
		manifest.Notes = append(manifest.Notes, note)
		t.Errorf("%s", note)
		t.Logf("the second mechanism, System Events `key code`, is implemented (keys.go) but delivers to the " +
			"FRONTMOST application, so using it would require bringing Emacs forward and leaving it there. " +
			"It was NOT attempted. No elisp fallback is taken for a key.")
		return nil
	}
	manifest.Notes = append(manifest.Notes,
		fmt.Sprintf("key driver: %s; accessibility trust held", driver.Method))
	return driver
}

// ---- The state database, polled ---------------------------------------

// wsActWorkspacesNow reads every workspace the state database holds, open and
// closed together.
//
// Open and closed are folded into one list here, unlike ReadWorkspaces's own
// two, because these tests ask "does this record exist at all" as often as
// they ask "is it open" — a closed workspace that is still re-openable and a
// nuked one that is gone are exactly the distinction realtest 6 turns on.
func wsActWorkspacesNow(ctx context.Context, dbPath string) ([]Workspace, []Workspace, []Workspace, error) {
	open, closed, err := ReadWorkspaces(ctx, dbPath)
	if err != nil {
		return nil, nil, nil, err
	}
	all := make([]Workspace, 0, len(open)+len(closed))
	all = append(all, open...)
	all = append(all, closed...)
	return all, open, closed, nil
}

// wsActWorkspaceByID finds a workspace record by its id.
func wsActWorkspaceByID(list []Workspace, id string) (Workspace, bool) {
	for _, ws := range list {
		if ws.ID == id {
			return ws, true
		}
	}
	return Workspace{}, false
}

// wsActWorkspaceByDir finds a workspace record by the directory it names.
func wsActWorkspaceByDir(list []Workspace, dir string) (Workspace, bool) {
	for _, ws := range list {
		if wsActSameDir(ws.Dir, dir) {
			return ws, true
		}
	}
	return Workspace{}, false
}

// wsActNewSince returns every workspace present now that was absent before,
// by id.
func wsActNewSince(before, now []Workspace) []Workspace {
	known := make(map[string]bool, len(before))
	for _, ws := range before {
		known[ws.ID] = true
	}
	var fresh []Workspace
	for _, ws := range now {
		if !known[ws.ID] {
			fresh = append(fresh, ws)
		}
	}
	return fresh
}

// wsActWaitForDB polls the state database until a predicate holds, and answers
// what it last read.
//
// THE DATABASE IS THE AUTHORITY ON WHETHER A WORKSPACE EXISTS, which is why
// the "no orphan" assertions read it rather than the roster: the roster is a
// view Emacs was pushed, and a stale view claiming a record is gone is exactly
// the defect the assertion exists to catch.
func wsActWaitForDB(ctx context.Context, t *testing.T, what, dbPath string,
	predicate func(all []Workspace) bool) []Workspace {
	t.Helper()
	var last []Workspace
	waitUntil(ctx, t, what, wsActActCeiling, func() bool {
		all, _, _, err := wsActWorkspacesNow(ctx, dbPath)
		if err != nil {
			return false
		}
		last = all
		return predicate(all)
	})
	return last
}

// wsActWaitForTabs polls Emacs's drawn tab order until a predicate holds, and
// answers what it last read.
func wsActWaitForTabs(ctx context.Context, t *testing.T, what string, client *Client,
	predicate func(tabs []string) bool) []string {
	t.Helper()
	var last []string
	waitUntil(ctx, t, what, wsActActCeiling, func() bool {
		tabs, err := wsActTabOrder(ctx, client)
		if err != nil {
			return false
		}
		last = tabs
		return predicate(tabs)
	})
	return last
}

// wsActHasTab reports whether the drawn tab order holds a tab of this name.
func wsActHasTab(tabs []string, name string) bool {
	for _, tab := range tabs {
		if tab == name {
			return true
		}
	}
	return false
}

// ---- Preserving a doomed workspace's logs -----------------------------

// wsActPreserveSinks resolves a workspace's five canonical log links to the
// concrete files behind them, so the harvest can still read them after the
// workspace's worktree is gone.
//
// A NUKE DELETES THE LINK, NOT THE BYTES. The canonical sink in a worktree is
// a symlink into the state root (logging-contract.md), so destroying the
// worktree destroys only the name: the records the workspace wrote during the
// run survive at the target. But the harvester reads THROUGH the link, by
// design and for good reason, so once the link is gone those records are
// unreachable by every source it enumerates — and the remediation bar would
// then be claiming a clean harvest over a workspace whose log it never opened.
//
// So the targets are captured while the workspace still stands and handed back
// as ordinary Sources, keyed to the same logical names, to be appended to the
// enumeration at harvest time. A link that resolves to nothing contributes
// nothing: there is no file to preserve.
func wsActPreserveSinks(ws Workspace) []Source {
	if ws.Dir == "" {
		return nil
	}
	var preserved []Source
	for _, sink := range workspaceSinks {
		link := filepath.Join(ws.Dir, ".claude", "emacs", sink)
		target, err := filepath.EvalSymlinks(link)
		if err != nil {
			continue
		}
		preserved = append(preserved, Source{
			Name:      "workspace." + sink,
			Path:      target,
			Kind:      KindJSONL,
			Workspace: ws.ID,
		})
	}
	return preserved
}

// wsActMergeSources appends sources that the enumeration did not already
// produce, comparing by path so a preserved target that is still reachable
// through its link is not read twice.
func wsActMergeSources(enumerated, extra []Source) []Source {
	seen := make(map[string]bool, len(enumerated))
	for _, src := range enumerated {
		seen[src.Path] = true
		if resolved, err := filepath.EvalSymlinks(src.Path); err == nil {
			seen[resolved] = true
		}
	}
	out := enumerated
	for _, src := range extra {
		if seen[src.Path] {
			continue
		}
		seen[src.Path] = true
		out = append(out, src)
	}
	return out
}

// ---- Cleanup ----------------------------------------------------------

// wsActCleanupCreated destroys a workspace this run created, whatever the test
// did with it.
//
// It is registered the moment the workspace exists and runs from `t.Cleanup`,
// so a test that fails between the create and the delete still leaves the
// owner's registry without the row it added. A workspace the test already
// nuked is gone from the database by then, and the cleanup answers that by
// doing nothing rather than by nuking again.
func wsActCleanupCreated(ctx context.Context, t *testing.T, client *Client, dbPath string, ws Workspace, name string) {
	t.Helper()
	all, _, _, err := wsActWorkspacesNow(ctx, dbPath)
	if err != nil {
		t.Errorf("cleanup: read the state database to decide whether %s (%s) still exists: %v", ws.ID, name, err)
		return
	}
	if _, still := wsActWorkspaceByID(all, ws.ID); !still {
		t.Logf("cleanup: workspace %s (%s) is already gone from the registry", ws.ID, name)
		return
	}
	t.Logf("cleanup: nuking the workspace this run created, %s (%s) at %s", ws.ID, name, ws.Dir)
	if err := wsActNukeWorkspace(ctx, client, name); err != nil {
		t.Errorf("cleanup: nuke the workspace %s (%s) this run created: %v. It is LEFT IN THE REGISTRY "+
			"and its worktree is LEFT ON DISK at %s", ws.ID, name, err, ws.Dir)
		return
	}
	remaining := wsActWaitForDB(ctx, t, fmt.Sprintf("cleanup: the registry to forget %s", name), dbPath,
		func(all []Workspace) bool {
			_, still := wsActWorkspaceByID(all, ws.ID)
			return !still
		})
	if _, still := wsActWorkspaceByID(remaining, ws.ID); still {
		t.Errorf("cleanup: the nuke of %s (%s) was sent but the registry still holds the record, so the "+
			"run LEAVES AN ORPHAN the owner has to remove", ws.ID, name)
	}
}

// wsActForgetCeiling bounds how long a forget issued through the command-file
// ingress may take to remove a workspace's registry row. The ingress polls its
// directory every commandfile.DefaultInterval (250ms, daemon/internal/commandfile
// /api.go); this is generous well past that, the way wsActChordCeiling is
// generous past what a chord's own prompt normally takes.
const wsActForgetCeiling = 30 * time.Second

// wsActCommandFileGlobPrefix and wsActCommandFileGlobSuffix must bracket every
// name this layer writes into the ingress directory, so the daemon's own
// glob (`workspace_commands_*.json`, daemon/internal/stateroot/stateroot.go
// CommandFileGlob) claims it.
const (
	wsActCommandFileGlobPrefix = "workspace_commands_realtest-forget-"
	wsActCommandFileGlobSuffix = ".json"
)

// wsActForgetCommandFile writes a one-entry command file asking the daemon to
// forget `id`, and answers the path it wrote.
//
// THE COMMAND-FILE INGRESS IS THE ONLY DOOR. `Forget` (daemon/internal
// /workspace/forget.go) is on the Verbs interface and mapped in
// daemon/internal/commandfile/ingress.go's `apply`, but no rpc arm exists yet
// for it — that needs a proto addition nobody has made. The command file is a
// real production ingress (the same one `agent-repl workspace-dispatch`
// scripts write through), not a side channel invented for this test, which is
// why driving it is faithful to "forget the way an owner shell script would".
func wsActForgetCommandFile(stateDir, id string) (string, error) {
	dir := filepath.Join(stateDir, "output")
	if err := os.MkdirAll(dir, 0o755); err != nil {
		return "", fmt.Errorf("create the command-file ingress directory %s: %w", dir, err)
	}
	path := filepath.Join(dir, fmt.Sprintf("%s%d%s", wsActCommandFileGlobPrefix, time.Now().UnixNano(), wsActCommandFileGlobSuffix))
	body := fmt.Sprintf(`[{"type":"forget","workspace":%s}]`, jsonString(id))
	if err := os.WriteFile(path, []byte(body), 0o644); err != nil {
		return "", fmt.Errorf("write the command file %s: %w", path, err)
	}
	return path, nil
}

// jsonString renders s as a JSON string literal, so a workspace id can never
// be interpolated into the command file unescaped.
func jsonString(s string) string {
	encoded, err := json.Marshal(s)
	if err != nil {
		// json.Marshal of a string cannot fail; this exists only so a caller
		// never has to check an error that can't occur, per the JSON stdlib's
		// own contract for string values.
		return `""`
	}
	return string(encoded)
}

// wsActForgetReasonRe matches the daemon's own account of a refused verb,
// `intended arm: <Rpc>Error.<Arm>: <reason>` (daemon/internal/workspace
// /refusal.go's Refusal.Error), and the ingress's wrapping of it,
// `a command-file entry was refused`. Either is useful evidence; this reads
// whichever the daemon actually wrote.
var wsActForgetReasonRe = regexp.MustCompile(`ForgetWorkspace|command-file entry was refused`)

// wsActForgetReason best-effort reads the daemon's own global log for the
// latest record, since `since`, that names both the forget arm and the
// workspace id — so a refusal is reported in the daemon's own words rather
// than left as a bare "it did not happen".
//
// BEST-EFFORT, NOT A SECOND VERDICT: nothing here fails the test. A daemon log
// that has rotated out from under a slow forget, or a record this regexp does
// not match, answers the empty string, and the caller states plainly that no
// explanation was found rather than fabricating one.
func wsActForgetReason(stateDir string, since time.Time, id string) string {
	path := filepath.Join(stateDir, "logs", "daemon.run.log")
	file, err := os.Open(path)
	if err != nil {
		return ""
	}
	defer file.Close()

	var latest string
	var latestAt time.Time
	scanner := bufio.NewScanner(file)
	scanner.Buffer(make([]byte, 0, 1<<20), 1<<24)
	for scanner.Scan() {
		text := scanner.Text()
		if strings.TrimSpace(text) == "" {
			continue
		}
		var rec record
		if err := json.Unmarshal([]byte(text), &rec); err != nil {
			continue
		}
		at, parseErr := time.Parse(time.RFC3339Nano, rec.Timestamp)
		if parseErr != nil || at.Before(since) {
			continue
		}
		if !wsActForgetReasonRe.MatchString(rec.Message) {
			continue
		}
		if !strings.Contains(rec.Message, id) && rec.WorkspaceID != id {
			continue
		}
		if latest == "" || at.After(latestAt) {
			latest, latestAt = rec.Message, at
		}
	}
	return latest
}

// wsActWaitForForgetGone polls the registry, on wsActForgetCeiling rather than
// wsActActCeiling, until id is gone, and answers whether it left within that
// wait. It is its own function rather than a wsActWaitForDB call because that
// helper's ceiling is the much longer wsActActCeiling, sized for a create or a
// nuke to reach the daemon and the log; a forget round-trips through a 250ms
// poll (commandfile.DefaultInterval) and a ceiling thirty times that already
// generously covers a slow sweep.
func wsActWaitForForgetGone(ctx context.Context, t *testing.T, dbPath, name, id string) bool {
	t.Helper()
	var gone bool
	waitUntil(ctx, t, fmt.Sprintf("cleanup: the registry to forget %s", name), wsActForgetCeiling, func() bool {
		all, _, _, err := wsActWorkspacesNow(ctx, dbPath)
		if err != nil {
			return false
		}
		_, still := wsActWorkspaceByID(all, id)
		gone = !still
		return gone
	})
	return gone
}

// wsActForgetDeps abstracts the collaborators the forget step needs, so the
// decision logic below — issue for the right id, wait, report a refusal
// loudly without failing the test — is testable without a live daemon, a real
// state database, or a real Emacs.
type wsActForgetDeps struct {
	// IssueForget writes the command-file entry for id and answers the path
	// written. An error here is a harness failure (the ingress directory
	// could not even be written to), distinct from the daemon later refusing
	// the request it did receive.
	IssueForget func(id string) (string, error)
	// WaitGone polls the registry until id is no longer in it, up to
	// wsActForgetCeiling, and answers whether it is gone.
	WaitGone func(id string) bool
	// Reason best-effort explains why a forget did not take. May be nil.
	Reason func(id string) string
}

// wsActForget drives one forget through deps and reports the outcome on t.
//
// IT NEVER FAILS THE TEST. Cleanup runs after the test's own verdict, and a
// forget the daemon refuses or a request the harness could not even issue is a
// residue to report, not a defect this cleanup pass gets to fail the run over
// — the run has already been judged. Losing that residue silently would be
// worse than reporting it, so every path here ends in a t.Logf that says
// plainly what is left and, where it can be learned, why.
func wsActForget(t *testing.T, deps wsActForgetDeps, ws Workspace, name string) {
	t.Helper()

	path, err := deps.IssueForget(ws.ID)
	if err != nil {
		t.Logf("REGISTRY RESIDUE, for the owner to rule on: forgetting %s (%s) at %s could not even be "+
			"requested through the command-file ingress: %v. The row and the repository record minted with it "+
			"are LEFT IN THE REGISTRY.", ws.ID, name, ws.Dir, err)
		return
	}

	if deps.WaitGone(ws.ID) {
		t.Logf("cleanup: forgot the workspace this run registered, %s (%s); the registry holds no trace of it "+
			"or of the repository record minted with it", ws.ID, name)
		return
	}

	reason := ""
	if deps.Reason != nil {
		reason = deps.Reason(ws.ID)
	}
	if reason == "" {
		reason = "no explanation for the refusal was found in the daemon's own log within the wait"
	}
	t.Logf("REGISTRY RESIDUE, for the owner to rule on: this run leaves ONE closed workspace row (%s, %s) "+
		"and the repository row minted with it, both naming %s. A forget was requested through the command file "+
		"%s but the daemon did not remove the record: %s.", ws.ID, name, ws.Dir, path, reason)
}

// wsActCleanupRegistered closes a workspace this run registered, then
// FORGETS it through the command-file ingress, so the run leaves the registry
// exactly as it found it whenever the daemon accepts the forget.
//
// CLOSE, THEN FORGET, IN THAT ORDER: `Forget` refuses an open workspace
// (daemon/internal/workspace/forget.go), so the close this cleanup already
// performed — and its own wait for the row to read back closed — must land
// before the forget is even requested.
//
// FORGET ONLY WHAT THIS RUN REGISTERED. The id this cleanup forgets is the one
// the register act minted and this test has been asserting against the whole
// time; before issuing the forget, the row is re-read by that same id and its
// directory is checked against the directory this run owns. A mismatch here
// would mean the identity this test has been asserting on all along was never
// what it appeared to be, which is worth failing loudly over rather than
// forgetting a row on someone else's say-so.
func wsActCleanupRegistered(ctx context.Context, t *testing.T, client *Client, dbPath string, ws Workspace, name string) {
	t.Helper()
	all, _, closed, err := wsActWorkspacesNow(ctx, dbPath)
	if err != nil {
		t.Errorf("cleanup: read the state database to decide whether %s (%s) is still open: %v", ws.ID, name, err)
		return
	}
	if _, gone := wsActWorkspaceByID(all, ws.ID); !gone {
		t.Logf("cleanup: the registered workspace %s (%s) is already gone from the registry", ws.ID, name)
		return
	}
	if _, alreadyClosed := wsActWorkspaceByID(closed, ws.ID); !alreadyClosed {
		t.Logf("cleanup: closing the workspace this run registered, %s (%s) at %s", ws.ID, name, ws.Dir)
		if err := wsActCloseWorkspace(ctx, client, name); err != nil {
			t.Errorf("cleanup: close the registered workspace %s (%s): %v. Its tab is LEFT STANDING",
				ws.ID, name, err)
			return
		}
		wsActWaitForDB(ctx, t, fmt.Sprintf("cleanup: the registry to mark %s closed", name), dbPath,
			func(all []Workspace) bool {
				_, _, closedNow, readErr := wsActWorkspacesNow(ctx, dbPath)
				if readErr != nil {
					return false
				}
				_, isClosed := wsActWorkspaceByID(closedNow, ws.ID)
				return isClosed
			})
	}

	// Re-read by id and verify ownership before forgetting anything.
	allNow, _, closedNow, err := wsActWorkspacesNow(ctx, dbPath)
	if err != nil {
		t.Errorf("cleanup: re-read the state database before forgetting %s (%s): %v. The row is LEFT "+
			"IN THE REGISTRY.", ws.ID, name, err)
		return
	}
	current, stillThere := wsActWorkspaceByID(allNow, ws.ID)
	if !stillThere {
		t.Logf("cleanup: %s (%s) left the registry on its own before this cleanup could forget it", ws.ID, name)
		return
	}
	if !wsActSameDir(current.Dir, ws.Dir) {
		t.Errorf("cleanup: refusing to forget %s (%s): the registry now names %s for this id, not the "+
			"directory this run registered (%s). Forgetting it would risk deleting a record this run never "+
			"created; the row is LEFT IN THE REGISTRY for the owner to inspect.",
			ws.ID, name, current.Dir, ws.Dir)
		return
	}
	if _, isClosed := wsActWorkspaceByID(closedNow, ws.ID); !isClosed {
		t.Errorf("cleanup: refusing to forget %s (%s): the registry still shows it open, and forget refuses "+
			"an open workspace. The row is LEFT IN THE REGISTRY.", ws.ID, name)
		return
	}

	stateDir := filepath.Dir(dbPath)
	requestedAt := time.Now()
	deps := wsActForgetDeps{
		IssueForget: func(id string) (string, error) { return wsActForgetCommandFile(stateDir, id) },
		WaitGone: func(id string) bool {
			return wsActWaitForForgetGone(ctx, t, dbPath, name, id)
		},
		Reason: func(id string) string { return wsActForgetReason(stateDir, requestedAt, id) },
	}
	wsActForget(t, deps, ws, name)
}

// ---- Unit tests for the forget helpers ---------------------------------
//
// These start no editor and no daemon: wsActForget takes its collaborators as
// wsActForgetDeps, which is what lets its decision logic — issue for the right
// id, wait, report a refusal without failing the calling test — be exercised
// directly. TestWsActCleanupRegisteredForgetsNothingWhenAlreadyGone goes one
// layer up, through the real database-reading fixture state_test.go already
// built (crashedWALFixture, snapshotsUnder), because that early-return
// happens before wsActCleanupRegistered ever touches its `client` argument.

// TestWsActForgetCommandFileNamesTheRequestedID is the edge case "the forget
// is issued for the right id": the command file wsActForgetCommandFile writes
// must decode as exactly one entry, of type "forget", naming the id it was
// asked to forget — never a different one and never more than one.
func TestWsActForgetCommandFileNamesTheRequestedID(t *testing.T) {
	// Arrange.
	stateDir := t.TempDir()

	// Act.
	path, err := wsActForgetCommandFile(stateDir, "ws-123")

	// Assert.
	if err != nil {
		t.Fatalf("write the forget command file: %v", err)
	}
	if !strings.HasPrefix(filepath.Base(path), "workspace_commands_") || filepath.Ext(path) != ".json" {
		t.Fatalf("the command file %s does not match the daemon's own glob workspace_commands_*.json "+
			"(daemon/internal/stateroot/stateroot.go CommandFileGlob)", path)
	}
	data, err := os.ReadFile(path)
	if err != nil {
		t.Fatalf("read the command file back: %v", err)
	}
	var entries []struct {
		Type      string `json:"type"`
		Workspace string `json:"workspace"`
	}
	if err := json.Unmarshal(data, &entries); err != nil {
		t.Fatalf("the command file %s did not decode as JSON: %v", path, err)
	}
	if len(entries) != 1 {
		t.Fatalf("expected exactly one entry, got %d: %+v", len(entries), entries)
	}
	if entries[0].Type != "forget" || entries[0].Workspace != "ws-123" {
		t.Fatalf("expected a forget entry naming ws-123, got %+v", entries[0])
	}
}

// TestWsActForgetRefusalIsReportedNotFailed is the edge case "a refused forget
// is reported and does not fail the test": wsActForget must state the residue
// loudly (t.Logf) rather than fail the calling test (t.Errorf/t.Fatalf), since
// the cleanup it runs from executes after the realtest's own verdict.
func TestWsActForgetRefusalIsReportedNotFailed(t *testing.T) {
	// Arrange.
	ws := Workspace{ID: "ws-refused", Dir: "/repos/refused"}
	deps := wsActForgetDeps{
		IssueForget: func(id string) (string, error) {
			return "/tmp/workspace_commands_refused.json", nil
		},
		WaitGone: func(id string) bool { return false },
		Reason:   func(id string) string { return "workspace has forks naming it as parent" },
	}

	// Act.
	ok := t.Run("forget", func(t *testing.T) {
		wsActForget(t, deps, ws, "refused-name")
	})

	// Assert.
	if !ok {
		t.Fatalf("a refused forget must be reported without failing the calling test, but the subtest failed")
	}
}

// TestWsActForgetIssueFailureIsReportedNotFailed is the same non-failure
// contract for the other way a forget can come up short: the harness could
// not even write the command file (the ingress directory is unwritable, say).
// That is a harness failure rather than a daemon refusal, and it still must
// not fail the calling test for the same reason: cleanup runs after the
// verdict.
func TestWsActForgetIssueFailureIsReportedNotFailed(t *testing.T) {
	// Arrange.
	ws := Workspace{ID: "ws-unwritable", Dir: "/repos/unwritable"}
	deps := wsActForgetDeps{
		IssueForget: func(id string) (string, error) {
			return "", fmt.Errorf("create the command-file ingress directory: permission denied")
		},
		WaitGone: func(id string) bool {
			t.Fatalf("WaitGone must not be consulted when the forget could not even be issued")
			return false
		},
	}

	// Act.
	ok := t.Run("forget", func(t *testing.T) {
		wsActForget(t, deps, ws, "unwritable-name")
	})

	// Assert.
	if !ok {
		t.Fatalf("an issue failure must be reported without failing the calling test, but the subtest failed")
	}
}

// TestWsActCleanupRegisteredForgetsNothingWhenAlreadyGone is the edge case "a
// run that registered nothing forgets nothing": a workspace id this run never
// put in the registry (or that some other actor already removed) must never
// reach a forget at all. `client` is passed as nil because this early-return
// path is reached before wsActCleanupRegistered ever uses it.
func TestWsActCleanupRegisteredForgetsNothingWhenAlreadyGone(t *testing.T) {
	// Arrange.
	snapshotsUnder(t)
	dbPath := crashedWALFixture(t)
	stateDir := filepath.Dir(dbPath)
	ws := Workspace{ID: "never-registered-by-this-run", Dir: filepath.Join(stateDir, "scratch")}

	// Act.
	wsActCleanupRegistered(context.Background(), t, nil, dbPath, ws, "ghost")

	// Assert: no command file was ever written to the ingress directory.
	outputDir := filepath.Join(stateDir, "output")
	entries, err := os.ReadDir(outputDir)
	if err != nil && !os.IsNotExist(err) {
		t.Fatalf("list the ingress directory %s: %v", outputDir, err)
	}
	if len(entries) != 0 {
		t.Fatalf("expected no command file for a workspace this run never registered, found %v", entries)
	}
}
