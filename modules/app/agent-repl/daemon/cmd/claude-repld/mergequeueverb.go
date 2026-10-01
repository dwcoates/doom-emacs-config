package main

import (
	"context"
	"errors"
	"flag"
	"fmt"
	"io"
	"os"
	"path/filepath"
	"strings"
	"time"

	"connectrpc.com/connect"

	agentreplv1 "agentrepl/proto/agentrepl/v1"
	"agentrepl/proto/agentrepl/v1/agentreplv1connect"
	frontendv1 "agentrepl/proto/frontend/v1"
	workspacev1 "agentrepl/proto/workspace/v1"

	"claude-repld/internal/commandfile"
	"claude-repld/internal/envc"
	"claude-repld/internal/stateroot"
)

// THE `merge-queue` VERB:
//
//	claude-repld merge-queue -own [-keep-open] [-wait] [-state-dir DIR]
//	claude-repld merge-queue -dir WORKTREE [-wait] [-state-dir DIR]
//	claude-repld merge-queue -branch BRANCH [-wait] [-state-dir DIR]
//	claude-repld merge-queue -pr-merged [-wait] [-state-dir DIR]
//
// A MERGE RUNS IN THE WORKSPACE THAT ASKED FOR IT, and the verb asks FROM THE
// CALLING WORKSPACE: the workspace whose worktree contains the caller's working
// directory, found on the roster the daemon streams. It ENQUEUES through the
// daemon's command-file ingress -- the same agent-requested path
// (merge.RequestedByAgent) the merge-queue skill's agents use -- naming what
// the calling workspace merges:
//
//   - `-own` its own branch, closed once it lands unless `-keep-open`;
//   - `-dir` another workspace's branch, by that workspace's worktree, which
//     closes once it lands;
//   - `-branch` a branch that is no workspace (a subagent's);
//   - `-pr-merged` its own branch, already merged upstream.
//
// No workspace is ever created for a merge. The verb decides nothing and never
// touches git.
//
// AN AGENT'S MERGE IS PUT IN LINE ONLY ONCE ITS TURN ENDS, and nothing about it
// is reported before then. So without -wait the verb returns as soon as the
// daemon has APPLIED the command (it retires the file to applied/), or REFUSED
// it (quarantine/, exitMergeRefused). With -wait it returns at the merge's
// outcome -- LANDED (exit 0) or FAILED with its area (exitMergeFailed) -- and
// -wait is REFUSED while the calling workspace has a turn in flight: that turn
// is the one that would wait, and the merge cannot start until it ends.

// mergeQueueVerb is the verb's name on the command line.
const mergeQueueVerb = "merge-queue"

// The verb's outcome exits beyond exitSuccess (landed, or requested without
// -wait) and exitFailure (the verb could not do its job).
const (
	// exitMergeFailed is a merge that failed.
	exitMergeFailed = 5
	// exitMergeRefused is a command the daemon refused and quarantined.
	exitMergeRefused = 6
)

// The roster status arms the verb reads, spelled as the proto's oneof field
// names (frontend.v1.RosterRow.status).
const (
	statusAbsent     = "absent"
	statusQueued     = "merge_queued"
	statusMerging    = "merging"
	statusMergeFail  = "merge_failed"
	statusMergedDone = "merged"
)

// turnArms are the roster arms of a workspace with a turn in flight.
var turnArms = map[string]bool{"submitting": true, "thinking": true, "clearing": true, "compacting": true, "permission": true}

// worktreeLinePrefix leads the one output line naming the requesting
// workspace's worktree, which the merge-queue skill's driver reads to fetch a
// failure's reason from that workspace's log.
const worktreeLinePrefix = "merge-queue: worktree: "

// errStreamEnding is the daemon standing the roster stream down in a planned
// exit (a handover or a restart); the verb reattaches to whichever daemon then
// serves.
var errStreamEnding = errors.New("the daemon stood the roster stream down")

// mergeQueueDaemon is the slice of the serving daemon the verb reads.
type mergeQueueDaemon interface {
	// WatchRoster calls yield with every roster frame until ctx ends or yield
	// fails, and answers errStreamEnding on a planned stand-down.
	WatchRoster(ctx context.Context, yield func(*frontendv1.WorkspaceRoster) error) error
	// Footer answers a workspace's footer as it stands now.
	Footer(ctx context.Context, ref *workspacev1.WorkspaceRef) (*frontendv1.FooterView, error)
}

// mergeQueueDialer builds the daemon reader for a loopback address.
type mergeQueueDialer func(address string) mergeQueueDaemon

// mergeQueueEnv is what the verb reads from its process rather than from its
// arguments.
type mergeQueueEnv struct {
	// getwd answers the caller's working directory.
	getwd func() (string, error)
	// poll is how often the verb looks for its command file in quarantine. It
	// is the ingress's own cadence: a refusal cannot be seen sooner.
	poll time.Duration
}

// mergeQueueSource is what one invocation merges: the merge entry's source
// fields, and its label on the verb's output.
type mergeQueueSource struct {
	label string
	entry commandfile.Entry
}

// connectMergeQueueDaemon is the production reader over the daemon's rpcs.
type connectMergeQueueDaemon struct {
	client agentreplv1connect.AgentReplClient
}

// dialMergeQueueDaemon is the production dialer.
func dialMergeQueueDaemon(address string) mergeQueueDaemon {
	return connectMergeQueueDaemon{client: newDaemonClient(address)}
}

// WatchRoster streams WatchWorkspaceRoster.
func (d connectMergeQueueDaemon) WatchRoster(ctx context.Context, yield func(*frontendv1.WorkspaceRoster) error) error {
	stream, err := d.client.WatchWorkspaceRoster(ctx, connect.NewRequest(&agentreplv1.WatchWorkspaceRosterRequest{}))
	if err != nil {
		return err
	}
	defer stream.Close()
	for stream.Receive() {
		switch push := stream.Msg().GetPush().(type) {
		case *agentreplv1.WatchWorkspaceRosterResponse_Roster:
			if err := yield(push.Roster); err != nil {
				return err
			}
		case *agentreplv1.WatchWorkspaceRosterResponse_Ending:
			return errStreamEnding
		default:
			return errors.New("the daemon pushed a roster frame with no arm")
		}
	}
	if err := stream.Err(); err != nil {
		return err
	}
	return errors.New("the daemon closed the roster stream without a planned ending")
}

// Footer reads WatchFooter's first frame, which is the footer whole.
func (d connectMergeQueueDaemon) Footer(ctx context.Context, ref *workspacev1.WorkspaceRef) (*frontendv1.FooterView, error) {
	ctx, cancel := context.WithCancel(ctx)
	defer cancel()
	stream, err := d.client.WatchFooter(ctx, connect.NewRequest(&agentreplv1.WatchFooterRequest{Workspace: ref}))
	if err != nil {
		return nil, err
	}
	defer stream.Close()
	if stream.Receive() {
		return stream.Msg().GetFooter(), nil
	}
	if err := stream.Err(); err != nil {
		return nil, err
	}
	return nil, errors.New("the footer stream ended before its first frame")
}

// runMergeQueueVerb runs the verb and answers the process's exit status.
func runMergeQueueVerb(ctx context.Context, args []string, dial mergeQueueDialer, env mergeQueueEnv, out, errOut io.Writer) int {
	fail := func(format string, a ...any) int {
		say(errOut, "claude-repld merge-queue: "+format+"\n", a...)
		return exitFailure
	}
	fs := flag.NewFlagSet(mergeQueueVerb, flag.ContinueOnError)
	fs.SetOutput(errOut)
	own := fs.Bool("own", false, "merge the calling workspace's own branch")
	keepOpen := fs.Bool("keep-open", false, "with -own: keep the calling workspace open once its branch lands")
	dir := fs.String("dir", "", "merge another workspace's branch, by its worktree; it closes once it lands")
	branch := fs.String("branch", "", "merge a branch that is no workspace (a subagent's)")
	prMerged := fs.Bool("pr-merged", false, "the calling workspace's branch already merged upstream: update the default branch and close it")
	wait := fs.Bool("wait", false, "wait for the outcome: landed (0) or failed (5)")
	stateDir := fs.String("state-dir", "", "state root, overriding $AGENT_REPL_STATE_DIR")
	if err := fs.Parse(args); err != nil {
		return exitFailure
	}
	if fs.NArg() != 0 {
		return fail("unexpected arguments: %s", strings.Join(fs.Args(), " "))
	}
	source, err := resolveMergeQueueSource(*own, *keepOpen, *dir, *branch, *prMerged)
	if err != nil {
		return fail("%v", err)
	}
	cwd, err := env.getwd()
	if err != nil {
		return fail("read the working directory: %v", err)
	}
	layout, err := stateroot.Root(*stateDir, envc.Load().WithStateDir(*stateDir).StateDir())
	if err != nil {
		return fail("resolve the state root: %v", err)
	}
	w := &mergeWatch{source: source, cwd: canonicalDir(cwd), wait: *wait, dial: dial, layout: layout, env: env, out: out, errOut: errOut}
	return w.run(ctx)
}

// resolveMergeQueueSource turns the flags into what the calling workspace
// merges. Exactly one source is named.
func resolveMergeQueueSource(own, keepOpen bool, dir, branch string, prMerged bool) (mergeQueueSource, error) {
	named := 0
	for _, set := range []bool{own, dir != "", branch != "", prMerged} {
		if set {
			named++
		}
	}
	switch {
	case named == 0:
		return mergeQueueSource{}, errors.New("name the merge: -own, -dir WORKTREE, -branch BRANCH or -pr-merged")
	case named > 1:
		return mergeQueueSource{}, errors.New("-own, -dir, -branch and -pr-merged each name a different merge; give one")
	case keepOpen && !own:
		return mergeQueueSource{}, errors.New("-keep-open goes with -own: only the calling workspace's own branch can keep it open")
	}
	switch {
	case dir != "":
		abs, err := filepath.Abs(dir)
		if err != nil {
			return mergeQueueSource{}, fmt.Errorf("resolve -dir %q: %w", dir, err)
		}
		if info, err := os.Stat(abs); err != nil || !info.IsDir() {
			return mergeQueueSource{}, fmt.Errorf("-dir %q is not a worktree directory on disk", abs)
		}
		return mergeQueueSource{label: filepath.Base(abs), entry: commandfile.Entry{SourceDir: abs}}, nil
	case branch != "":
		return mergeQueueSource{label: branch, entry: commandfile.Entry{Branch: branch}}, nil
	case prMerged:
		return mergeQueueSource{label: "its own branch, already merged upstream", entry: commandfile.Entry{PRWasMerged: true}}, nil
	}
	return mergeQueueSource{label: "its own branch", entry: commandfile.Entry{KeepOpen: keepOpen}}, nil
}

// mergeWatch is one invocation's watch over the roster.
type mergeWatch struct {
	source mergeQueueSource
	// cwd is the caller's canonical working directory, which names the
	// requesting workspace.
	cwd string
	// requester is the requesting workspace's canonical worktree, its name
	// and its id, read off the first roster frame.
	requester     string
	requesterName string
	wait          bool
	dial          mergeQueueDialer
	layout        stateroot.Layout
	env           mergeQueueEnv
	out           io.Writer
	errOut        io.Writer

	// daemon is the reader for the daemon serving now.
	daemon mergeQueueDaemon
	// file is the command file's base name once written.
	file string
	// baseline is the row's status before the command was written, and
	// baselineMergedAt its merged instant; a concluded state that is still the
	// baseline's is an EARLIER merge's, not this one's.
	baseline         string
	baselineMergedAt int64
	// seenLive is whether this merge has been seen in flight.
	seenLive bool
	// applied is whether the daemon applied the command.
	applied bool
	// last is the last status printed.
	last string
}

// rosterEvent is one thing the watch goroutine reports.
type rosterEvent struct {
	roster *frontendv1.WorkspaceRoster
	err    error
}

// run takes the baseline, writes the command, and follows the row to the
// outcome, reattaching across a planned daemon stand-down.
func (w *mergeWatch) run(ctx context.Context) int {
	ctx, cancel := context.WithCancel(ctx)
	defer cancel()
	events, err := w.attach(ctx)
	if err != nil {
		return w.fail("%v", err)
	}
	ticker := time.NewTicker(w.env.poll)
	defer ticker.Stop()
	for {
		select {
		case <-ctx.Done():
			return w.fail("interrupted before the merge's outcome: %v", ctx.Err())
		case <-ticker.C:
			if code, done := w.checkCommand(); done {
				return code
			}
		case ev := <-events:
			if ev.err != nil {
				if !errors.Is(ev.err, errStreamEnding) {
					return w.fail("the roster stream failed: %v", ev.err)
				}
				say(w.out, "merge-queue: the daemon stood down; reattaching\n")
				if events, err = w.attach(ctx); err != nil {
					return w.fail("%v", err)
				}
				continue
			}
			if code, done := w.observe(ctx, ev.roster); done {
				return code
			}
		}
	}
}

// attach dials the daemon serving now and starts its roster stream. The first
// attach takes the baseline from the first frame and only THEN writes the
// command, so the baseline is the row as it stood before this merge existed.
func (w *mergeWatch) attach(ctx context.Context) (<-chan rosterEvent, error) {
	address, err := servingAddress(w.layout)
	if err != nil {
		return nil, err
	}
	w.daemon = w.dial(address)
	events := make(chan rosterEvent)
	go func() {
		err := w.daemon.WatchRoster(ctx, func(r *frontendv1.WorkspaceRoster) error {
			select {
			case events <- rosterEvent{roster: r}:
				return nil
			case <-ctx.Done():
				return ctx.Err()
			}
		})
		select {
		case events <- rosterEvent{err: err}:
		case <-ctx.Done():
		}
	}()
	if w.file != "" {
		return events, nil
	}
	var first rosterEvent
	select {
	case first = <-events:
	case <-ctx.Done():
		return nil, ctx.Err()
	}
	if first.err != nil {
		return nil, fmt.Errorf("read the roster before enqueueing: %w", first.err)
	}
	row := callingRow(first.roster, w.cwd)
	if row == nil {
		return nil, fmt.Errorf("the caller's directory %s is in no workspace's worktree; a merge runs in the workspace that asks for it", w.cwd)
	}
	w.requester = canonicalDir(row.GetWorkspace().GetWorkspace().GetDir())
	w.requesterName = filepath.Base(w.requester)
	w.baseline = rowStatus(row)
	w.baselineMergedAt = row.GetWhen().GetMerged().GetAtMs()
	w.last = w.baseline
	if w.wait && turnArms[w.baseline] {
		return nil, fmt.Errorf("refusing -wait from %s while its turn is in flight (%s): a merge it asks for starts only when that turn ends, "+
			"and this turn is the one that would wait. Enqueue without -wait and end the turn; the merge reports into this session", w.requester, w.baseline)
	}
	entry := w.source.entry
	entry.Type = commandfile.TypeMerge
	entry.ProjectDir = row.GetWorkspace().GetWorkspace().GetDir()
	entry.Workspace = row.GetWorkspace().GetWorkspace().GetId()
	name, err := commandfile.Write(w.layout.OutputDir(), []commandfile.Entry{entry})
	if err != nil {
		return nil, err
	}
	w.file = name
	say(w.out, "merge-queue: %s asked to merge %s (command file %s)\n", w.requesterName, w.source.label, name)
	say(w.out, "%s%s\n", worktreeLinePrefix, w.requester)
	return events, nil
}

// observe reads one roster frame, and answers the exit status once the merge
// reached a point the verb reports.
func (w *mergeWatch) observe(ctx context.Context, roster *frontendv1.WorkspaceRoster) (int, bool) {
	if code, done := w.checkCommand(); done {
		return code, true
	}
	row := findRosterRow(roster, w.requester)
	status := rowStatus(row)
	if status != w.last {
		say(w.out, "merge-queue: %s: %s\n", w.requesterName, status)
		w.last = status
	}
	switch status {
	case statusQueued, statusMerging:
		w.seenLive = true
		return 0, false
	case statusMergedDone, statusMergeFail:
		if !w.ours(status, row) {
			return 0, false
		}
		return w.conclude(ctx, status, row), true
	default:
		return 0, false
	}
}

// ours reports whether a concluded status belongs to THIS merge rather than
// standing over from an earlier one.
func (w *mergeWatch) ours(status string, row *frontendv1.RosterRow) bool {
	switch {
	case w.seenLive, status != w.baseline:
		return true
	case status == statusMergedDone:
		return row.GetWhen().GetMerged().GetAtMs() > w.baselineMergedAt
	default:
		return false
	}
}

// conclude reports the merge's outcome.
func (w *mergeWatch) conclude(ctx context.Context, status string, row *frontendv1.RosterRow) int {
	if status == statusMergedDone {
		say(w.out, "merge-queue: LANDED: %s merged %s\n", w.requesterName, w.source.label)
		return exitSuccess
	}
	area := "unread"
	footer, err := w.daemon.Footer(ctx, row.GetWorkspace().GetWorkspace())
	if err != nil {
		say(w.errOut, "claude-repld merge-queue: %s's merge failed, and its footer could not be read for the area: %v\n", w.requesterName, err)
	} else {
		area = failedArea(footer.GetStrip().GetStatus().GetMergeFailed())
	}
	say(w.out, "merge-queue: FAILED (%s): %s's merge of %s did not land. The account is the daemon's daemon.merge.abort record:\n", area, w.requesterName, w.source.label)
	say(w.out, "  modules/app/agent-repl/bin/logs.sh --workspace %s --since 6h --json | jq -r 'select(.operation == \"daemon.merge.abort\") | .context.summary'\n", w.requester)
	return exitMergeFailed
}

// failedArea names where a failed merge failed, as the footer draws it.
func failedArea(failed *frontendv1.FooterStatusMergeFailed) string {
	switch {
	case failed.GetConflicts() != nil:
		return "conflicts"
	case failed.GetTests() != nil:
		return "tests"
	case failed.GetOther() != nil:
		return "other"
	}
	return "the footer has moved on"
}

// checkCommand reports the command's fate: refused (quarantined), or -- when
// the verb does not wait -- applied.
func (w *mergeWatch) checkCommand() (int, bool) {
	if code, done := w.checkQuarantine(); done {
		return code, true
	}
	if w.wait || w.file == "" {
		return 0, false
	}
	applied := filepath.Join(w.layout.OutputDir(), "applied", w.file)
	switch _, err := os.Stat(applied); {
	case err == nil:
		say(w.out, "merge-queue: requested; %s's merge of %s is put in line once this turn ends, and reports into this session\n", w.requesterName, w.source.label)
		return exitSuccess, true
	case errors.Is(err, os.ErrNotExist):
		return 0, false
	default:
		return w.fail("stat %s: %v", applied, err), true
	}
}

// checkQuarantine reports a command the ingress refused.
func (w *mergeWatch) checkQuarantine() (int, bool) {
	if w.file == "" {
		return 0, false
	}
	quarantined := filepath.Join(w.layout.OutputDir(), "quarantine", w.file)
	switch _, err := os.Stat(quarantined); {
	case err == nil:
		say(w.errOut, "claude-repld merge-queue: REFUSED: the daemon quarantined %s. Its reason is the ingress's warning:\n", quarantined)
		say(w.errOut, "  modules/app/agent-repl/bin/logs.sh --central --since 1h --json | jq -r 'select(.context.path // \"\" | endswith(\"%s\")) | .context.cause // empty'\n", w.file)
		return exitMergeRefused, true
	case errors.Is(err, os.ErrNotExist):
		return 0, false
	default:
		return w.fail("stat %s: %v", quarantined, err), true
	}
}

// fail prints the verb's own failure.
func (w *mergeWatch) fail(format string, a ...any) int {
	say(w.errOut, "claude-repld merge-queue: "+format+"\n", a...)
	return exitFailure
}

// callingRow finds the requesting workspace's row: the one whose worktree
// contains the caller's directory, the deepest when worktrees nest.
func callingRow(roster *frontendv1.WorkspaceRoster, cwd string) *frontendv1.RosterRow {
	var best *frontendv1.RosterRow
	bestLen := -1
	for _, row := range rosterRows(roster) {
		dir := canonicalDir(row.GetWorkspace().GetWorkspace().GetDir())
		if row.GetWorkspace().GetWorkspace().GetDir() == "" || !within(cwd, dir) {
			continue
		}
		if len(dir) > bestLen {
			best, bestLen = row, len(dir)
		}
	}
	return best
}

// rosterRows lists every row, in every grouping the roster carries.
func rosterRows(roster *frontendv1.WorkspaceRoster) []*frontendv1.RosterRow {
	var groups []*frontendv1.RosterRows
	for _, section := range roster.GetRepository().GetSections() {
		groups = append(groups, section.GetRows())
	}
	for _, section := range roster.GetTask().GetSections() {
		groups = append(groups, section.GetRows())
	}
	groups = append(groups, roster.GetRecentlyMerged().GetRows())
	var out []*frontendv1.RosterRow
	for _, group := range groups {
		out = append(out, group.GetRows()...)
	}
	return out
}

// findRosterRow finds a workspace's row by its canonical directory, in every
// grouping the roster carries.
func findRosterRow(roster *frontendv1.WorkspaceRoster, dir string) *frontendv1.RosterRow {
	var groups []*frontendv1.RosterRows
	for _, section := range roster.GetRepository().GetSections() {
		groups = append(groups, section.GetRows())
	}
	for _, section := range roster.GetTask().GetSections() {
		groups = append(groups, section.GetRows())
	}
	groups = append(groups, roster.GetRecentlyMerged().GetRows())
	for _, group := range groups {
		for _, row := range group.GetRows() {
			if canonicalDir(row.GetWorkspace().GetWorkspace().GetDir()) == dir {
				return row
			}
		}
	}
	return nil
}

// rowStatus names a row's status arm by its proto field name; a row the roster
// does not carry is statusAbsent.
func rowStatus(row *frontendv1.RosterRow) string {
	if row == nil {
		return statusAbsent
	}
	field := row.ProtoReflect().WhichOneof(row.ProtoReflect().Descriptor().Oneofs().ByName("status"))
	if field == nil {
		return "unset"
	}
	return string(field.Name())
}

// canonicalDir resolves a directory's symlinks through its longest existing
// prefix, so a path that no longer exists (a landed workspace's removed
// worktree) still compares equal to the spelling it had.
func canonicalDir(path string) string {
	clean := filepath.Clean(path)
	if resolved, err := filepath.EvalSymlinks(clean); err == nil {
		return resolved
	}
	parent := filepath.Dir(clean)
	if parent == clean {
		return clean
	}
	return filepath.Join(canonicalDir(parent), filepath.Base(clean))
}

// within reports whether path is dir or lies beneath it.
func within(path, dir string) bool {
	return path == dir || strings.HasPrefix(path, dir+string(filepath.Separator))
}

// say is the verb's one writer. The verb is a CLI whose product is its output:
// every line goes to the injected stdout or stderr, and the daemon logs every
// decision it makes itself.
func say(to io.Writer, format string, a ...any) {
	fmt.Fprintf(to, format, a...)
}
