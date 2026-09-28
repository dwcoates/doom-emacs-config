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
	"claude-repld/internal/workspace"
)

// THE `merge-queue` VERB:
//
//	claude-repld merge-queue -dir WORKTREE [-wait] [-state-dir DIR]
//	claude-repld merge-queue -branch BRANCH -repo MAIN_WORKTREE [-wait] [-state-dir DIR]
//
// It ENQUEUES a merge through the daemon's command-file ingress -- the same
// ingress, and so the same agent-requested path (merge.RequestedByAgent), the
// merge-queue skill's agents use -- and reads the outcome off the roster the
// daemon already streams. It decides nothing and never touches git.
//
//   - `-dir` names a workspace's worktree.
//   - `-branch` names a branch that is NO workspace (a subagent's). The verb
//     asks the daemon to create a workspace cut from it,
//     "merge-queue/<bare>-landing", and to merge THAT, in one command file:
//     the queue merges workspaces, and a branch reaches it through one, so the
//     conflict and test repairs have a session of their own to go to.
//
// Without -wait it returns once the daemon shows the merge in flight. With
// -wait it returns at the merge's outcome: LANDED (exit 0), PARKED awaiting
// guidance (exitMergeParked, with the daemon's parked line), or FAILED
// (exitMergeFailed, with where the daemon recorded the reason). A command the
// daemon refused is quarantined by the ingress, and the verb reports that
// (exitMergeRefused).
//
// AN AGENT CANNOT WAIT ON ITS OWN WORKSPACE'S MERGE. An agent-requested merge
// starts only once the requesting workspace's turn ends, and that turn is the
// one running the verb; -wait from inside the target worktree is refused.

// mergeQueueVerb is the verb's name on the command line.
const mergeQueueVerb = "merge-queue"

// The verb's outcome exits beyond exitSuccess (landed, or enqueued without
// -wait) and exitFailure (the verb could not do its job).
const (
	// exitMergeParked is a merge that stopped awaiting the user's guidance.
	exitMergeParked = 4
	// exitMergeFailed is a merge that failed.
	exitMergeFailed = 5
	// exitMergeRefused is a command the daemon refused and quarantined.
	exitMergeRefused = 6
)

// landingPrefix and landingSuffix spell the workspace a `-branch` merge lands
// through. The suffix keeps its worktree directory apart from the branch's own,
// which the same naming convention would otherwise put at the same path.
const (
	landingPrefix = "merge-queue/"
	landingSuffix = "-landing"
)

// The roster status arms the verb reads, spelled as the proto's oneof field
// names (frontend.v1.RosterRow.status).
const (
	statusAbsent     = "absent"
	statusEnqueuing  = "merge_enqueuing"
	statusQueued     = "merge_queued"
	statusMerging    = "merging"
	statusConflict   = "merge_conflict"
	statusMergeFail  = "merge_failed"
	statusMergedDone = "merged"
)

// worktreeLinePrefix leads the one output line naming the merged workspace's
// worktree, which the merge-queue skill's driver reads to fetch a failure's
// reason from that workspace's log.
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

// mergeQueueTarget is what one invocation merges.
type mergeQueueTarget struct {
	// label names the merge on the verb's output.
	label string
	// dir is the canonical worktree directory whose roster row reports the
	// merge.
	dir string
	// entries is the command file.
	entries []commandfile.Entry
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
		fmt.Fprintf(errOut, "claude-repld merge-queue: "+format+"\n", a...)
		return exitFailure
	}
	fs := flag.NewFlagSet(mergeQueueVerb, flag.ContinueOnError)
	fs.SetOutput(errOut)
	dir := fs.String("dir", "", "the worktree of the workspace to merge")
	branch := fs.String("branch", "", "a branch that is no workspace (a subagent's), merged through a workspace cut from it")
	repo := fs.String("repo", "", "with -branch: the repository's main worktree")
	wait := fs.Bool("wait", false, "wait for the outcome: landed (0), parked (4) or failed (5)")
	stateDir := fs.String("state-dir", "", "state root, overriding $AGENT_REPL_STATE_DIR")
	if err := fs.Parse(args); err != nil {
		return exitFailure
	}
	if fs.NArg() != 0 {
		return fail("unexpected arguments: %s", strings.Join(fs.Args(), " "))
	}
	target, err := resolveMergeQueueTarget(*dir, *branch, *repo)
	if err != nil {
		return fail("%v", err)
	}
	if *wait {
		cwd, err := env.getwd()
		if err != nil {
			return fail("read the working directory: %v", err)
		}
		if within(canonicalDir(cwd), target.dir) {
			return fail("refusing -wait from inside %s: an agent's merge of its own workspace starts only when its turn ends, "+
				"and this turn is the one that would wait. Enqueue without -wait and end the turn; the merge reports into this session.", target.dir)
		}
	}
	layout, err := stateroot.Root(*stateDir, envc.Load().WithStateDir(*stateDir).StateDir())
	if err != nil {
		return fail("resolve the state root: %v", err)
	}
	w := &mergeWatch{target: target, wait: *wait, dial: dial, layout: layout, env: env, out: out, errOut: errOut}
	return w.run(ctx)
}

// resolveMergeQueueTarget turns the flags into what to enqueue and which
// roster row reports it.
func resolveMergeQueueTarget(dir, branch, repo string) (mergeQueueTarget, error) {
	switch {
	case dir != "" && branch != "":
		return mergeQueueTarget{}, errors.New("-dir and -branch name two different merges; give one")
	case dir != "" && repo != "":
		return mergeQueueTarget{}, errors.New("-repo goes with -branch; a -dir merge's repository is its workspace's")
	case dir != "":
		abs, err := filepath.Abs(dir)
		if err != nil {
			return mergeQueueTarget{}, fmt.Errorf("resolve -dir %q: %w", dir, err)
		}
		if info, err := os.Stat(abs); err != nil || !info.IsDir() {
			return mergeQueueTarget{}, fmt.Errorf("-dir %q is not a worktree directory on disk", abs)
		}
		return mergeQueueTarget{
			label:   filepath.Base(abs),
			dir:     canonicalDir(abs),
			entries: []commandfile.Entry{{Type: commandfile.TypeMerge, ProjectDir: abs}},
		}, nil
	case branch != "" && repo == "":
		return mergeQueueTarget{}, errors.New("-branch needs -repo, the repository's main worktree")
	case branch != "":
		absRepo, err := filepath.Abs(repo)
		if err != nil {
			return mergeQueueTarget{}, fmt.Errorf("resolve -repo %q: %w", repo, err)
		}
		name := landingPrefix + workspace.BareName(branch) + landingSuffix
		landing, err := workspace.WorktreeDir(absRepo, name)
		if err != nil {
			return mergeQueueTarget{}, fmt.Errorf("derive the landing workspace's directory: %w", err)
		}
		if _, err := os.Stat(landing); err == nil {
			return mergeQueueTarget{}, fmt.Errorf("the landing workspace's directory %s already exists; "+
				"an earlier landing of this branch is still there, and it must be resolved first", landing)
		} else if !errors.Is(err, os.ErrNotExist) {
			return mergeQueueTarget{}, fmt.Errorf("stat the landing workspace's directory %s: %w", landing, err)
		}
		return mergeQueueTarget{
			label: name,
			dir:   canonicalDir(landing),
			entries: []commandfile.Entry{
				{Type: commandfile.TypeCreate, GitRoot: absRepo, Name: name, BaseRef: branch},
				{Type: commandfile.TypeMerge, ProjectDir: landing, Workspace: name},
			},
		}, nil
	default:
		return mergeQueueTarget{}, errors.New("name the merge: -dir WORKTREE, or -branch BRANCH -repo MAIN_WORKTREE")
	}
}

// mergeWatch is one invocation's watch over the roster.
type mergeWatch struct {
	target mergeQueueTarget
	wait   bool
	dial   mergeQueueDialer
	layout stateroot.Layout
	env    mergeQueueEnv
	out    io.Writer
	errOut io.Writer

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
			if code, done := w.checkQuarantine(); done {
				return code
			}
		case ev := <-events:
			if ev.err != nil {
				if !errors.Is(ev.err, errStreamEnding) {
					return w.fail("the roster stream failed: %v", ev.err)
				}
				fmt.Fprintf(w.out, "merge-queue: the daemon stood down; reattaching\n")
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
	row := findRosterRow(first.roster, w.target.dir)
	w.baseline = rowStatus(row)
	w.baselineMergedAt = row.GetWhen().GetMerged().GetAtMs()
	w.last = w.baseline
	name, err := commandfile.Write(w.layout.OutputDir(), w.target.entries)
	if err != nil {
		return nil, err
	}
	w.file = name
	fmt.Fprintf(w.out, "merge-queue: enqueued %s (command file %s)\n", w.target.label, name)
	fmt.Fprintf(w.out, "%s%s\n", worktreeLinePrefix, w.target.dir)
	return events, nil
}

// observe reads one roster frame, and answers the exit status once the merge
// reached a point the verb reports.
func (w *mergeWatch) observe(ctx context.Context, roster *frontendv1.WorkspaceRoster) (int, bool) {
	if code, done := w.checkQuarantine(); done {
		return code, true
	}
	row := findRosterRow(roster, w.target.dir)
	status := rowStatus(row)
	if status != w.last {
		fmt.Fprintf(w.out, "merge-queue: %s: %s\n", w.target.label, status)
		w.last = status
	}
	switch status {
	case statusEnqueuing, statusQueued, statusMerging:
		w.seenLive = true
		if !w.wait {
			fmt.Fprintf(w.out, "merge-queue: %s is in the queue\n", w.target.label)
			return exitSuccess, true
		}
		return 0, false
	case statusMergedDone, statusMergeFail, statusConflict:
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
	switch status {
	case statusMergedDone:
		fmt.Fprintf(w.out, "merge-queue: LANDED: %s is on its target\n", w.target.label)
		return exitSuccess
	case statusMergeFail:
		fmt.Fprintf(w.out, "merge-queue: FAILED: %s did not land. The reason is the daemon's daemon.merge.abort record:\n", w.target.label)
		fmt.Fprintf(w.out, "  modules/app/agent-repl/bin/logs.sh --workspace %s --since 6h --json | jq -r 'select(.operation == \"daemon.merge.abort\") | .context.summary'\n", w.target.dir)
		return exitMergeFailed
	default:
		footer, err := w.daemon.Footer(ctx, row.GetWorkspace().GetWorkspace())
		if err != nil {
			fmt.Fprintf(w.errOut, "claude-repld merge-queue: %s stopped awaiting guidance, and its footer could not be read: %v\n", w.target.label, err)
			return exitMergeParked
		}
		stopped := footer.GetStrip().GetStatus().GetMergeConflict()
		switch {
		case stopped == nil:
			// The roster and the footer are two streams; the footer moved on
			// between the frame that concluded and this read.
			fmt.Fprintf(w.out, "merge-queue: PARKED: %s stopped awaiting guidance; its footer has since moved on\n", w.target.label)
		case stopped.GetParked() == nil:
			// The arm's own meaning: stopped on a conflict, whose name is the
			// whole fact.
			fmt.Fprintf(w.out, "merge-queue: PARKED: %s stopped on a conflict its resolution could not settle\n", w.target.label)
		default:
			fmt.Fprintf(w.out, "merge-queue: PARKED: %s: %s\n", w.target.label, stopped.GetParked().GetLine())
		}
		return exitMergeParked
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
		fmt.Fprintf(w.errOut, "claude-repld merge-queue: REFUSED: the daemon quarantined %s. Its reason is the ingress's warning:\n", quarantined)
		fmt.Fprintf(w.errOut, "  modules/app/agent-repl/bin/logs.sh --central --since 1h --json | jq -r 'select(.context.path // \"\" | endswith(\"%s\")) | .context.cause // empty'\n", w.file)
		return exitMergeRefused, true
	case errors.Is(err, os.ErrNotExist):
		return 0, false
	default:
		return w.fail("stat %s: %v", quarantined, err), true
	}
}

// fail prints the verb's own failure.
func (w *mergeWatch) fail(format string, a ...any) int {
	fmt.Fprintf(w.errOut, "claude-repld merge-queue: "+format+"\n", a...)
	return exitFailure
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
