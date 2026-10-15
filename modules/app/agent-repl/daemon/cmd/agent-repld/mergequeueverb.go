package main

import (
	"bufio"
	"bytes"
	"context"
	"encoding/json"
	"errors"
	"flag"
	"fmt"
	"io"
	"os"
	"path/filepath"
	"strings"
	"time"

	agentreplv1 "agentrepl/proto/agentrepl/v1"
	"agentrepl/proto/agentrepl/v1/agentreplv1connect"
	frontendv1 "agentrepl/proto/frontend/v1"
	workspacev1 "agentrepl/proto/workspace/v1"
	"agentrepl/protohelpers/rosterwalk"

	"agent-repld/internal/commandfile"
	"agent-repld/internal/envc"
	"agent-repld/internal/protoarm"
	"agent-repld/internal/stateroot"
)

// THE `merge-queue` VERB:
//
//	agent-repld merge-queue -one-shot [-pr-merged] [-front] [-version-part PART -changelog-line TEXT] [-wait] [-state-dir DIR]
//	agent-repld merge-queue -async -name NAME -branch BRANCH [-instruction TEXT] [-close-dir WORKTREE] [-cleanup-worktree] [-compact-after] [-before-merge TEXT] [-after-merge TEXT] [-front] [-version-part PART -changelog-line TEXT] [-state-dir DIR]
//	agent-repld merge-queue -evict | -evict-dir WORKTREE [-answer-timeout D] [-state-dir DIR]
//	agent-repld merge-queue -move-up | -move-down [-move-dir WORKTREE] [-answer-timeout D] [-state-dir DIR]
//	agent-repld merge-queue -pause | -resume [-repository-dir DIR] [-answer-timeout D] [-state-dir DIR]
//	agent-repld merge-queue -hold-rollouts | -release-rollouts [-answer-timeout D] [-state-dir DIR]
//	agent-repld merge-queue -disable-racing | -enable-racing [-answer-timeout D] [-state-dir DIR]
//
// A MERGE RUNS IN THE WORKSPACE THAT ASKED FOR IT, and the verb asks FROM THE
// CALLING WORKSPACE: the workspace whose worktree contains the caller's working
// directory, found on the roster the daemon streams. It ENQUEUES through the
// daemon's command-file ingress -- the same agent-requested path
// (merge.RequestedByAgent) the merge-queue skill's agents use -- naming what
// the calling workspace merges:
//
//   - `-one-shot` its own branch, SYNC: merged in place once the turn ends,
//     closing the calling workspace once it lands, and marked a one-shot
//     from the moment the daemon applies the request; with `-pr-merged` its
//     own branch already merged upstream, which updates the default branch
//     and closes it;
//   - `-async -name NAME -branch BRANCH` the named branch, ASYNC: in a hidden
//     merge workspace named NAME, forked from the calling workspace's
//     conversation onto BRANCH's tip, leaving the calling workspace free.
//     `-instruction TEXT` has the merge workspace prepare the branch first;
//     `-close-dir WORKTREE` names another workspace to close once it lands;
//     `-cleanup-worktree` removes the worktree that has BRANCH checked out
//     once it lands; `-compact-after` compacts the calling workspace then.
//     `-before-merge TEXT` and `-after-merge TEXT` are the merge actions the
//     MERGE WORKSPACE runs (agentrepl.v1.MergeWorkspaceAsync.merge_actions):
//     the first before the landing, failing the merge if it fails, the
//     second after it lands, never failing it. They are the only actions it
//     runs: the calling workspace's own configured actions are never
//     inherited.
//
// `-front` puts any of them at the FRONT of its queue once it is put in line:
// ahead of every ordinary merge still waiting, never ahead of the one running.
//
// `-version-part minor|patch` with `-changelog-line TEXT` is how a merge onto
// agent-repl's master moves the version: the queue tags the landed merge with
// the next version, annotated with the line. The daemon refuses an agent's
// merge onto agent-repl's master without them, and any other merge with them
// (agentrepl.v1.MergeWorkspaceRequest.version_increment).
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
//
// THE QUEUE'S CONTROLS ask, from the calling workspace too, through the same
// ingress and the same writer, for the queue itself to change:
//
//   - `-evict` takes the calling workspace's merge off the queue, `-evict-dir`
//     another workspace's (merge_evict);
//   - `-move-up` / `-move-down` move the calling workspace's waiting merge one
//     place up or down its queue, or `-move-dir`'s workspace's
//     (merge_move_up, merge_move_down);
//   - `-pause` / `-resume` pause or resume the queue of the repository whose
//     main checkout is `-repository-dir`, else of every repository
//     (merge_pause, merge_resume);
//   - `-hold-rollouts` / `-release-rollouts` hold or release the queue's
//     ROLLOUTS, daemon-wide: while held, merges still land, but a landing in
//     the daemon's own checkout no longer redeploys, hot-reloads or bounces
//     the runtime (merge_hold_rollouts, merge_release_rollouts);
//   - `-disable-racing` / `-enable-racing` disable or enable the queue's
//     RACING, daemon-wide: while disabled, every queue works its head alone,
//     whatever the host's power source (merge_disable_racing,
//     merge_enable_racing).
//
// A control is answered AT ONCE, so the verb waits (at most -answer-timeout)
// for the file's fate and prints the daemon's own answer, read off the record
// the ingress keys by the file's path in the run log: `outcome: <outcome>`
// (evicted, moved_up, moved_down, unmoved, not_queued, paused, resumed, held, released, racing_disabled,
// racing_enabled) when applied, or the refusal's cause
// when quarantined. An answer the run log does not carry is printed as unread,
// on stderr, and the file's fate still decides the exit.

// mergeQueueVerb is the verb's name on the command line.
const mergeQueueVerb = "merge-queue"

// The verb's outcome exits beyond exitSuccess (landed, or requested without
// -wait) and exitFailure (the verb could not do its job).
const (
	// exitMergeFailed is a merge that failed.
	exitMergeFailed = 5
	// exitMergeRefused is a command the daemon refused and quarantined.
	exitMergeRefused = 6
	// exitControlUnanswered is a queue control whose file the daemon did not
	// retire within -answer-timeout; the file stays in the ingress, pending.
	exitControlUnanswered = 7
)

// defaultAnswerTimeout bounds the wait for a queue control's answer. The
// ingress answers within a poll interval of a serving daemon.
const defaultAnswerTimeout = 15 * time.Second

// The run-log operations a queue control's answer is read from
// (internal/commandfile's records, keyed by the file's path).
const (
	opControlOutcome    = "daemon.commandfile.merge_queue"
	opControlRefusal    = "daemon.commandfile.entry"
	opControlMalformed  = "daemon.commandfile.quarantine"
	outcomeLinePrefix   = "merge-queue: outcome: "
	unreadControlAnswer = "unread"
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

// mergeQueueSource is what one invocation asks for: the entry's type and
// source fields, its label on the verb's output, and whether it is a queue
// control rather than a merge.
type mergeQueueSource struct {
	label   string
	entry   commandfile.Entry
	control bool
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
	stream, err := openDaemonStream(ctx, d.client.WatchWorkspaceRoster, &agentreplv1.WatchWorkspaceRosterRequest{})
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
	stream, err := openDaemonStream(ctx, d.client.WatchFooter, &agentreplv1.WatchFooterRequest{Workspace: ref})
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
		say(errOut, "agent-repld merge-queue: "+format+"\n", a...)
		return exitFailure
	}
	fs := flag.NewFlagSet(mergeQueueVerb, flag.ContinueOnError)
	fs.SetOutput(errOut)
	oneShot := fs.Bool("one-shot", false, "merge the calling workspace's own branch SYNC, in place once the calling turn ends, closing the calling workspace once it lands")
	prMerged := fs.Bool("pr-merged", false, "with -one-shot: the calling workspace's branch already merged upstream: update the default branch and close it")
	async := fs.Bool("async", false, "with -name and -branch: merge the branch ASYNC, in a hidden merge workspace forked from the calling workspace, which stays free")
	name := fs.String("name", "", "with -async: the merge workspace's name, a short slug naming what is merged")
	branch := fs.String("branch", "", "with -async: the branch to land")
	instruction := fs.String("instruction", "", "with -async: how the merge workspace prepares the branch before it joins the queue")
	closeDir := fs.String("close-dir", "", "with -async: another workspace, by its worktree, to close once the merge lands")
	cleanupWorktree := fs.Bool("cleanup-worktree", false, "with -async: remove the worktree that has the branch checked out once the merge lands")
	compactAfter := fs.Bool("compact-after", false, "with -async: compact the calling workspace's conversation once the branch lands")
	beforeMerge := fs.String("before-merge", "", "with -async: a prompt the merge workspace runs before the landing; its failure fails the merge")
	afterMerge := fs.String("after-merge", "", "with -async: a prompt the merge workspace runs after the branch lands; it never fails the merge")
	front := fs.Bool("front", false, "with a merge: put it at the front of its queue, ahead of every ordinary merge waiting, never ahead of the one running")
	versionPart := fs.String("version-part", "", "with a merge onto agent-repl's master: the version number it moves, minor or patch")
	changelogLine := fs.String("changelog-line", "", "with -version-part: the one-line changelog entry the version's tag carries")
	evict := fs.Bool("evict", false, "take the calling workspace's merge off the queue")
	evictDir := fs.String("evict-dir", "", "take another workspace's merge off the queue, by its worktree")
	moveUp := fs.Bool("move-up", false, "move the calling workspace's waiting merge one place up its queue (or -move-dir's)")
	moveDown := fs.Bool("move-down", false, "move the calling workspace's waiting merge one place down its queue (or -move-dir's)")
	moveDir := fs.String("move-dir", "", "with -move-up or -move-down: move another workspace's merge, by its worktree")
	pause := fs.Bool("pause", false, "pause the merge queue (every repository's, or -repository-dir's)")
	resume := fs.Bool("resume", false, "resume the merge queue (every repository's, or -repository-dir's)")
	holdRollouts := fs.Bool("hold-rollouts", false, "hold the merge queue's rollouts: merges still land, but a landing no longer redeploys")
	releaseRollouts := fs.Bool("release-rollouts", false, "release the merge queue's rollouts: the next landing redeploys")
	disableRacing := fs.Bool("disable-racing", false, "disable the merge queue's racing: every queue works its head alone, whatever the power source")
	enableRacing := fs.Bool("enable-racing", false, "enable the merge queue's racing: on AC power each queue works its top two merges")
	repositoryDir := fs.String("repository-dir", "", "with -pause or -resume: the repository's main checkout")
	answerTimeout := fs.Duration("answer-timeout", defaultAnswerTimeout, "with a queue control: how long to wait for the daemon's answer")
	wait := fs.Bool("wait", false, "wait for the outcome: landed (0) or failed (5)")
	stateDir := fs.String("state-dir", "", "state root, overriding $AGENT_REPL_STATE_DIR")
	if err := fs.Parse(args); err != nil {
		return exitFailure
	}
	if fs.NArg() != 0 {
		return fail("unexpected arguments: %s", strings.Join(fs.Args(), " "))
	}
	source, err := resolveMergeQueueSource(mergeQueueFlags{
		oneShot: *oneShot, prMerged: *prMerged, async: *async, name: *name, branch: *branch, instruction: *instruction,
		closeDir: *closeDir, cleanupWorktree: *cleanupWorktree, compactAfter: *compactAfter, beforeMerge: *beforeMerge, afterMerge: *afterMerge, wait: *wait, front: *front,
		versionPart: *versionPart, changelogLine: *changelogLine,
		evict: *evict, evictDir: *evictDir, moveUp: *moveUp, moveDown: *moveDown, moveDir: *moveDir,
		pause: *pause, resume: *resume, repositoryDir: *repositoryDir,
		holdRollouts: *holdRollouts, releaseRollouts: *releaseRollouts,
		disableRacing: *disableRacing, enableRacing: *enableRacing,
	})
	if err != nil {
		return fail("%v", err)
	}
	if source.control && *wait {
		return fail("-wait waits for a merge's outcome; a queue control is answered at once")
	}
	if *answerTimeout <= 0 {
		return fail("-answer-timeout must be positive, not %s", *answerTimeout)
	}
	cwd, err := env.getwd()
	if err != nil {
		return fail("read the working directory: %v", err)
	}
	layout, err := stateroot.Root(*stateDir, envc.Load().WithStateDir(*stateDir).StateDir())
	if err != nil {
		return fail("resolve the state root: %v", err)
	}
	w := &mergeWatch{source: source, cwd: canonicalDir(cwd), wait: *wait, answerTimeout: *answerTimeout,
		dial: dial, layout: layout, env: env, out: out, errOut: errOut}
	return w.run(ctx)
}

// mergeQueueFlags are the flags that name what one invocation asks for.
type mergeQueueFlags struct {
	// oneShot asks for the calling workspace's own branch SYNC, in place,
	// closing it once it lands; prMerged says that branch already merged
	// upstream.
	oneShot, prMerged, wait bool
	// async asks for BRANCH ASYNC, in a hidden merge workspace named name;
	// instruction prepares the branch first, closeDir names a workspace to
	// close once it lands, cleanupWorktree removes the branch's worktree
	// then, and compactAfter compacts the calling workspace then.
	async                 bool
	name, branch          string
	instruction, closeDir string
	cleanupWorktree       bool
	compactAfter          bool
	// beforeMerge and afterMerge are the merge workspace's own merge actions
	// (commandfile.Entry.BeforeWSMerge, .PostprocessingPrompt).
	beforeMerge, afterMerge string
	// front puts the merge at the front of its queue (commandfile.Entry.Front).
	front bool
	// versionPart and changelogLine are the merge's version increment
	// (commandfile.Entry.VersionPart, .ChangelogLine).
	versionPart, changelogLine string
	evict                      bool
	evictDir                   string
	// moveUp and moveDown move a waiting merge one place; moveDir names
	// another workspace's merge to move rather than the calling workspace's.
	moveUp, moveDown bool
	moveDir          string
	pause, resume    bool
	repositoryDir    string
	// holdRollouts and releaseRollouts hold or release the queue's rollouts.
	holdRollouts, releaseRollouts bool
	// disableRacing and enableRacing disable or enable the queue's racing.
	disableRacing, enableRacing bool
}

// resolveMergeQueueSource turns the flags into what the calling workspace
// asks for, puts a merge asked for with -front at the front of its queue, and
// carries a merge's version increment.
func resolveMergeQueueSource(f mergeQueueFlags) (mergeQueueSource, error) {
	source, err := resolveMergeQueueRequest(f)
	if err != nil {
		return source, err
	}
	if f.front {
		if source.control {
			return mergeQueueSource{}, errors.New("-front goes with a merge: it places the merge in its queue, and a queue control places nothing")
		}
		source.entry.Front = true
		source.label += ", to the front of the queue"
	}
	if f.versionPart != "" || f.changelogLine != "" {
		if source.control {
			return mergeQueueSource{}, errors.New("-version-part and -changelog-line go with a merge: they move the version it lands, and a queue control lands nothing")
		}
		source.entry.VersionPart, source.entry.ChangelogLine = f.versionPart, f.changelogLine
		// The entry's own rule refuses a malformed increment here, before any
		// file is written, with the same words the daemon would.
		if _, err := source.entry.VersionIncrement(); err != nil {
			return mergeQueueSource{}, err
		}
		source.label += ", moving the version's " + f.versionPart
	}
	return source, nil
}

// resolveMergeQueueRequest turns the flags into the one merge or control they
// name.
func resolveMergeQueueRequest(f mergeQueueFlags) (mergeQueueSource, error) {
	named := 0
	for _, set := range []bool{f.oneShot, f.async, f.evict, f.evictDir != "", f.moveUp, f.moveDown, f.pause, f.resume, f.holdRollouts, f.releaseRollouts, f.disableRacing, f.enableRacing} {
		if set {
			named++
		}
	}
	switch {
	case named == 0:
		return mergeQueueSource{}, errors.New("name the merge or control: -one-shot, -async, -evict, -evict-dir WORKTREE, -move-up, -move-down, -pause, -resume, -hold-rollouts, -release-rollouts, -disable-racing or -enable-racing")
	case named > 1:
		return mergeQueueSource{}, errors.New("-one-shot, -async, -evict, -evict-dir, -move-up, -move-down, -pause, -resume, -hold-rollouts, -release-rollouts, -disable-racing and -enable-racing each name a different request; give one")
	case f.prMerged && !f.oneShot:
		return mergeQueueSource{}, errors.New("-pr-merged goes with -one-shot: it closes the calling workspace")
	case !f.async && (f.name != "" || f.branch != "" || f.instruction != "" || f.closeDir != "" || f.cleanupWorktree || f.compactAfter || f.beforeMerge != "" || f.afterMerge != ""):
		return mergeQueueSource{}, errors.New("-name, -branch, -instruction, -close-dir, -cleanup-worktree, -compact-after, -before-merge and -after-merge go with -async")
	case f.async && f.wait:
		return mergeQueueSource{}, errors.New("-wait waits on a merge in the calling workspace; an async merge reports back to its conversation instead")
	case f.async && strings.TrimSpace(f.name) == "":
		return mergeQueueSource{}, errors.New("-async names its merge workspace: give -name NAME")
	case f.async && strings.TrimSpace(f.branch) == "":
		return mergeQueueSource{}, errors.New("-async names the branch it lands: give -branch BRANCH")
	case f.instruction != "" && strings.TrimSpace(f.instruction) == "":
		return mergeQueueSource{}, errors.New("-instruction says how to prepare the branch")
	case f.beforeMerge != "" && strings.TrimSpace(f.beforeMerge) == "":
		return mergeQueueSource{}, errors.New("-before-merge is the prompt the merge workspace runs before the landing")
	case f.afterMerge != "" && strings.TrimSpace(f.afterMerge) == "":
		return mergeQueueSource{}, errors.New("-after-merge is the prompt the merge workspace runs after the landing")
	case f.moveDir != "" && !f.moveUp && !f.moveDown:
		return mergeQueueSource{}, errors.New("-move-dir goes with -move-up or -move-down: it names the workspace whose merge they move")
	case f.repositoryDir != "" && !f.pause && !f.resume:
		return mergeQueueSource{}, errors.New("-repository-dir goes with -pause or -resume: it names the repository whose queue they address")
	}
	switch {
	case f.evict:
		return mergeQueueSource{label: "its own merge off the queue", control: true, entry: commandfile.Entry{Type: commandfile.TypeMergeEvict}}, nil
	case f.evictDir != "":
		abs, err := existingDir("-evict-dir", f.evictDir)
		if err != nil {
			return mergeQueueSource{}, err
		}
		return mergeQueueSource{label: filepath.Base(abs) + "'s merge off the queue", control: true,
			entry: commandfile.Entry{Type: commandfile.TypeMergeEvict, EvictDir: abs}}, nil
	case f.moveUp, f.moveDown:
		typ, way := commandfile.TypeMergeMoveUp, "up"
		if f.moveDown {
			typ, way = commandfile.TypeMergeMoveDown, "down"
		}
		if f.moveDir == "" {
			return mergeQueueSource{label: "its own merge one place " + way + " the queue", control: true, entry: commandfile.Entry{Type: typ}}, nil
		}
		abs, err := existingDir("-move-dir", f.moveDir)
		if err != nil {
			return mergeQueueSource{}, err
		}
		return mergeQueueSource{label: filepath.Base(abs) + "'s merge one place " + way + " the queue", control: true,
			entry: commandfile.Entry{Type: typ, MoveDir: abs}}, nil
	case f.holdRollouts:
		return mergeQueueSource{label: "a hold of the merge queue's rollouts", control: true, entry: commandfile.Entry{Type: commandfile.TypeMergeHoldRollouts}}, nil
	case f.releaseRollouts:
		return mergeQueueSource{label: "a release of the merge queue's rollouts", control: true, entry: commandfile.Entry{Type: commandfile.TypeMergeReleaseRollouts}}, nil
	case f.disableRacing:
		return mergeQueueSource{label: "a disable of the merge queue's racing", control: true, entry: commandfile.Entry{Type: commandfile.TypeMergeDisableRacing}}, nil
	case f.enableRacing:
		return mergeQueueSource{label: "an enable of the merge queue's racing", control: true, entry: commandfile.Entry{Type: commandfile.TypeMergeEnableRacing}}, nil
	case f.pause, f.resume:
		typ, verb := commandfile.TypeMergePause, "a pause"
		if f.resume {
			typ, verb = commandfile.TypeMergeResume, "a resume"
		}
		label := verb + " of every repository's queue"
		var repo string
		if f.repositoryDir != "" {
			abs, err := existingDir("-repository-dir", f.repositoryDir)
			if err != nil {
				return mergeQueueSource{}, err
			}
			repo, label = abs, verb+" of "+abs+"'s queue"
		}
		return mergeQueueSource{label: label, control: true, entry: commandfile.Entry{Type: typ, RepositoryDir: repo}}, nil
	}
	// A ONE-SHOT is announced from inside the turn it follows: it runs in
	// place and closes the calling workspace once it lands.
	if f.oneShot {
		if f.prMerged {
			return mergeQueueSource{label: "its own branch, already merged upstream, as a one-shot",
				entry: commandfile.Entry{Type: commandfile.TypeMerge, OneShot: true, PRWasMerged: true}}, nil
		}
		return mergeQueueSource{label: "its own branch, as a one-shot", entry: commandfile.Entry{Type: commandfile.TypeMerge, OneShot: true}}, nil
	}
	// AN ASYNC MERGE lands the named branch in a hidden merge workspace.
	name, branch, instruction := strings.TrimSpace(f.name), strings.TrimSpace(f.branch), strings.TrimSpace(f.instruction)
	entry := commandfile.Entry{Type: commandfile.TypeMerge, AsyncName: name, Branch: branch, Instruction: instruction,
		CleanupWorktree: f.cleanupWorktree, CompactAfter: f.compactAfter,
		BeforeWSMerge: strings.TrimSpace(f.beforeMerge), PostprocessingPrompt: strings.TrimSpace(f.afterMerge)}
	label := branch + ", async, in merge workspace " + name
	if instruction != "" {
		label += ", prepared as instructed"
	}
	if f.closeDir != "" {
		abs, err := existingDir("-close-dir", f.closeDir)
		if err != nil {
			return mergeQueueSource{}, err
		}
		entry.CloseDir = abs
		label += ", closing " + filepath.Base(abs) + " once it lands"
	}
	if f.cleanupWorktree {
		label += ", removing the branch's worktree once it lands"
	}
	if f.compactAfter {
		label += ", compacting this workspace once it lands"
	}
	if entry.BeforeWSMerge != "" {
		label += ", running its before-merge prompt"
	}
	if entry.PostprocessingPrompt != "" {
		label += ", running its after-merge prompt once it lands"
	}
	return mergeQueueSource{label: label, entry: entry}, nil
}

// existingDir resolves a flag's directory to an absolute path that is a
// directory on disk.
func existingDir(flagName, dir string) (string, error) {
	abs, err := filepath.Abs(dir)
	if err != nil {
		return "", fmt.Errorf("resolve %s %q: %w", flagName, dir, err)
	}
	if info, err := os.Stat(abs); err != nil || !info.IsDir() {
		return "", fmt.Errorf("%s %q is not a directory on disk", flagName, abs)
	}
	return abs, nil
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
	// answerTimeout bounds a queue control's wait for its answer.
	answerTimeout time.Duration
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
	var unanswered <-chan time.Time
	if w.source.control {
		timer := time.NewTimer(w.answerTimeout)
		defer timer.Stop()
		unanswered = timer.C
	}
	for {
		select {
		case <-ctx.Done():
			return w.fail("interrupted before the merge's outcome: %v", ctx.Err())
		case <-unanswered:
			if code, done := w.checkCommand(); done {
				return code
			}
			say(w.errOut, "agent-repld merge-queue: NO ANSWER: the daemon did not take %s within %s; the request stays pending in %s\n",
				w.file, w.answerTimeout, w.layout.OutputDir())
			return exitControlUnanswered
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
	entry.ProjectDir = row.GetWorkspace().GetWorkspace().GetDir()
	entry.Workspace = row.GetWorkspace().GetWorkspace().GetId()
	name, err := commandfile.Write(w.layout.OutputDir(), []commandfile.Entry{entry})
	if err != nil {
		return nil, err
	}
	w.file = name
	if w.source.control {
		say(w.out, "merge-queue: %s asked for %s (command file %s)\n", w.requesterName, w.source.label, name)
	} else {
		say(w.out, "merge-queue: %s asked to merge %s (command file %s)\n", w.requesterName, w.source.label, name)
	}
	say(w.out, "%s%s\n", worktreeLinePrefix, w.requester)
	return events, nil
}

// observe reads one roster frame, and answers the exit status once the merge
// reached a point the verb reports.
func (w *mergeWatch) observe(ctx context.Context, roster *frontendv1.WorkspaceRoster) (int, bool) {
	if code, done := w.checkCommand(); done {
		return code, true
	}
	if w.source.control {
		return 0, false
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
		say(w.errOut, "agent-repld merge-queue: %s's merge failed, and its footer could not be read for the area: %v\n", w.requesterName, err)
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
	case err == nil && w.source.control:
		return w.reportApplied(), true
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
	case err == nil && w.source.control:
		say(w.errOut, "agent-repld merge-queue: REFUSED: the daemon quarantined %s.\n", quarantined)
		record, found, err := readControlAnswer(w.layout, w.file, opControlRefusal, opControlMalformed)
		switch {
		case err != nil:
			say(w.errOut, "agent-repld merge-queue: the refusal's cause is unread: %v\n", err)
		case !found:
			say(w.errOut, "agent-repld merge-queue: the refusal's cause is unread: the run log carries no record for %s\n", w.file)
		default:
			say(w.errOut, "agent-repld merge-queue: cause: %s\n", record.Context.Cause)
		}
		return exitMergeRefused, true
	case err == nil:
		say(w.errOut, "agent-repld merge-queue: REFUSED: the daemon quarantined %s. Its reason is the ingress's warning:\n", quarantined)
		say(w.errOut, "  modules/app/agent-repl/bin/logs.sh --central --since 1h --json | jq -r 'select(.context.path // \"\" | endswith(\"%s\")) | .context.cause // empty'\n", w.file)
		return exitMergeRefused, true
	case errors.Is(err, os.ErrNotExist):
		return 0, false
	default:
		return w.fail("stat %s: %v", quarantined, err), true
	}
}

// reportApplied prints an applied queue control's outcome, read off the
// ingress's record. The file's fate is the answer the exit carries; an
// outcome the run log does not hold is printed as unread, never guessed.
func (w *mergeWatch) reportApplied() int {
	record, found, err := readControlAnswer(w.layout, w.file, opControlOutcome)
	outcome := unreadControlAnswer
	switch {
	case err != nil:
		say(w.errOut, "agent-repld merge-queue: %s was applied, and its outcome is unread: %v\n", w.file, err)
	case !found:
		say(w.errOut, "agent-repld merge-queue: %s was applied, and its outcome is unread: the run log carries no %s record for it\n", w.file, opControlOutcome)
	default:
		outcome = record.Context.Outcome
		say(w.out, "merge-queue: APPLIED: %s\n", record.Message)
	}
	say(w.out, "%s%s\n", outcomeLinePrefix, outcome)
	return exitSuccess
}

// controlRecord is the slice of a run-log record a queue control's answer
// is read from.
type controlRecord struct {
	Operation string `json:"operation"`
	Message   string `json:"message"`
	Context   struct {
		Path    string `json:"path"`
		Outcome string `json:"outcome"`
		Cause   string `json:"cause"`
	} `json:"context"`
}

// readControlAnswer finds the last record of one of ops that the ingress
// keyed by the command file named file, in the run log and its newest backup
// generation (a rotation may land between the record and the read).
func readControlAnswer(layout stateroot.Layout, file string, ops ...string) (controlRecord, bool, error) {
	for _, path := range []string{layout.RunLog(), layout.RunLog() + ".1"} {
		record, found, err := scanControlAnswer(path, file, ops)
		if err != nil || found {
			return record, found, err
		}
	}
	return controlRecord{}, false, nil
}

// scanControlAnswer is readControlAnswer over one log file; a file that does
// not exist holds no answer.
func scanControlAnswer(path, file string, ops []string) (controlRecord, bool, error) {
	f, err := os.Open(path)
	if errors.Is(err, os.ErrNotExist) {
		return controlRecord{}, false, nil
	}
	if err != nil {
		return controlRecord{}, false, fmt.Errorf("open %s: %w", path, err)
	}
	defer f.Close()
	var last controlRecord
	found := false
	scanner := bufio.NewScanner(f)
	scanner.Buffer(make([]byte, 0, 64*1024), 16*1024*1024)
	needle := []byte(file)
	for scanner.Scan() {
		line := scanner.Bytes()
		if !bytes.Contains(line, needle) {
			continue
		}
		var record controlRecord
		if err := json.Unmarshal(line, &record); err != nil {
			return controlRecord{}, false, fmt.Errorf("decode a record in %s naming %s: %w", path, file, err)
		}
		if filepath.Base(record.Context.Path) != file {
			continue
		}
		for _, op := range ops {
			if record.Operation == op {
				last, found = record, true
			}
		}
	}
	if err := scanner.Err(); err != nil {
		return controlRecord{}, false, fmt.Errorf("read %s: %w", path, err)
	}
	return last, found, nil
}

// fail prints the verb's own failure.
func (w *mergeWatch) fail(format string, a ...any) int {
	say(w.errOut, "agent-repld merge-queue: "+format+"\n", a...)
	return exitFailure
}

// callingRow finds the requesting workspace's row: the one whose worktree
// contains the caller's directory, the deepest when worktrees nest.
func callingRow(roster *frontendv1.WorkspaceRoster, cwd string) *frontendv1.RosterRow {
	var best *frontendv1.RosterRow
	bestLen := -1
	for _, row := range rosterwalk.AllRows(roster) {
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

// findRosterRow finds a workspace's row by its canonical directory, in every
// grouping the roster carries, nested (child) rows included: a child is a
// workspace in its own right, so it can ask for its own merge, which lands in
// its parent.
func findRosterRow(roster *frontendv1.WorkspaceRoster, dir string) *frontendv1.RosterRow {
	for _, row := range rosterwalk.AllRows(roster) {
		if canonicalDir(row.GetWorkspace().GetWorkspace().GetDir()) == dir {
			return row
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
	arm, ok := protoarm.Name(row.ProtoReflect(), "status")
	if !ok {
		return "unset"
	}
	return string(arm)
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
