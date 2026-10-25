package merge

import (
	"context"
	"errors"
	"strings"
	"testing"

	"agent-repld/internal/dlog"
	"agent-repld/internal/refusalarm"
	"agent-repld/internal/semver"
	"agent-repld/internal/wsm"
)

// aPatch is a well-formed increment.
func aPatch() *wsm.VersionIncrement {
	return &wsm.VersionIncrement{Part: semver.Patch, ChangelogLine: "Fix the footer"}
}

// withVersion is ownBranch carrying v.
func withVersion(v *wsm.VersionIncrement) wsm.MergeSource {
	source := ownBranch
	source.Version = v
	return source
}

func TestAVersionIncrementIsAcceptedExactlyWhereItBelongs(t *testing.T) {
	tests := []struct {
		name      string
		versioned bool
		by        Requester
		version   *wsm.VersionIncrement
	}{
		{name: "an agent's deploy onto a versioned repository's main with one", versioned: true, by: RequestedByAgent, version: aPatch()},
		{name: "the user's deploy onto a versioned repository's main without one", versioned: true, by: RequestedByUser},
		{name: "the user's deploy onto a versioned repository's main with one", versioned: true, by: RequestedByUser, version: aPatch()},
		{name: "an agent's deploy into an unversioned repository without one", versioned: false, by: RequestedByAgent},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			h := newHarness(t)
			if tt.versioned {
				h.emacsRepo()
			}

			// Act
			err := h.request(t, withVersion(tt.version), tt.by)

			// Assert
			if err != nil {
				t.Fatalf("Enqueue = %v, want accepted", err)
			}
			if _, recorded := h.entryState(); !recorded {
				t.Fatal("the request was not recorded")
			}
		})
	}
}

func TestAnAgentsDeployOntoAVersionedMainWithoutAnIncrementIsRefusedAndRecordsNothing(t *testing.T) {
	// Arrange
	h := newHarness(t)
	h.emacsRepo()

	// Act
	err := h.request(t, ownBranch, RequestedByAgent)

	// Assert
	refusal, refused := Refused(err)
	if !refused || refusal.Arm != ArmVersionIncrementRequired {
		t.Fatalf("Enqueue = %v, want the %s refusal", err, ArmVersionIncrementRequired)
	}
	if _, recorded := h.entryState(); recorded {
		t.Fatal("a refused request was recorded")
	}
	if _, logged := h.recordFor(dlog.LevelInfo, "daemon.merge.enqueue"); !logged {
		t.Fatal("the refusal was not recorded in the merge log")
	}
}

func TestAnIncrementOnADeployOffAVersionedMainIsRefusedWithItsReason(t *testing.T) {
	tests := []struct {
		name    string
		arrange func(h *harness)
		source  wsm.MergeSource
		want    refusalarm.Nested
	}{
		{
			name:    "an unversioned repository",
			arrange: func(h *harness) {},
			source:  withVersion(aPatch()),
			want:    reasonUnversionedRepository,
		},
		{
			name: "a parent workspace's checkout",
			arrange: func(h *harness) {
				h.emacsRepo()
				h.git.mainWorktrees = map[string]string{h.targetD: t.TempDir()}
			},
			source: withVersion(aPatch()),
			want:   reasonNotDefaultBranch,
		},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			h := newHarness(t)
			tt.arrange(h)

			// Act
			err := h.request(t, tt.source, RequestedByAgent)

			// Assert
			refusal, refused := Refused(err)
			if !refused || refusal.Arm != ArmVersionIncrementUnexpected || refusal.Fields["reason"] != tt.want {
				t.Fatalf("Enqueue = %v (fields %v), want %s with reason %s", err, refusal, ArmVersionIncrementUnexpected, tt.want)
			}
			if _, recorded := h.entryState(); recorded {
				t.Fatal("a refused request was recorded")
			}
		})
	}
}

func TestAnUnreadableRepositoryFailsTheRequestLoudlyAndRecordsNothing(t *testing.T) {
	// Arrange
	h := newHarness(t)
	h.git.mainWorktreeErr = errors.New("git exploded")

	// Act
	err := h.request(t, withVersion(aPatch()), RequestedByAgent)

	// Assert
	if err == nil {
		t.Fatal("Enqueue = nil, want the main worktree lookup's failure")
	}
	if _, refused := Refused(err); refused {
		t.Fatalf("Enqueue = %v, want a failure, not a refusal", err)
	}
	if _, recorded := h.entryState(); recorded {
		t.Fatal("a failed request was recorded")
	}
	record, logged := h.recordFor(dlog.LevelError, "daemon.merge.enqueue")
	if !logged || record.Context["error"] == nil {
		t.Fatalf("records = %+v, want an ERROR enqueue record with the cause", h.logs.Records())
	}
}

// landWithVersion runs one agent's merge onto agent-repl's master carrying v
// to its end, and answers the error the admission ended on.
func landWithVersion(t *testing.T, h *harness, v *wsm.VersionIncrement) error {
	t.Helper()
	h.emacsRepo()
	h.landsCleanly("abc123def4567")
	h.git.changed = []string{"modules/app/agent-repl/daemon/x.go"}
	h.gatePasses("daemon")
	if err := h.request(t, withVersion(v), RequestedByAgent); err != nil {
		t.Fatalf("Enqueue: %v", err)
	}
	h.runWait(t)
	return h.admit(context.Background())
}

func TestALandingTagsTheNextVersionWithItsChangelogLine(t *testing.T) {
	tests := []struct {
		name    string
		version *wsm.VersionIncrement
		want    string
	}{
		{name: "a patch", version: &wsm.VersionIncrement{Part: semver.Patch, ChangelogLine: "Fix the footer"}, want: "0.4.8@abc123def4567: Fix the footer"},
		{name: "a minor", version: &wsm.VersionIncrement{Part: semver.Minor, ChangelogLine: "Merges record a version"}, want: "0.5.0@abc123def4567: Merges record a version"},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange: the highest version reachable is 0.4.7, among other tags.
			h := newHarness(t)
			h.git.tagsMerged = []string{"0.0.1", "0.4.7", "start/x", "v9.9.9"}

			// Act
			err := landWithVersion(t, h, tt.version)

			// Assert
			if err != nil {
				t.Fatalf("the merge failed: %v", err)
			}
			if len(h.git.createdTags) != 1 || h.git.createdTags[0] != tt.want {
				t.Fatalf("tags created = %q, want %q", h.git.createdTags, tt.want)
			}
		})
	}
}

func TestALandedHeadDrawsTheVersionWithTheIncrementedNumberApart(t *testing.T) {
	tests := []struct {
		name                           string
		part                           semver.Part
		leading, incremented, trailing string
	}{
		{name: "a minor", part: semver.Minor, leading: "0.", incremented: "5", trailing: ".0"},
		{name: "a patch", part: semver.Patch, leading: "0.4.", incremented: "8", trailing: ""},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			h := newHarness(t)
			h.git.tagsMerged = []string{"0.4.7"}

			// Act
			err := landWithVersion(t, h, &wsm.VersionIncrement{Part: tt.part, ChangelogLine: "x"})

			// Assert
			if err != nil {
				t.Fatalf("the merge failed: %v", err)
			}
			head := h.feed.lastHead()
			if head == nil {
				t.Fatal("no head was drawn")
			}
			merge := head.Row.GetActivity().GetMerge()
			v := merge.GetHead().GetVersion()
			if merge.GetSuccess() == nil || v == nil {
				t.Fatalf("head = %+v, want a landed head with a version", merge)
			}
			if v.GetLeading().GetText() != tt.leading || v.GetIncremented().GetText() != tt.incremented || v.GetTrailing().GetText() != tt.trailing {
				t.Fatalf("version = %q %q %q, want %q %q %q", v.GetLeading().GetText(), v.GetIncremented().GetText(), v.GetTrailing().GetText(), tt.leading, tt.incremented, tt.trailing)
			}
			if (v.Trailing == nil) != (tt.trailing == "") {
				t.Fatalf("trailing = %+v, want it present exactly when there is text after the number", v.Trailing)
			}
		})
	}
}

func TestALandingWhoseMergeCommitIsAlreadyTaggedTagsNothingAgain(t *testing.T) {
	// Arrange: a restart found the step finished: the merge commit carries
	// its version.
	h := newHarness(t)
	h.git.tagsMerged = []string{"0.4.7", "0.4.8"}
	h.git.tagsAt = map[string][]string{"abc123def4567": {"0.4.8"}}

	// Act
	err := landWithVersion(t, h, aPatch())

	// Assert
	if err != nil {
		t.Fatalf("the merge failed: %v", err)
	}
	if len(h.git.createdTags) != 0 {
		t.Fatalf("tags created = %q, want none", h.git.createdTags)
	}
	if v := h.feed.lastHead().Row.GetActivity().GetMerge().GetHead().GetVersion(); v.GetIncremented().GetText() != "8" {
		t.Fatalf("version = %+v, want the existing 0.4.8 drawn", v)
	}
}

func TestATagThatCannotBeMadeLeavesTheMergeLandedAndLogsTheFault(t *testing.T) {
	tests := []struct {
		name    string
		arrange func(h *harness)
		message string
	}{
		{
			name:    "no version is reachable",
			arrange: func(h *harness) { h.git.tagsMerged = []string{"start/x"} },
			message: "no version tag is reachable from the tip the merge landed on, so there is no version to increment; tag master by hand",
		},
		{
			name:    "the reachable tags cannot be read",
			arrange: func(h *harness) { h.git.tagsMergedErr = errors.New("git exploded") },
			message: "could not read the version tags reachable from the tip the merge landed on; it is drawn without a version",
		},
		{
			name: "git refuses the tag",
			arrange: func(h *harness) {
				h.git.tagsMerged = []string{"0.4.7"}
				h.git.createTagErr = errors.New("fatal: tag '0.4.8' already exists")
			},
			message: "could not create the landed merge's version tag; it is drawn without a version",
		},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			h := newHarness(t)
			tt.arrange(h)

			// Act
			err := landWithVersion(t, h, aPatch())

			// Assert
			if err != nil {
				t.Fatalf("the merge failed: %v", err)
			}
			merge := h.feed.lastHead().Row.GetActivity().GetMerge()
			if merge.GetSuccess() == nil || merge.GetHead().GetVersion() != nil {
				t.Fatalf("head = %+v, want landed with no version", merge)
			}
			record, logged := h.recordFor(dlog.LevelError, "daemon.merge.version")
			if !logged || record.Message != tt.message || record.Context["commit"] != "abc123def4567" || record.Context["part"] != "patch" {
				t.Fatalf("records = %+v, want the version fault with its context", h.logs.Records())
			}
		})
	}
}

func TestTheDaemonsExitAfterTheFastForwardStopsTheStepBeforeItsLandingIsRecorded(t *testing.T) {
	// Arrange: the landing's fast-forward is in flight when the daemon exits;
	// the gate lets it finish and refuses the tagging git after it.
	h := newHarness(t)
	h.emacsRepo()
	h.landsCleanly("abc123def4567")
	h.gatePasses("daemon")
	h.git.tagsMerged = []string{"0.4.7"}
	entered, release := h.git.hold("fast_forward")
	if err := h.request(t, withVersion(aPatch()), RequestedByUser); err != nil {
		t.Fatalf("Enqueue: %v", err)
	}
	done := admitAsync(h, context.Background())
	<-entered
	h.o.onGitStopWait = func() { close(release) }

	// Act
	h.o.Drain(context.Background())
	<-done

	// Assert: no tag, no landed head, no fault -- the next boot tags it.
	if len(h.git.createdTags) != 0 {
		t.Fatalf("tags created = %q, want none after the exit", h.git.createdTags)
	}
	if head := h.feed.lastHead(); head != nil && head.Row.GetActivity().GetMerge().GetSuccess() != nil {
		t.Fatal("the landing was concluded although its tagging was cut off")
	}
	if _, logged := h.recordFor(dlog.LevelError, "daemon.merge.version"); logged {
		t.Fatal("the daemon's exit was recorded as a version fault")
	}
	if _, logged := h.recordFor(dlog.LevelInfo, "daemon.merge.version"); !logged {
		t.Fatal("the stop was not recorded")
	}
}

func TestAMergeResumedAfterItsFastForwardTagsTheVersionTheExitCutOff(t *testing.T) {
	// Arrange: the target stands at the recorded merge commit, untagged.
	h := newHarness(t)
	interruptedAt(t, h, atCommit)
	h.db.mu.Lock()
	h.db.queues[h.repoKey()][0].Source.Version = aPatch()
	h.db.mu.Unlock()
	h.git.refs["HEAD"] = mergeSHA
	h.git.tagsMerged = []string{"0.4.7"}

	// Act
	resume(t, h)

	// Assert
	if want := "0.4.8@" + mergeSHA + ": Fix the footer"; len(h.git.createdTags) != 1 || h.git.createdTags[0] != want {
		t.Fatalf("tags created = %q, want %q", h.git.createdTags, want)
	}
	if v := h.feed.lastHead().Row.GetActivity().GetMerge().GetHead().GetVersion(); v.GetIncremented().GetText() != "8" {
		t.Fatalf("version = %+v, want 0.4.8 drawn", v)
	}
}

func TestALandedVersionRoundTripsThroughTheRecord(t *testing.T) {
	tests := []struct {
		name string
		in   outcome
	}{
		{name: "with a version", in: outcome{landed: "abc", target: "master", version: &landedVersion{tag: "0.5.0", part: semver.Minor}}},
		{name: "without one", in: outcome{landed: "abc", target: "master"}},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Act
			got, err := toOutcomeDoc(tt.in).outcome()

			// Assert
			if err != nil || got.landed != tt.in.landed || got.target != tt.in.target || (got.version == nil) != (tt.in.version == nil) ||
				(got.version != nil && *got.version != *tt.in.version) {
				t.Fatalf("outcome = %+v, %v; want %+v", got, err, tt.in)
			}
		})
	}
}

func TestARecordedLandingThatNamesNoTargetIsRefused(t *testing.T) {
	// Arrange: a landing recorded before the record carried its target.
	doc := &outcomeDoc{Landed: "abc"}

	// Act
	got, err := doc.outcome()

	// Assert
	if err == nil || !strings.Contains(err.Error(), "names no target branch") || got.landed != "" {
		t.Fatalf("outcome = %+v, %v; want it refused for naming no target branch", got, err)
	}
}

func TestARecordedVersionThatDoesNotParseIsRefused(t *testing.T) {
	tests := []struct {
		name string
		doc  versionDoc
	}{
		{name: "an unknown part", doc: versionDoc{Tag: "0.5.0", Part: "major"}},
		{name: "a tag that is no version", doc: versionDoc{Tag: "v0.5.0", Part: "minor"}},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			doc := &outcomeDoc{Landed: "abc", Version: &tt.doc}

			// Act
			_, err := doc.outcome()

			// Assert
			if err == nil {
				t.Fatal("outcome = nil error, want the bad version refused")
			}
		})
	}
}

func TestHeadVersionIsAbsentWithoutALandedVersion(t *testing.T) {
	// Act
	got := headVersion(nil)

	// Assert
	if got != nil {
		t.Fatalf("headVersion(nil) = %+v, want nil", got)
	}
}

func TestAnAsyncMergeFromTheMainCheckoutOntoAgentReplsMasterNeedsAnIncrement(t *testing.T) {
	tests := []struct {
		name    string
		version *wsm.VersionIncrement
		refused bool
	}{
		{name: "without one", refused: true},
		{name: "with one", version: aPatch()},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange: the main checkout, which the daemon never created.
			h := newHarness(t)
			h.emacsRepo()
			h.db.mu.Lock()
			delete(h.db.jobs, theWorkspace)
			h.db.mu.Unlock()
			h.inFlight = "turn-asking"
			source := asyncSource
			source.Version = tt.version

			// Act
			_, err := h.o.Enqueue(context.Background(), Request{Workspace: theWorkspace, Source: source, By: RequestedByAgent, Asker: theWorkspace,
				Detached: &DetachedRun{Maker: theWorkspace, Name: "n"}})

			// Assert
			refusal, refused := Refused(err)
			if refused != tt.refused || (refused && refusal.Arm != ArmVersionIncrementRequired) || (!tt.refused && err != nil) {
				t.Fatalf("Enqueue = %v, want refused %v", err, tt.refused)
			}
		})
	}
}

func TestAnAsyncRequestLandsWhereItsRequestNamed(t *testing.T) {
	tests := []struct {
		name       string
		intoMaker  bool
		wantDir    func(h *harness) string
		wantBranch func(h *harness) string
	}{
		{name: "onto the primary branch in the main worktree",
			wantDir: func(h *harness) string { return h.db.workspaces[theWorkspace].Dir }, wantBranch: func(h *harness) string { return h.git.defaultBranch }},
		{name: "into the maker's branch in its worktree", intoMaker: true,
			wantDir:    func(h *harness) string { return h.db.workspaces[otherWorkspace].Dir },
			wantBranch: func(h *harness) string { return h.db.workspaces[otherWorkspace].Branch }},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			h := newHarness(t)
			h.registerOther(otherWorkspace, "ws-two", "maker-branch")
			source := wsm.MergeSource{DetachMaker: otherWorkspace, DetachTargetMaker: tt.intoMaker}

			// Act
			dir, branch, err := h.o.detachedLanding(context.Background(), h.db.workspaces[theWorkspace], source)

			// Assert
			if err != nil || dir != tt.wantDir(h) || branch != tt.wantBranch(h) {
				t.Fatalf("detachedLanding = %q, %q, %v; want %q, %q", dir, branch, err, tt.wantDir(h), tt.wantBranch(h))
			}
		})
	}
}

func TestAnAsyncLandingIntoAMakerTheRegistryLostFails(t *testing.T) {
	// Arrange
	h := newHarness(t)
	source := wsm.MergeSource{DetachMaker: "ws-gone", DetachTargetMaker: true}

	// Act
	dir, _, err := h.o.detachedLanding(context.Background(), h.db.workspaces[theWorkspace], source)

	// Assert
	if err == nil || dir != "" {
		t.Fatalf("detachedLanding = %q, %v; want the maker's read failure", dir, err)
	}
}
