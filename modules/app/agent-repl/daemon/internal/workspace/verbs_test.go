package workspace

import (
	"context"
	"errors"
	"go/ast"
	"go/parser"
	"go/token"
	"slices"
	"strings"
	"testing"

	conversationv1 "agentrepl/proto/conversation/v1"

	"agent-repld/internal/dlog"
	"agent-repld/internal/resolve/sidebar"
	"agent-repld/internal/sourcescan"
	"agent-repld/internal/wsm"
)

func TestNewRefusesEveryMissingCollaborator(t *testing.T) {
	// Arrange: a verb that silently skipped a missing collaborator would answer
	// success for work it never did, so construction refuses instead.
	full := func() Deps {
		f := newFixture(t)
		return f.mutable(t).deps
	}
	tests := []struct {
		name  string
		strip func(*Deps)
	}{
		{name: "no instance id", strip: func(d *Deps) { d.Instance = "" }},
		{name: "no state client", strip: func(d *Deps) { d.DB = nil }},
		{name: "no git client", strip: func(d *Deps) { d.Git = nil }},
		{name: "no create gate", strip: func(d *Deps) { d.Creates = nil }},
		{name: "no network state", strip: func(d *Deps) { d.Network = nil }},
		{name: "no held create reporters", strip: func(d *Deps) { d.CreateProgressFor = nil }},
		{name: "no daemon lifetime", strip: func(d *Deps) { d.Lifetime = nil }},
		{name: "no account resolver", strip: func(d *Deps) { d.Accounts = nil }},
		{name: "no GitHub owners", strip: func(d *Deps) { d.PullOwners = nil }},
		{name: "no prompt queue", strip: func(d *Deps) { d.Queue = nil }},
		{name: "no turn-stop queue", strip: func(d *Deps) { d.TurnStops = nil }},
		{name: "no merge orchestrator", strip: func(d *Deps) { d.Merge = nil }},
		{name: "no rollout controller", strip: func(d *Deps) { d.Rollout = nil }},
		{name: "no feed resolver", strip: func(d *Deps) { d.Feed = nil }},
		{name: "no footer resolver", strip: func(d *Deps) { d.Footer = nil }},
		{name: "no topbar resolver", strip: func(d *Deps) { d.Topbar = nil }},
		{name: "no sidebar resolver", strip: func(d *Deps) { d.Sidebar = nil }},
		{name: "no host relay", strip: func(d *Deps) { d.Host = nil }},
		{name: "no session fleet", strip: func(d *Deps) { d.Sessions = nil }},
		{name: "no shim resolver", strip: func(d *Deps) { d.Shim = nil }},
		{name: "no freeness probe", strip: func(d *Deps) { d.Freeness = nil }},
		{name: "no ownership probe", strip: func(d *Deps) { d.Ownership = nil }},
		{name: "no summary restorer", strip: func(d *Deps) { d.Summaries = nil }},
		{name: "no served-card store", strip: func(d *Deps) { d.Cards = nil }},
		{name: "no prompts directory", strip: func(d *Deps) { d.PromptsDir = "" }},
		{name: "no home directory", strip: func(d *Deps) { d.HomeDir = "" }},
		{name: "a relative home directory", strip: func(d *Deps) { d.HomeDir = "home/me" }},
		{name: "no log surfaces", strip: func(d *Deps) { d.Log = nil }},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange.
			deps := full()
			tt.strip(&deps)
			// Act.
			_, err := New(deps)
			// Assert.
			if err == nil {
				t.Fatalf("New(%s) = nil error, want a refusal", tt.name)
			}
		})
	}
}

func TestRepublishRegistryCarriesEveryDurableHalf(t *testing.T) {
	// Arrange.
	f := newFixture(t)
	f.workspace("w1", t.TempDir())
	f.db.tasks = []wsm.Task{{ID: "task-1", Title: "ship it"}}
	// A roster that cannot take the select's own answer has the select
	// install the whole registry, which is the read under test.
	f.sidebar.refuseSelection = true

	// Act.
	if err := f.verbs.Select(context.Background(), "w1", Switch{History: RecordSelection}); err != nil {
		t.Fatalf("Select: %v", err)
	}

	// Assert.
	registry := f.sidebar.registries[len(f.sidebar.registries)-1]
	switch {
	case len(registry.Workspaces) != 1:
		t.Fatalf("registry workspaces = %d, want one", len(registry.Workspaces))
	case len(registry.Repositories) != 1:
		t.Fatalf("registry repositories = %d, want one", len(registry.Repositories))
	case len(registry.Tasks) != 1:
		t.Fatalf("registry tasks = %d, want one", len(registry.Tasks))
	case registry.Current == nil || *registry.Current != "w1":
		t.Fatalf("registry current = %v, want w1", registry.Current)
	}
}

func TestRecordRunsTheVerbWhenTheWorkspaceSinkCannotBeResolved(t *testing.T) {
	// Arrange: a registered workspace whose directory cannot host a sink —
	// a scratch path, or a worktree that has been deleted.
	f := newFixture(t)
	f.workspace("w1", t.TempDir())
	f.log.workspaceErr = errors.New("the symlink target is gone")

	// Act.
	err := f.verbs.Select(context.Background(), "w1", Switch{History: RecordSelection})

	// Assert: resolving a sink is total, so the verb answers on its own terms.
	if err != nil {
		t.Fatalf("Select() = %v, want the verb to run against the central sink", err)
	}
}

func TestRecordNamesTheWorkspaceOnCentrallyRoutedRecords(t *testing.T) {
	// Arrange.
	f := newFixture(t)
	ws := f.workspace("w1", t.TempDir())
	f.log.workspaceErr = errors.New("the symlink target is gone")

	// Act.
	if err := f.verbs.Select(context.Background(), "w1", Switch{History: RecordSelection}); err != nil {
		t.Fatalf("Select: %v", err)
	}

	// Assert: the record still says which workspace it is about.
	for _, rec := range f.log.logger.Records() {
		if rec.Context[dlog.KeyUnroutableWorkspace] == ws.Dir {
			return
		}
	}
	t.Fatalf("no record named the unroutable workspace %q", ws.Dir)
}

func TestPromptSubmissionCarriesTheCreationOrigin(t *testing.T) {
	// Arrange. Act.
	sub := promptSubmission("w1", "t1", SaidText("hello"))

	// Assert.
	if sub.WS != "w1" || sub.Turn != "t1" || sub.Target != nil {
		t.Fatalf("submission = %+v, want the workspace and turn with no bubble target", sub)
	}
}

// TestARepublishLevelsACancelledRosterReadAtInfo pins the level of a roster
// read abandoned by the daemon's own exit. The serving context is cancelled
// under whatever is in flight, and a roster nobody is left to receive is no
// loss -- while every other refusal of the state client keeps its ERROR.
func TestARepublishLevelsACancelledRosterReadAtInfo(t *testing.T) {
	tests := []struct {
		name      string
		listErr   error
		wantLevel string
	}{
		{name: "the read was cancelled", listErr: context.Canceled, wantLevel: "info"},
		{name: "the state client refused", listErr: errors.New("the database is gone"), wantLevel: "error"},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange.
			f := newFixture(t)
			f.workspace("w1", t.TempDir())
			f.db.listWorkspacesErr = tt.listErr

			// Act: a priority change republishes the whole registry.
			if err := f.verbs.SetPriority(context.Background(), "w1", nil); err != nil {
				t.Fatalf("SetPriority: %v", err)
			}

			// Assert.
			var level string
			for _, r := range f.log.logger.Records() {
				if strings.Contains(r.Message, "subsections for the roster") ||
					strings.Contains(r.Message, "roster read ended") {
					level = r.Level
				}
			}
			if level != tt.wantLevel {
				t.Fatalf("the roster-read record is %q, want %q", level, tt.wantLevel)
			}
		})
	}
}

func TestRepublishRegistryLogsItsCompletionAtInfo(t *testing.T) {
	// Arrange: a verb whose only roster publish is republishRegistry's success.
	f := newFixture(t)
	f.workspace("w1", t.TempDir())

	// Act.
	if err := f.verbs.SetPriority(context.Background(), "w1", nil); err != nil {
		t.Fatalf("SetPriority: %v", err)
	}

	// Assert: a roster republish is a production-visible mutation, so its
	// completion stands at info rather than debug.
	var level string
	for _, r := range f.log.logger.Records() {
		if r.Message == "republished the roster registry" {
			level = r.Level
		}
	}
	if level != "info" {
		t.Fatalf("the roster republish completion is %q, want info", level)
	}
}

func TestDeferredBriefIsDeferred(t *testing.T) {
	// Arrange, Act.
	got := deferredBrief("w1", "the brief", conversationv1.PromptOrigin_PROMPT_ORIGIN_MERGE_REVERT)

	// Assert.
	if got.Delivery != wsm.DeliveryDeferred {
		t.Fatalf("delivery = %v, want deferred", got.Delivery)
	}
}

func TestDeferredBriefCarriesItsWorkspaceOriginAndText(t *testing.T) {
	// Arrange, Act.
	got := deferredBrief("w1", "the brief", conversationv1.PromptOrigin_PROMPT_ORIGIN_MERGE_REVERT)

	// Assert.
	if got.WS != "w1" || got.Origin != conversationv1.PromptOrigin_PROMPT_ORIGIN_MERGE_REVERT || submittedText(got.Said) != "the brief" {
		t.Fatalf("submission = %+v, want w1's MERGE_REVERT brief saying the brief", got)
	}
}

func TestDeferredBriefMintsAFreshTurnEachTime(t *testing.T) {
	// Arrange, Act.
	first := deferredBrief("w1", "a", conversationv1.PromptOrigin_PROMPT_ORIGIN_MERGE_REVERT)
	second := deferredBrief("w1", "a", conversationv1.PromptOrigin_PROMPT_ORIGIN_MERGE_REVERT)

	// Assert.
	if first.Turn == "" || first.Turn == second.Turn {
		t.Fatalf("turns = %q, %q, want two distinct minted turns", first.Turn, second.Turn)
	}
}

// TestOnlyDeferredBriefDefersASubmission pins deferredBrief as the one place
// this package composes a deferred submission: a verb hand-rolling one would
// be free to drift from it.
func TestOnlyDeferredBriefDefersASubmission(t *testing.T) {
	// Arrange, Act.
	got := functionsReferencing(t, "DeliveryDeferred")

	// Assert.
	if !slices.Equal(got, []string{"deferredBrief"}) {
		t.Fatalf("functions naming DeliveryDeferred = %v, want only deferredBrief", got)
	}
}

// functionsReferencing names, sorted and once each, every function of this
// package's production sources whose body names the identifier NAME, bare or
// as a selector's field.
func functionsReferencing(t *testing.T, name string) []string {
	t.Helper()
	files := token.NewFileSet()
	var found []string
	for _, file := range sourcescan.Production(t) {
		parsed, err := parser.ParseFile(files, file.Name, file.Source, 0)
		if err != nil {
			t.Fatalf("parse %s: %v", file.Name, err)
		}
		for _, decl := range parsed.Decls {
			fn, ok := decl.(*ast.FuncDecl)
			if !ok || fn.Body == nil {
				continue
			}
			ast.Inspect(fn.Body, func(node ast.Node) bool {
				if ident, ok := node.(*ast.Ident); ok && ident.Name == name && !slices.Contains(found, fn.Name.Name) {
					found = append(found, fn.Name.Name)
				}
				return true
			})
		}
	}
	slices.Sort(found)
	return found
}

func TestQueueBriefRecordsAndFailsWhatTheQueueDidNotTake(t *testing.T) {
	tests := []struct {
		name    string
		arrange func(f *fixture)
		message string
	}{
		{name: "the queue takes it"},
		{name: "the queue fails it", arrange: func(f *fixture) { f.queue.submitErr = errors.New("the queue is gone") },
			message: "the workspace's queue did not accept the brief"},
		{name: "the queue refuses it", arrange: func(f *fixture) { f.queue.submitRefusedArm = "workspace_closed" },
			message: "the workspace's queue refused the brief"},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			f := newFixture(t)
			if tt.arrange != nil {
				tt.arrange(f)
			}
			submission := deferredBrief("w1", "BRIEF", conversationv1.PromptOrigin_PROMPT_ORIGIN_SUBSECTION_PLACEMENT)
			const op = "daemon.workspace.test_brief"

			// Act
			disposition, err := f.mutable(t).queueBrief(context.Background(), f.log.logger, op, "the workspace's", "the brief", submission)

			// Assert
			if tt.message == "" {
				if err != nil || !disposition.Delivered || len(f.queue.submissions) != 1 {
					t.Fatalf("queueBrief = %+v, %v with %d submissions; want it delivered", disposition, err, len(f.queue.submissions))
				}
				return
			}
			if err == nil {
				t.Fatal("queueBrief succeeded for a brief the queue did not take")
			}
			record, ok := dlog.FindRecord(f.log.logger.Records(), dlog.RecordQuery{Level: dlog.LevelError, Operation: op, Message: tt.message})
			if !ok || record.Context["turn"] != string(submission.Turn) {
				t.Fatalf("record = %+v (found %v), want %q naming the brief's turn", record, ok, tt.message)
			}
		})
	}
}

func TestQueueDeferredBrief(t *testing.T) {
	tests := []struct {
		name    string
		arrange func(f *fixture)
		wantErr bool
	}{
		{name: "the queue takes it"},
		{name: "the queue fails it", arrange: func(f *fixture) { f.queue.submitErr = errors.New("the queue is gone") }, wantErr: true},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			f := newFixture(t)
			if tt.arrange != nil {
				tt.arrange(f)
			}

			// Act
			turn, _, err := f.mutable(t).queueDeferredBrief(context.Background(), f.log.logger, "daemon.workspace.test_brief", "w1", "BRIEF",
				conversationv1.PromptOrigin_PROMPT_ORIGIN_MERGE_REVERT, "the workspace's", "the brief")

			// Assert: a brief the queue did not take is the error, recorded
			// under the turn answered; one it took is the deferred brief.
			if (err != nil) != tt.wantErr {
				t.Fatalf("queueDeferredBrief error = %v, want error %v", err, tt.wantErr)
			}
			if tt.wantErr {
				record, ok := dlog.FindRecord(f.log.logger.Records(), dlog.RecordQuery{Level: dlog.LevelError,
					Operation: "daemon.workspace.test_brief", Message: "the workspace's queue did not accept the brief"})
				if !ok || record.Context["turn"] != string(turn) {
					t.Fatalf("record = %+v (found %v), want the failure naming turn %s", record, ok, turn)
				}
				return
			}
			if len(f.queue.submissions) != 1 {
				t.Fatalf("submissions = %d, want 1", len(f.queue.submissions))
			}
			got := f.queue.submissions[0]
			if got.WS != "w1" || got.Turn != turn || got.Delivery != wsm.DeliveryDeferred ||
				got.Origin != conversationv1.PromptOrigin_PROMPT_ORIGIN_MERGE_REVERT || submittedText(got.Said) != "BRIEF" {
				t.Errorf("submission = %+v under turn %s, want w1's deferred merge-revert BRIEF under it", got, turn)
			}
		})
	}
}

// TestEveryDeferredBriefIsComposedByQueueDeferredBrief pins the brief sites
// to queueDeferredBrief's shape: a site composing its own deferredBrief can
// drift from the queueing every other site shares.
func TestEveryDeferredBriefIsComposedByQueueDeferredBrief(t *testing.T) {
	// Act
	sites := sourcescan.Sites(t, "deferredBrief(")

	// Assert: the definition and queueDeferredBrief's call, both in verbs.go.
	if !slices.Equal(sites, []string{"verbs.go", "verbs.go"}) {
		t.Fatalf("deferredBrief sites = %v, want only its definition and queueDeferredBrief in verbs.go", sites)
	}
}

// TestEveryDeferredBriefIsQueuedThroughQueueBrief pins that the brief sites
// share queueBrief's shape: a file composing a deferred brief that hands it
// to the queue itself would take a refused brief for a delivered one.
func TestEveryDeferredBriefIsQueuedThroughQueueBrief(t *testing.T) {
	// Arrange / Act.
	var direct []string
	for _, file := range sourcescan.Production(t) {
		source := string(file.Source)
		if file.Name != "verbs.go" && strings.Contains(source, "deferredBrief(") && strings.Contains(source, "v.deps.Queue.Submit(") {
			direct = append(direct, file.Name)
		}
	}

	// Assert.
	if len(direct) != 0 {
		t.Fatalf("files queuing a deferred brief outside queueBrief = %v, want none", direct)
	}
}

// TestARegistryReadASelectOvertookIsReadAgainBeforeItIsInstalled pins
// installRegistry's guard: a select that lands while a republish reads the
// registry makes that read stale, so it is never installed; the registry is
// read again, and the roster takes the one carrying the select.
func TestARegistryReadASelectOvertookIsReadAgainBeforeItIsInstalled(t *testing.T) {
	// Arrange: the republish's first read lets a select land mid-read.
	f := newFixture(t)
	f.workspace("w1", t.TempDir())
	f.workspace("w2", t.TempDir())
	v := f.verbs.(*verbs)
	reads := 0
	f.db.onReadArrangement = func() {
		reads++
		if reads != 1 {
			return
		}
		if err := v.Select(context.Background(), "w2", Switch{History: RecordSelection}); err != nil {
			t.Fatalf("Select: %v", err)
		}
	}

	// Act: a priority change republishes the whole registry.
	if err := v.SetPriority(context.Background(), "w1", nil); err != nil {
		t.Fatalf("SetPriority: %v", err)
	}

	// Assert.
	if reads != 2 {
		t.Fatalf("registry reads = %d, want 2: the overtaken read and the one installed", reads)
	}
	if len(f.sidebar.registries) != 1 {
		t.Fatalf("registries installed = %d, want exactly one", len(f.sidebar.registries))
	}
	if got := f.sidebar.registries[0].Current; got == nil || *got != "w2" {
		t.Fatalf("installed current = %v, want w2, the select that overtook the first read", got)
	}
	if _, ok := dlog.FindRecord(f.log.logger.Records(), dlog.RecordQuery{Level: "debug", Operation: opSetPriority, Message: "a select landed while the registry was read, so the read may predate its selection; it is taken again"}); !ok {
		t.Fatalf("records = %v, want the overtaken read recorded", f.log.logger.Records())
	}
}

// TestARegistryInstallReturnsAFailedRead pins the install's error path: a
// read that fails installs nothing, is recorded by readRegistry, and is
// returned to the caller.
func TestARegistryInstallReturnsAFailedRead(t *testing.T) {
	// Arrange.
	f := newFixture(t)
	f.workspace("w1", t.TempDir())
	f.db.readArrangementErr = errors.New("the database is gone")
	v := f.verbs.(*verbs)
	installed := false

	// Act.
	err := v.installRegistry(context.Background(), f.log.logger, "daemon.workspace.test", func(sidebar.Registry) { installed = true })

	// Assert.
	if err == nil || !strings.Contains(err.Error(), "the database is gone") {
		t.Fatalf("installRegistry = %v, want the read's failure", err)
	}
	if installed {
		t.Fatal("a failed read was installed")
	}
	if _, ok := dlog.FindRecord(f.log.logger.Records(), dlog.RecordQuery{Level: "error", Operation: "daemon.workspace.test", Message: "could not read the workspaces, repositories and subsections for the roster"}); !ok {
		t.Fatalf("records = %v, want the failed read at error", f.log.logger.Records())
	}
}
