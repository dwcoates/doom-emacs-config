package daemonrules

import (
	"go/ast"
	"go/parser"
	"go/token"
	"io/fs"
	"os"
	"path/filepath"
	"reflect"
	"regexp"
	"slices"
	"sort"
	"strings"
	"testing"

	"agent-repld/reposcans/internal/sourcescan"
)

// internalWorkspaceDir is the package these rules hold, under the daemon module's root.
const internalWorkspaceDir = "internal/workspace"

// TestARefusedColdGateStandsAgainInOneHome fails when a verb stands a spent
// gate again by hand instead of through reraiseRefusedColdGate (a refused
// answer) or coldGateSettled (a failed re-open).
func TestARefusedColdGateStandsAgainInOneHome(t *testing.T) {
	sourcescan.InPackage(t, internalWorkspaceDir)
	// Arrange
	files := sourcescan.Production(t, internalWorkspaceDir)

	// Act
	sites := 0
	for _, file := range files {
		n := strings.Count(string(file.Source), "v.deps.Cards.ReraiseColdGate(")
		if n > 0 && file.Name != "answers.go" {
			t.Fatalf("%s stands a cold gate again by hand", file.Name)
		}
		sites += n
	}

	// Assert
	if sites != 2 {
		t.Fatalf("ReraiseColdGate is called at %d sites, want reraiseRefusedColdGate and coldGateSettled", sites)
	}
}

// TestEveryCreateSpecBuilderGoesThroughNewCreateSpec pins the one way a
// production spec is built: no source of the daemon composes a CreateSpec
// literal with fields (an empty one is only a zero value beside an error),
// and every known builder calls NewCreateSpec.
func TestEveryCreateSpecBuilderGoesThroughNewCreateSpec(t *testing.T) {
	sourcescan.InPackage(t, internalWorkspaceDir)
	// Arrange
	want := []string{
		"internal/server/workspaces.go:CreateWorkspace",
		"internal/workspace/commandsupport.go:RequestCommandSupport",
		"internal/workspace/create.go:CreateMergeWorkspace",
		"internal/workspace/deploychecks.go:mediatorSpec",
		"internal/workspace/intake.go:CreateIntake",
		"internal/workspace/pendingcreates.go:decodeHeldSpec",
		"internal/workspace/productionchildren.go:productionChildSpec",
	}

	// Act
	literals, callers := createSpecBuilders(t)

	// Assert
	if len(literals) > 0 {
		t.Fatalf("CreateSpec literals with fields outside NewCreateSpec: %v", literals)
	}
	if strings.Join(callers, "\n") != strings.Join(want, "\n") {
		t.Fatalf("NewCreateSpec callers = %v, want %v", callers, want)
	}
}

// createSpecBuilders scans every production source of the daemon for
// CreateSpec literals with fields outside NewCreateSpec, and for the
// functions that call NewCreateSpec, each as path:function.
func createSpecBuilders(t *testing.T) (literals, callers []string) {
	t.Helper()
	root := filepath.Join("..", "..")
	err := filepath.WalkDir(root, func(path string, d fs.DirEntry, err error) error {
		if err != nil {
			return err
		}
		if d.IsDir() {
			if d.Name() == "testdata" || d.Name() == "node_modules" {
				return filepath.SkipDir
			}
			return nil
		}
		if !strings.HasSuffix(path, ".go") || strings.HasSuffix(path, "_test.go") {
			return nil
		}
		file, err := parser.ParseFile(token.NewFileSet(), path, nil, 0)
		if err != nil {
			return err
		}
		rel, err := filepath.Rel(root, path)
		if err != nil {
			return err
		}
		for _, decl := range file.Decls {
			fn, ok := decl.(*ast.FuncDecl)
			if !ok || fn.Body == nil {
				continue
			}
			site := filepath.ToSlash(rel) + ":" + fn.Name.Name
			calls := false
			ast.Inspect(fn.Body, func(n ast.Node) bool {
				switch n := n.(type) {
				case *ast.CompositeLit:
					if isCreateSpecName(n.Type) && len(n.Elts) > 0 && fn.Name.Name != "NewCreateSpec" {
						literals = append(literals, site)
					}
				case *ast.CallExpr:
					if isNewCreateSpecName(n.Fun) {
						calls = true
					}
				}
				return true
			})
			if calls {
				callers = append(callers, site)
			}
		}
		return nil
	})
	if err != nil {
		t.Fatalf("scan the daemon's sources: %v", err)
	}
	sort.Strings(callers)
	return literals, callers
}

// isCreateSpecName reports whether EXPR names CreateSpec, bare or qualified.
func isCreateSpecName(expr ast.Expr) bool {
	return lastName(expr) == "CreateSpec"
}

// isNewCreateSpecName reports whether EXPR names NewCreateSpec, bare or
// qualified.
func isNewCreateSpecName(expr ast.Expr) bool {
	return lastName(expr) == "NewCreateSpec"
}

func lastName(expr ast.Expr) string {
	switch e := expr.(type) {
	case *ast.Ident:
		return e.Name
	case *ast.SelectorExpr:
		return e.Sel.Name
	}
	return ""
}

// TestEveryPromptsFaultIsRaisedThroughComposeBrief pins that the brief sites
// share composeBrief's shape: a hand-rolled load-splice-raise in any other file
// would drift from it (another log message, a fault left open), so raising the
// prompts fault anywhere but the helper fails here.
func TestEveryPromptsFaultIsRaisedThroughComposeBrief(t *testing.T) {
	sourcescan.InPackage(t, internalWorkspaceDir)
	// Arrange / Act.
	var raisers []string
	for _, file := range sourcescan.Production(t, internalWorkspaceDir) {
		if file.Name != "briefs.go" && strings.Contains(string(file.Source), "v.raisePromptsFault(") {
			raisers = append(raisers, file.Name)
		}
	}

	// Assert.
	if len(raisers) != 0 {
		t.Fatalf("files raising the prompts fault outside composeBrief = %v, want none", raisers)
	}
}

// TestNoVerbReadsTheClockAroundItsNilDefault pins that every verb reads the
// clock through v.now(), which falls back to time.Now when Deps.Now is nil
// as production leaves it: a direct v.deps.Now() call panicked the daemon on
// every deploy check (2026-11-17 17:35, releaseMergedHolder).
func TestNoVerbReadsTheClockAroundItsNilDefault(t *testing.T) {
	sourcescan.InPackage(t, internalWorkspaceDir)
	// Act.
	var offenders []string
	for _, file := range sourcescan.Production(t, internalWorkspaceDir) {
		if strings.Contains(string(file.Source), "v.deps.Now()") {
			offenders = append(offenders, file.Name)
		}
	}

	// Assert.
	if len(offenders) != 0 {
		t.Fatalf("files calling v.deps.Now() directly = %v, want every read through v.now()", offenders)
	}
}

// TestNoCreatePathAsksAboutAncestry pins that a create never asks git whether
// a base lies on the primary branch: a workspace is always cut from its
// source's tip. The package's one ancestry question -- whether a reopened
// merged workspace's branch lies inside its parent (advanceMerged, open.go) --
// is pinned to its own file, so no create path grows one unseen.
func TestNoCreatePathAsksAboutAncestry(t *testing.T) {
	sourcescan.InPackage(t, internalWorkspaceDir)
	// Act.
	perFile := map[string]int{}
	for _, file := range sourcescan.Production(t, internalWorkspaceDir) {
		if n := strings.Count(string(file.Source), "Git.IsAncestor("); n > 0 {
			perFile[file.Name] = n
		}
	}

	// Assert: advanceMerged's call alone; no create path asks it.
	want := map[string]int{"open.go": 1}
	if !reflect.DeepEqual(perFile, want) {
		t.Fatalf("Git.IsAncestor( calls per file = %v, want %v", perFile, want)
	}
}

// TestEveryDaemonFaultRaiserSharesRaiseDaemonFault pins that a fault the verbs
// raise once per condition goes through the one helper: a hand-rolled
// read-then-open would drift from it (another dedupe, a record left out).
func TestEveryDaemonFaultRaiserSharesRaiseDaemonFault(t *testing.T) {
	sourcescan.InPackage(t, internalWorkspaceDir)
	// Arrange / Act
	shared := sourcescan.Count(t, internalWorkspaceDir, "v.raiseDaemonFault(")
	var byHand []string
	for _, file := range sourcescan.Production(t, internalWorkspaceDir) {
		if file.Name != "daemonfault.go" && strings.Contains(string(file.Source), "Health.OpenFaults(ctx, wsm.FaultScope{Kind:") {
			byHand = append(byHand, file.Name)
		}
	}

	// Assert
	if shared != 2 || len(byHand) != 0 {
		t.Fatalf("shared raisers = %d, hand-rolled in %v; want 2 and none", shared, byHand)
	}
}

func TestEveryAdvanceToAParentsTipGoesThroughOneHelper(t *testing.T) {
	sourcescan.InPackage(t, internalWorkspaceDir)
	// Arrange
	sources := map[string]string{}
	for _, file := range []string{"open.go", "deploychecks.go"} {
		body, err := os.ReadFile(file)
		if err != nil {
			t.Fatal(err)
		}
		sources[file] = string(body)
	}

	// Assert: both callers share advanceToParent, and only it fast-forwards.
	for file, src := range sources {
		if !strings.Contains(src, "v.advanceToParent(") {
			t.Fatalf("%s does not advance through advanceToParent", file)
		}
	}
	if strings.Count(sources["open.go"]+sources["deploychecks.go"], "Git.FastForward(") != 1 {
		t.Fatal("a second fast-forward to a parent's tip was written beside advanceToParent")
	}
}

func TestEveryFoldThatHidesTabsGoesThroughFoldTabs(t *testing.T) {
	sourcescan.InPackage(t, internalWorkspaceDir)
	// Act.
	specs := sourcescan.Count(t, internalWorkspaceDir, "foldTabsSpec{")
	calls := sourcescan.Count(t, internalWorkspaceDir, "v.foldTabs(ctx, log, ")
	byHand := sourcescan.Count(t, internalWorkspaceDir, "v.deps.StandDowns.StandDownFolded(ws.ID)")

	// Assert.
	if specs != 3 || calls != 3 || byHand != 1 {
		t.Fatalf("fold specs = %d, foldTabs calls = %d, stand-down loops = %d; want the repository's, the subsection's and the children's spec, each fold through foldTabs",
			specs, calls, byHand)
	}
}

// TestFoldTabsMembershipIsTheSharedOne pins every fold that hides tabs to
// wsm.FoldMembers: a hand-rolled membership closure would let the sessions a
// collapse stands down drift from the ones the roster and the refusal count.
func TestFoldTabsMembershipIsTheSharedOne(t *testing.T) {
	sourcescan.InPackage(t, internalWorkspaceDir)
	// Act
	shared := sourcescan.Count(t, internalWorkspaceDir, "return wsm.RepositoryMembers(") +
		sourcescan.Count(t, internalWorkspaceDir, "return wsm.SubsectionMembers(") + sourcescan.Count(t, internalWorkspaceDir, "return wsm.DescendantMembers(")
	byHand := sourcescan.Count(t, internalWorkspaceDir, "func(ws wsm.Workspace) bool { return ws.Repo ==") +
		sourcescan.Count(t, internalWorkspaceDir, "func(ws wsm.Workspace) bool { return ws.Subsection")

	// Assert
	if shared != 3 || byHand != 0 {
		t.Fatalf("shared memberships = %d, hand-rolled = %d; want 3 and 0", shared, byHand)
	}
}

// TestEveryReapedCheckGoesThroughHeld pins that the fleet reads a client's
// reaping only through live.held, so a hand-rolled check cannot drift from it.
func TestEveryReapedCheckGoesThroughHeld(t *testing.T) {
	sourcescan.InPackage(t, internalWorkspaceDir)
	// Arrange, Act.
	var sites []string
	for _, file := range sourcescan.Production(t, internalWorkspaceDir) {
		for _, line := range strings.Split(string(file.Source), "\n") {
			if strings.Contains(line, "client.Reaped()") {
				sites = append(sites, file.Name+":"+strings.TrimSpace(line))
			}
		}
	}

	// Assert.
	if len(sites) != 1 || sites[0] != "sessions.go:_, reaped := l.client.Reaped()" {
		t.Fatalf("reaped checks outside live.held: %v", sites)
	}
}

// TestTheDaemonPostsToSlackOnlyWhenAnAgentAsks pins how far the daemon
// speaks in Slack. What a thread is told of a workspace's LIFECYCLE is its
// repository's hooks' to say, never the daemon's. The daemon posts only the
// message an agent hands it for a thread its workspace follows
// (agentrepl.v1.PostToSlackThread), so:
//   - every Slack command line is spelled in internal/gnsclient alone, the
//     daemon's one way to Slack;
//   - gnsclient's Post is called from slackwatch's post verb alone.
func TestTheDaemonPostsToSlackOnlyWhenAnAgentAsks(t *testing.T) {
	sourcescan.InPackage(t, internalWorkspaceDir)
	// Arrange
	daemon, err := filepath.Abs(filepath.Join("..", ".."))
	if err != nil {
		t.Fatal(err)
	}
	gnsclient := filepath.Join(daemon, "internal", "gnsclient")
	postVerb := filepath.Join(daemon, "internal", "slackwatch", "verbs.go")
	var spelling, posting []string

	// Act
	err = filepath.WalkDir(daemon, func(path string, d os.DirEntry, err error) error {
		if err != nil {
			return err
		}
		if d.IsDir() || !strings.HasSuffix(path, ".go") || strings.HasSuffix(path, "_test.go") {
			return nil
		}
		raw, err := os.ReadFile(path)
		if err != nil {
			return err
		}
		source := string(raw)
		if filepath.Dir(path) != gnsclient && !strings.Contains(path, filepath.Join("internal", "lifecycleconfig")) {
			for _, verb := range []string{`"slack", "send"`, `"slack", "react"`, "slack send", "slack react"} {
				if strings.Contains(source, verb) {
					spelling = append(spelling, path)
					break
				}
			}
		}
		if path != postVerb && strings.Contains(source, "GNS.Post(") {
			posting = append(posting, path)
		}
		return nil
	})

	// Assert
	if err != nil {
		t.Fatal(err)
	}
	if len(spelling) != 0 {
		t.Errorf("a Slack command line is spelled in %v, want only in internal/gnsclient", spelling)
	}
	if len(posting) != 0 {
		t.Errorf("Slack is posted to from %v, want only slackwatch's post verb", posting)
	}
}

// TestAMissingDirectoryIsClosedOnlyBySettleMissingDirectory keeps every
// missing-directory close in the daemon on the one settlement: a caller
// closing such a row itself skips the ruled recreation, which is how a running
// daemon closed workspaces whose branches survived (2026-10-24).
func TestAMissingDirectoryIsClosedOnlyBySettleMissingDirectory(t *testing.T) {
	sourcescan.InPackage(t, internalWorkspaceDir)
	// Arrange.
	const closeRecord = "a workspace whose directory is gone could not be closed"
	root := sourcescan.DaemonRoot(t)

	// Act.
	var spelledIn []string
	sourcescan.EachDaemonSource(t, root, nil, func(fset *token.FileSet, file *ast.File) {
		ast.Inspect(file, func(n ast.Node) bool {
			if lit, ok := n.(*ast.BasicLit); ok && lit.Kind == token.STRING && strings.Contains(lit.Value, closeRecord) {
				rel, _ := filepath.Rel(root, fset.Position(lit.Pos()).Filename)
				spelledIn = append(spelledIn, rel)
			}
			return true
		})
	})

	// Assert.
	want := []string{filepath.Join("internal", "workspace", "missingdir.go")}
	if !slices.Equal(spelledIn, want) {
		t.Fatalf("the missing-directory close is spelled in %v, want only %v", spelledIn, want)
	}
}

func TestEveryNameIsDisambiguatedByFreeNameAgainst(t *testing.T) {
	sourcescan.InPackage(t, internalWorkspaceDir)
	// Arrange / Act: the suffix loop is written once.
	loops := sourcescan.Count(t, internalWorkspaceDir, `"-" + strconv.Itoa(suffix)`)

	// Assert
	if loops != 1 {
		t.Fatalf("name suffixes are composed at %d sites, want 1: freeNameAgainst", loops)
	}
}

// TestFirstPageRequestsHaveNamedSitesOnly is the structural half of the rule:
// it fails the moment any other path could request a first page without a
// pointer.
//
//   - The two replay openings each have ONE production caller, the fleet's
//     openingFor.
//   - A WatchAgentRequest is built in ONE place, the watcher's watchRequest,
//     which opens tail_only or from the pointer the watcher holds.
//   - A StartTurnRequest is built in ONE place, the sender's startTurn (behind
//     both StartTurn and JoinRunningTurn), which opens tail_only or from the
//     main pointer the daemon holds.
//   - A history's newest page is asked for in ONE place, the store reader's
//     Newest, which the fleet's ReadHistory calls only for a reader's
//     request (feed paging on demand): no watch and no turn reads history,
//     and no shim is asked for it.
func TestFirstPageRequestsHaveNamedSitesOnly(t *testing.T) {
	sourcescan.InPackage(t, internalWorkspaceDir)
	// Arrange.
	want := map[string][]string{
		"sessionwatcher.WorkspaceOpened":            {"internal/workspace/opening.go:openingFor"},
		"sessionwatcher.TranscriptSelected":         {"internal/workspace/opening.go:openingFor"},
		"shimv1.WatchAgentRequest":                  {"internal/sessionwatcher/watcher.go:watchRequest"},
		"shimv1.StartTurnRequest":                   {"internal/workspace/sender.go:startTurn"},
		"storereadv1.ReadHistoryPageNewest":         {"internal/storereader/storereader.go:Newest"},
		"storereadv1.ReadHistoryPageRequest_Newest": {"internal/storereader/storereader.go:Newest"},
	}

	// Act.
	got := firstPageSites(t, sourcescan.DaemonRoot(t), want)

	// Assert.
	for name, sites := range want {
		if strings.Join(got[name], ",") != strings.Join(sites, ",") {
			t.Errorf("%s is referenced from %v, want only %v", name, got[name], sites)
		}
	}
}

// firstPageSites answers, for every watched name, the production sites (the
// integration harness excluded) that
// reference it as "<file relative to root>:<enclosing func>", sorted.
func firstPageSites(t *testing.T, root string, watched map[string][]string) map[string][]string {
	t.Helper()
	got := map[string][]string{}
	fset := token.NewFileSet()
	err := filepath.WalkDir(root, func(path string, d fs.DirEntry, err error) error {
		if err != nil {
			return err
		}
		if d.IsDir() {
			// PRODUCTION ONLY: the integration suite's harness is test support
			// that scripts a shim's requests, not a daemon path that sends one.
			if name := d.Name(); name == "integration" || name == "testdata" {
				return filepath.SkipDir
			}
			return nil
		}
		if !strings.HasSuffix(path, ".go") || strings.HasSuffix(path, "_test.go") {
			return nil
		}
		file, err := parser.ParseFile(fset, path, nil, 0)
		if err != nil {
			return err
		}
		rel, err := filepath.Rel(root, path)
		if err != nil {
			return err
		}
		for _, decl := range file.Decls {
			fn, ok := decl.(*ast.FuncDecl)
			if !ok {
				continue
			}
			ast.Inspect(fn, func(n ast.Node) bool {
				// A SITE IS A CALL OR A LITERAL — what builds an opening or a
				// request. A type named in a signature builds nothing.
				var expr ast.Expr
				switch node := n.(type) {
				case *ast.CallExpr:
					expr = node.Fun
				case *ast.CompositeLit:
					expr = node.Type
				default:
					return true
				}
				sel, ok := expr.(*ast.SelectorExpr)
				if !ok {
					return true
				}
				pkg, ok := sel.X.(*ast.Ident)
				if !ok {
					return true
				}
				name := pkg.Name + "." + sel.Sel.Name
				if _, watch := watched[name]; watch {
					got[name] = append(got[name], filepath.ToSlash(rel)+":"+fn.Name.Name)
				}
				return true
			})
		}
		return nil
	})
	if err != nil {
		t.Fatalf("walk %s: %v", root, err)
	}
	for name := range got {
		sort.Strings(got[name])
	}
	return got
}

func TestEveryGateAnswerTheDaemonChoosesUsesTheDefaultSummarizerAnswer(t *testing.T) {
	sourcescan.InPackage(t, internalWorkspaceDir)
	// Arrange.
	for _, file := range []string{"deploychecks.go", "answers.go"} {
		body, err := os.ReadFile(file)
		if err != nil {
			t.Fatal(err)
		}

		// Assert: the gate answers the daemon makes on its own build their
		// compaction through one helper, never by hand.
		if !strings.Contains(string(body), "defaultSummarizerAnswer()") || strings.Contains(string(body), "&agentreplv1.AnswerColdGateCompact{") {
			t.Fatalf("%s builds a gate's compaction by hand; it must use defaultSummarizerAnswer()", file)
		}
	}
}

func TestEveryFeedLookupThroughAShimSharesThroughShim(t *testing.T) {
	sourcescan.InPackage(t, internalWorkspaceDir)
	// Arrange: the lookups the feed asks a workspace's shim for.
	files := []string{"peerdelivery.go", "vendortask.go"}

	// Act, Assert.
	for _, name := range files {
		source, err := os.ReadFile(name)
		if err != nil {
			t.Fatalf("read %s: %v", name, err)
		}
		if !strings.Contains(string(source), "return throughShim(f, ws,") {
			t.Errorf("%s does not ask through throughShim", name)
		}
		if strings.Count(string(source), "f.Client(ws)") > 0 && name != "peerdelivery.go" {
			t.Errorf("%s reaches the shim client by hand", name)
		}
	}
}

// TestTheContextArmsAreBuiltOnlyByProto fails on a hand-rolled echo token
// anywhere in the daemon's production code: every one comes from
// QueueBypassContext.Proto.
func TestTheContextArmsAreBuiltOnlyByProto(t *testing.T) {
	sourcescan.InPackage(t, internalWorkspaceDir)
	// Arrange
	const spelling = "TopbarQueueBypassContext_FullMain{FullMain:"
	var offenders []string

	// Act
	err := filepath.WalkDir(filepath.Join("..", ".."), func(path string, d fs.DirEntry, err error) error {
		if err != nil || d.IsDir() || !strings.HasSuffix(path, ".go") || strings.HasSuffix(path, "_test.go") ||
			path == filepath.Join("..", "..", "internal", "workspace", "queuebypass.go") {
			return err
		}
		body, err := os.ReadFile(path)
		if err != nil {
			return err
		}
		if strings.Contains(string(body), spelling) {
			offenders = append(offenders, path)
		}
		return nil
	})

	// Assert
	if err != nil {
		t.Fatalf("walk the daemon: %v", err)
	}
	if len(offenders) != 0 {
		t.Fatalf("files building the context's echo token by hand = %v, want every one through QueueBypassContext.Proto", offenders)
	}
}

// TestOnlyRefuseUnrestorableRaisesTheUnrestorableArm pins refuseUnrestorable as
// the one place this package raises worktree_unrestorable, so no refusal of it
// can leave out the evidence the arm carries.
func TestOnlyRefuseUnrestorableRaisesTheUnrestorableArm(t *testing.T) {
	sourcescan.InPackage(t, internalWorkspaceDir)
	// Arrange, Act.
	got := functionsReferencing(t, "ArmWorktreeUnrestorable")

	// Assert.
	if !slices.Equal(got, []string{"refuseUnrestorable"}) {
		t.Fatalf("functions naming ArmWorktreeUnrestorable = %v, want only refuseUnrestorable", got)
	}
}

func TestEveryParentPreCheckGoesThroughRequireOpenParent(t *testing.T) {
	sourcescan.InPackage(t, internalWorkspaceDir)
	// Act.
	direct := sourcescan.Count(t, internalWorkspaceDir, "v.deps.DB.RequireOpenParent(")
	shared := sourcescan.Count(t, internalWorkspaceDir, "v.requireOpenParent(ctx, ")

	// Assert.
	if direct != 1 || shared != 3 {
		t.Fatalf("direct reads = %d, shared calls = %d; want Open, and Create's parent and landing target, all through the one helper", direct, shared)
	}
}

// TestNoRefusalHandRollsAWorkspaceRefField pins that every refusal whose one
// field is a `workspace` ref goes through refuseNamingWorkspace, so the field's
// name and shape are spelled once.
func TestNoRefusalHandRollsAWorkspaceRefField(t *testing.T) {
	sourcescan.InPackage(t, internalWorkspaceDir)
	// Act.
	perFile := map[string]int{}
	for _, file := range sourcescan.Production(t, internalWorkspaceDir) {
		if n := strings.Count(string(file.Source), `map[string]any{"workspace": &workspacev1.WorkspaceRef{`); n > 0 {
			perFile[file.Name] = n
		}
	}

	// Assert: the helper's own spelling alone.
	want := map[string]int{"refusal.go": 1}
	if !reflect.DeepEqual(perFile, want) {
		t.Fatalf("hand-rolled workspace ref fields per file = %v, want %v", perFile, want)
	}
}

// TestTheBackloggedFlagHasOneWrite fails when a verb writes the backlogged flag by
// hand instead of through recordBacklogged, which would skip the roster push or
// the cache keeper's edge.
func TestTheBackloggedFlagHasOneWrite(t *testing.T) {
	sourcescan.InPackage(t, internalWorkspaceDir)
	// Act
	sites := sourcescan.Sites(t, internalWorkspaceDir, "v.deps.DB.SetBacklogged(")

	// Assert
	if len(sites) != 1 || sites[0] != "select.go" {
		t.Fatalf("the backlogged flag is written at %v, want only recordBacklogged in select.go", sites)
	}
}

// A show and a select are the two callers that reveal a merge workspace for
// the user, and both go through the orchestrator's one reveal: the verbs ask
// it only from revealMergeHidden, and never write the reveal around it.
func TestAShowRevealsThroughTheOrchestratorsOneReveal(t *testing.T) {
	sourcescan.InPackage(t, internalWorkspaceDir)
	// Act
	asked := sourcescan.Sites(t, internalWorkspaceDir, "v.deps.Merge.RevealMergeWorkspace(")
	written := sourcescan.Sites(t, internalWorkspaceDir, "v.deps.DB.RevealMergeWorkspace(")

	// Assert
	if len(asked) != 1 || asked[0] != "select.go" || len(written) != 1 || written[0] != "select.go" {
		t.Fatalf("the reveal is asked for at %v and written at %v, want each once in select.go", asked, written)
	}
}

func TestSwapAndMoveShareThePlaceRefusal(t *testing.T) {
	sourcescan.InPackage(t, internalWorkspaceDir)
	// Act.
	shared := sourcescan.Count(t, internalWorkspaceDir, "placeRefusal(log, ")
	byHand := sourcescan.Count(t, internalWorkspaceDir, "ArmNotSiblings, err.Error()")

	// Assert.
	if shared != 3 || byHand != 1 {
		t.Fatalf("placeRefusal calls = %d and hand-spelled not_siblings arms = %d, want Swap, Move and the arrangement sharing the one helper", shared, byHand)
	}
}

// TestEveryBacklogMoveEndsInBackloggedRecorded fails when a move into or
// out of the backlog republishes or raises the keeper's edge by hand.
func TestEveryBacklogMoveEndsInBackloggedRecorded(t *testing.T) {
	sourcescan.InPackage(t, internalWorkspaceDir)
	// Act
	sites := sourcescan.Sites(t, internalWorkspaceDir, "v.backloggedRecorded(ctx, log, ws, ")

	// Assert
	if !slices.Equal(sites, []string{"select.go", "select.go"}) {
		t.Fatalf("backloggedRecorded is called at %v, want recordBacklogged and BacklogColdGate in select.go", sites)
	}
}

// EVERY BRING-UP TAKES THE RECORD'S FACTS THROUGH withRecord: no other code
// in the package sets a source's record facts, so a path that hand-copied one
// and forgot another fails here.
func TestEveryBringUpTakesTheRecordThroughWithRecord(t *testing.T) {
	sourcescan.InPackage(t, internalWorkspaceDir)
	// Arrange.
	handCopied := regexp.MustCompile(`(HoldKeepalive|WorkspaceName)\s*[:=]\s*record\.`)
	helperBody := regexp.MustCompile(`(?s)func \(src source\) withRecord\(.*?\n}\n`)

	// Act.
	var found []string
	for _, file := range sourcescan.Production(t, internalWorkspaceDir) {
		// withRecord's own body is the one copy there is meant to be.
		if handCopied.Match(helperBody.ReplaceAll(file.Source, nil)) {
			found = append(found, file.Name)
		}
	}

	// Assert.
	if len(found) > 0 {
		t.Fatalf("these files copy the record onto a source by hand instead of withRecord: %v", found)
	}
}

// TestEveryOrderedRemovalGoesThroughRemoveStoodDownLocked pins that the
// fleet takes a session out of its map only through removeStoodDownLocked,
// which counts it, or through retireReaped, which records a death: a removal
// written by hand would leave StandDowns blind to it, and a revival would
// then read the stand-down as a revival that lied.
func TestEveryOrderedRemovalGoesThroughRemoveStoodDownLocked(t *testing.T) {
	sourcescan.InPackage(t, internalWorkspaceDir)
	// Arrange.
	allowed := map[string]bool{"removeStoodDownLocked": true, "retireReaped": true}
	var offenders []string

	// Act.
	sourcescan.EachSourceIn(t, ".", false, func(fset *token.FileSet, file *ast.File) {
		name := filepath.Base(fset.Position(file.Pos()).Filename)
		for _, decl := range file.Decls {
			fn, ok := decl.(*ast.FuncDecl)
			if !ok || fn.Body == nil || allowed[fn.Name.Name] {
				continue
			}
			ast.Inspect(fn.Body, func(n ast.Node) bool {
				call, ok := n.(*ast.CallExpr)
				if !ok || len(call.Args) != 2 {
					return true
				}
				if id, ok := call.Fun.(*ast.Ident); !ok || id.Name != "delete" {
					return true
				}
				if sel, ok := call.Args[0].(*ast.SelectorExpr); ok && sel.Sel.Name == "sessions" {
					offenders = append(offenders, name+": "+fn.Name.Name)
				}
				return true
			})
		}
	})

	// Assert.
	if len(offenders) != 0 {
		t.Fatalf("sessions removed from the fleet's map by hand in %v; use removeStoodDownLocked", offenders)
	}
}

// TestNoCallerHandRollsTheNoSessionCheck holds every caller in this package
// to refusedNoSession: a hand-rolled arm comparison is where the checks drift.
func TestNoCallerHandRollsTheNoSessionCheck(t *testing.T) {
	sourcescan.InPackage(t, internalWorkspaceDir)
	// Arrange.
	pattern := regexp.MustCompile(`\.Arm\s*[!=]=\s*ArmShimNoSession`)

	// Act.
	var offenders []string
	for _, f := range sourcescan.Production(t, internalWorkspaceDir) {
		if f.Name == "shimarms.go" {
			continue
		}
		if pattern.Match(f.Source) {
			offenders = append(offenders, f.Name)
		}
	}

	// Assert.
	if len(offenders) != 0 {
		t.Fatalf("hand-rolled no_session checks in %v, want refusedNoSession", offenders)
	}
}

// TestEveryShippedBriefLoadGoesThroughLoadShippedBrief keeps the package's
// tests from hand-rolling the shipped corpus's path again.
func TestEveryShippedBriefLoadGoesThroughLoadShippedBrief(t *testing.T) {
	sourcescan.InPackage(t, internalWorkspaceDir)
	// Arrange.
	files, err := filepath.Glob("*_test.go")
	if err != nil {
		t.Fatalf("Glob: %v", err)
	}
	handRolled := `filepath.Join("..", "..", "..", "prompts")`

	for _, file := range files {
		t.Run(file, func(t *testing.T) {
			// Act.
			text, err := os.ReadFile(file)
			if err != nil {
				t.Fatalf("ReadFile: %v", err)
			}

			// Assert.
			count := strings.Count(string(text), handRolled)
			if file == "shippedbrief_test.go" {
				count-- // the helper's own definition; this rule lives in reposcans
			}
			if count != 0 {
				t.Fatalf("%s spells the shipped corpus's path %d times, want loadShippedBrief or shippedPromptsDir", file, count)
			}
		})
	}
}

// TestEveryCarryCheckGoesThroughAssertCarries keeps the package's tests from
// hand-rolling assertCarries's loop again.
func TestEveryCarryCheckGoesThroughAssertCarries(t *testing.T) {
	sourcescan.InPackage(t, internalWorkspaceDir)
	// Arrange.
	files, err := filepath.Glob("*_test.go")
	if err != nil {
		t.Fatalf("Glob: %v", err)
	}
	handRolled := "want it to carry %q"

	for _, file := range files {
		t.Run(file, func(t *testing.T) {
			// Act.
			text, err := os.ReadFile(file)
			if err != nil {
				t.Fatalf("ReadFile: %v", err)
			}

			// Assert.
			count := strings.Count(string(text), handRolled)
			if file == "shippedbrief_test.go" {
				count-- // the helper's own Fatalf; this rule lives in reposcans
			}
			if count != 0 {
				t.Fatalf("%s hand-rolls a carry check %d times, want assertCarries", file, count)
			}
		})
	}
}

func TestEveryRegistrationAdmitsThroughAdmit(t *testing.T) {
	sourcescan.InPackage(t, internalWorkspaceDir)
	// Arrange / Act: the one admission, and every site that admits a row.
	sites := 0
	for _, file := range []string{"register.go", "thread.go"} {
		src, err := os.ReadFile(file)
		if err != nil {
			t.Fatal(err)
		}
		sites += strings.Count(string(src), "v.admit(ctx, log, record, ")
	}

	// Assert
	if sites != 2 {
		t.Fatalf("admit is called at %d sites, want registerRow's and CreateThread's", sites)
	}
}

// TestOnlyDeferredBriefDefersASubmission pins deferredBrief as the one place
// this package composes a deferred submission: a verb hand-rolling one would
// be free to drift from it.
func TestOnlyDeferredBriefDefersASubmission(t *testing.T) {
	sourcescan.InPackage(t, internalWorkspaceDir)
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
	for _, file := range sourcescan.Production(t, internalWorkspaceDir) {
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

// TestEveryDeferredBriefIsComposedByQueueDeferredBrief pins the brief sites
// to queueDeferredBrief's shape: a site composing its own deferredBrief can
// drift from the queueing every other site shares.
func TestEveryDeferredBriefIsComposedByQueueDeferredBrief(t *testing.T) {
	sourcescan.InPackage(t, internalWorkspaceDir)
	// Act
	sites := sourcescan.Sites(t, internalWorkspaceDir, "deferredBrief(")

	// Assert: the definition and queueDeferredBrief's call, both in verbs.go.
	if !slices.Equal(sites, []string{"verbs.go", "verbs.go"}) {
		t.Fatalf("deferredBrief sites = %v, want only its definition and queueDeferredBrief in verbs.go", sites)
	}
}

// TestEveryDeferredBriefIsQueuedThroughQueueBrief pins that the brief sites
// share queueBrief's shape: a file composing a deferred brief that hands it
// to the queue itself would take a refused brief for a delivered one.
func TestEveryDeferredBriefIsQueuedThroughQueueBrief(t *testing.T) {
	sourcescan.InPackage(t, internalWorkspaceDir)
	// Arrange / Act.
	var direct []string
	for _, file := range sourcescan.Production(t, internalWorkspaceDir) {
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
