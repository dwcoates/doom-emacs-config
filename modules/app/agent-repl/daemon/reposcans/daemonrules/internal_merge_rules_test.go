package daemonrules

import (
	"fmt"
	"go/ast"
	"go/parser"
	"go/token"
	"os"
	"path/filepath"
	"regexp"
	"slices"
	"strconv"
	"strings"
	"testing"

	"agent-repld/internal/prompts"
	"agent-repld/reposcans/internal/sourcescan"
)

// internalMergeDir is the package these rules hold, under the daemon module's root.
const internalMergeDir = "internal/merge"

// TestEveryResolutionIsAnnouncedThroughEnded pins the one funnel: a merge's
// resolved banner is raised only through raiseResolved, by tellThenRaise,
// which ended (and the fork that never made a merge workspace) call, so every
// end is handed back before its banner rises. A handback skips ended only for
// a merge never put in line, so no terminal can hand back without announcing
// its end, and every end's banner is worded by the one bannerLine.
func TestEveryResolutionIsAnnouncedThroughEnded(t *testing.T) {
	sourcescan.InPackage(t, internalMergeDir)
	// Arrange / Act: the package's own production sources.
	resolved, raised, ordered, handBacks, settles := 0, 0, 0, 0, 0
	for _, file := range sourcescan.Production(t, internalMergeDir) {
		resolved += strings.Count(string(file.Source), "Banners.Resolved(")
		raised += strings.Count(string(file.Source), "o.raiseResolved(")
		ordered += strings.Count(string(file.Source), "o.tellThenRaise(")
		handBacks += strings.Count(string(file.Source), "o.handBack(ctx")
		settles += strings.Count(string(file.Source), "o.settle(ctx")
	}

	// Assert
	if resolved != 1 {
		t.Errorf("Banners.Resolved is called at %d sites, want 1: raiseResolved", resolved)
	}
	if raised != 1 {
		t.Errorf("raiseResolved is called at %d sites, want 1: tellThenRaise", raised)
	}
	if ordered != 2 {
		t.Errorf("tellThenRaise is called at %d sites, want 2: ended and the failed fork", ordered)
	}
	if handBacks != 1 {
		t.Errorf("handBack is called at %d sites, want 1: settle", handBacks)
	}
	if settles != 2 {
		t.Errorf("settle is called at %d sites, want 2: ended and a merge never put in line", settles)
	}
}

func TestOnlyEndAbandonedBubbleDrawsAnAbandonedHeadForAMergeWithNoRun(t *testing.T) {
	sourcescan.InPackage(t, internalMergeDir)
	// Act
	sites := sourcescan.Sites(t, internalMergeDir, "}, headAbandoned)")

	// Assert
	if !slices.Equal(sites, []string{"terminal.go"}) {
		t.Fatalf("an abandoned head is drawn at %v, want only endAbandonedBubble in terminal.go", sites)
	}
}

func TestOnlyTheHandOffAndTheHeldDeliveryPassTheColdGate(t *testing.T) {
	sourcescan.InPackage(t, internalMergeDir)
	// Act
	sites := sourcescan.Sites(t, internalMergeDir, "o.passColdGate(")

	// Assert: every merge turn reaches its queue through one of the two.
	if !slices.Equal(sites, []string{"resume.go", "run.go"}) {
		t.Fatalf("the gate is passed at %v, want bringTurnOn's held delivery and handOff alone", sites)
	}
}

func TestAHeadExtendsItsChainOnlyOnceItKnowsItsDeploy(t *testing.T) {
	sourcescan.InPackage(t, internalMergeDir)
	// Arrange: the admission's own chain extension is the one that follows
	// the head's session; a sourcescan finds it after takeSession.
	src, err := os.ReadFile("run.go")
	if err != nil {
		t.Fatal(err)
	}
	body := string(src)
	admit := body[strings.Index(body, "func (r *run) admit("):]
	admit = admit[:strings.Index(admit, "\n}\n")]

	// Assert
	take, extend := strings.Index(admit, "r.takeSession(ctx)"), strings.Index(admit, "o.extendChain(")
	if take < 0 || extend < 0 || extend < take {
		t.Fatalf("admit extends its chain at %d and takes its session at %d; want the extension after the session", extend, take)
	}
}

func TestEveryMergeTabIsPublishedThroughPublishTab(t *testing.T) {
	sourcescan.InPackage(t, internalMergeDir)
	// Arrange / Act: the package's own production sources.
	files := sourcescan.Production(t, internalMergeDir)

	// Assert: tabRow is composed only inside publishTab, so no tab reaches a
	// feed other than its bubble's.
	for _, file := range files {
		if file.Name == "tabs.go" {
			continue
		}
		if strings.Contains(string(file.Source), "tabRow(") {
			t.Errorf("%s composes a tab row itself, want every tab published through publishTab", file.Name)
		}
	}
}

// THE ROUND'S HOLD HAS ONE WRITER (holdRound): a second caller of the
// store's hold could mark a round held that no resume record waits on.
func TestOnlyHoldRoundWritesARoundsHold(t *testing.T) {
	sourcescan.InPackage(t, internalMergeDir)
	// Act
	var callers []string
	for _, file := range sourcescan.Production(t, internalMergeDir) {
		if strings.Contains(string(file.Source), ".HoldMergeRound(") && !strings.HasSuffix(file.Name, "held.go") {
			callers = append(callers, file.Name)
		}
	}

	// Assert
	if len(callers) != 0 {
		t.Fatalf("files calling HoldMergeRound besides held.go: %v", callers)
	}
}

func TestEveryScriptOutsideADeployRunRunsThroughRunKeptScript(t *testing.T) {
	sourcescan.InPackage(t, internalMergeDir)
	// Arrange: a deploy's own hooks run in deploy.go, drawn as they write;
	// every other script run (a release, a drain, a finish hook) is
	// RunKeptScript's.
	entries, err := os.ReadDir(".")
	if err != nil {
		t.Fatal(err)
	}
	calls := map[string]int{}

	// Act
	for _, entry := range entries {
		name := entry.Name()
		if !strings.HasSuffix(name, ".go") || strings.HasSuffix(name, "_test.go") {
			continue
		}
		raw, err := os.ReadFile(name)
		if err != nil {
			t.Fatal(err)
		}
		if n := strings.Count(string(raw), "RunLinesWith("); n > 0 {
			calls[name] = n
		}
	}

	// Assert
	if len(calls) != 3 || calls["deploy.go"] != 1 || calls["keptscript.go"] != 1 || calls["api.go"] != 1 {
		t.Fatalf("RunLinesWith is called in %v, want once in deploy.go (a deploy's own hooks), once in keptscript.go (RunKeptScript) and its declaration in api.go", calls)
	}
}

// TestEverySubmissionFilterGoesThroughSubmissionsTo keeps the deploy-check
// tests' filters of the harness's prompts on the one helper: a hand-rolled
// loop over the submissions by workspace fails here.
func TestEverySubmissionFilterGoesThroughSubmissionsTo(t *testing.T) {
	sourcescan.InPackage(t, internalMergeDir)
	for _, file := range []string{"mediation_test.go", "escalation_test.go"} {
		src, err := os.ReadFile(file)
		if err != nil {
			t.Fatal(err)
		}
		if n := strings.Count(string(src), "range h.queue.submissions {\n\t\tif sub.WS =="); n != 0 && !(file == "mediation_test.go" && n == 1) {
			t.Fatalf("%s filters the submissions by workspace by hand %d times; use submissionsTo", file, n)
		}
	}
}

// TestEveryCheckReleaseGoesThroughReleaseCheck keeps one path from a
// released check to its chain's end: a site releasing a check by hand, which
// could leave its chain running, fails here.
func TestEveryCheckReleaseGoesThroughReleaseCheck(t *testing.T) {
	sourcescan.InPackage(t, internalMergeDir)
	if sites := sourcescan.Sites(t, internalMergeDir, "DB.ReleaseMediation("); !slices.Equal(sites, []string{"mediation.go"}) {
		t.Fatalf("checks are released in %v, want only releaseCheck in mediation.go", sites)
	}
}

// TestEveryStillOwedTestGoesThroughOwes keeps one reading of "this check is
// still the one owed": a hand-rolled comparison of a check's instant fails
// here.
func TestEveryStillOwedTestGoesThroughOwes(t *testing.T) {
	sourcescan.InPackage(t, internalMergeDir)
	if sites := sourcescan.Sites(t, internalMergeDir, ".OwedAt.Equal("); !slices.Equal(sites, []string{"mediation.go"}) {
		t.Fatalf("an owed check's instant is compared in %v, want only owes in mediation.go", sites)
	}
}

func TestEveryListedQueueLookupGoesThroughQueueKeyed(t *testing.T) {
	sourcescan.InPackage(t, internalMergeDir)
	// Arrange: every merge source file but the helper's own.
	byHand := regexp.MustCompile(`sameQueue\(\w+\.key,`)
	total := 0

	// Act, Assert: no other code compares a listed queue's key by hand.
	for _, file := range sourcescan.Production(t, internalMergeDir) {
		n := len(byHand.FindAllIndex(file.Source, -1))
		if n > 0 && file.Name != "mediation.go" {
			t.Fatalf("%s compares a listed queue's key by hand; use queueKeyed", file.Name)
		}
		total += n
	}
	if total != 1 {
		t.Fatalf("a listed queue's key is compared %d times, want once, inside queueKeyed", total)
	}
}

// TestEveryFinishPhaseGoesThroughFinishPhase keeps the turn meter and the run
// counter labeling a finish alike: a site spelling a finish's phase by hand
// fails here.
func TestEveryFinishPhaseGoesThroughFinishPhase(t *testing.T) {
	sourcescan.InPackage(t, internalMergeDir)
	if sites := sourcescan.Sites(t, internalMergeDir, "f.ServesEscalation, MediatorPhaseFixing"); !slices.Equal(sites, []string{"mediatormetrics.go"}) {
		t.Fatalf("a finish's phase is spelled in %v, want only finishPhase in mediatormetrics.go", sites)
	}
}

// The head's gate gives up through fixesExhausted; a hand-rolled copy of its
// summary anywhere else would drift from it.
func TestEveryExhaustedGateGivesUpThroughFixesExhausted(t *testing.T) {
	sourcescan.InPackage(t, internalMergeDir)
	// Arrange
	sources := []string{"process.go", "chain.go"}

	// Act
	calls, copies := 0, 0
	for _, name := range sources {
		body, err := os.ReadFile(name)
		if err != nil {
			t.Fatalf("reading %s: %v", name, err)
		}
		calls += strings.Count(string(body), "r.fixesExhausted(")
		copies += strings.Count(string(body), "fixing attempts; the whole run is archived")
	}

	// Assert: one caller, and the summary spelled once, in the helper.
	if calls != 1 || copies != 1 {
		t.Fatalf("fixesExhausted calls = %d, summary spellings = %d; want 1 and 1", calls, copies)
	}
}

// handSettled is a run's settled draw followed by its tests tab settling.
var handSettled = regexp.MustCompile(`publishRun\([^\n]*\.settled\([^\n]*\n\s*(g\.)?r\.settleTesting\(`)

func TestEveryRunEndingSettlesThroughEndRun(t *testing.T) {
	sourcescan.InPackage(t, internalMergeDir)
	// Arrange.
	for _, file := range []string{"gate.go", "chain.go", "process.go"} {
		body, err := os.ReadFile(file)
		if err != nil {
			t.Fatalf("reading %s: %v", file, err)
		}

		// Act / Assert: a run settled together with its tests tab never
		// stands outside endRun. (A failed run alone, which leaves the tab
		// live for the fixing attempt after it, is not that shape.)
		if handSettled.Match(body) {
			t.Fatalf("%s settles a run and its tests tab by hand instead of through endRun", file)
		}
	}
}

func TestOnlyRegisteredQueuesKeysTheRegistry(t *testing.T) {
	sourcescan.InPackage(t, internalMergeDir)
	// Arrange: every walk of the registry, the status read's included, is
	// registeredQueues', which passes an unresolvable repository over.
	entries, err := os.ReadDir(".")
	if err != nil {
		t.Fatal(err)
	}
	var walkers []string

	// Act
	for _, entry := range entries {
		name := entry.Name()
		if !strings.HasSuffix(name, ".go") || strings.HasSuffix(name, "_test.go") {
			continue
		}
		raw, err := os.ReadFile(name)
		if err != nil {
			t.Fatal(err)
		}
		if strings.Contains(string(raw), "repoKeyFor(ctx, repository.Dir)") {
			walkers = append(walkers, name)
		}
	}

	// Assert
	if !slices.Equal(walkers, []string{"orchestrator.go"}) {
		t.Fatalf("the registry is keyed by hand in %v, want orchestrator.go (registeredQueues) alone", walkers)
	}
}

func TestNoopPauseRefusalsAllGoThroughRefuseNoopPause(t *testing.T) {
	sourcescan.InPackage(t, internalMergeDir)
	// Arrange.
	src, err := os.ReadFile("queue.go")
	if err != nil {
		t.Fatalf("read queue.go: %v", err)
	}

	// Act.
	text := string(src)

	// Assert: two call sites share the helper, and only the helper builds the
	// already_paused arm by hand.
	if n := strings.Count(text, "o.refuseNoopPause(op, paused,"); n != 2 {
		t.Fatalf("refuseNoopPause call sites = %d, want 2", n)
	}
	if n := strings.Count(text, "ArmAlreadyPaused, "); n != 1 {
		t.Fatalf("hand-built already_paused arms in queue.go = %d, want only the helper's", n)
	}
}

// TestEveryRebaseIsStartedFromShasOf is the drift guard on the shared shape.
func TestEveryRebaseIsStartedFromShasOf(t *testing.T) {
	sourcescan.InPackage(t, internalMergeDir)
	// Arrange.
	body, err := os.ReadFile("process.go")
	if err != nil {
		t.Fatalf("reading process.go: %v", err)
	}

	// Act.
	starts := strings.Count(string(body), "r.git.StartRebase(")
	shared := strings.Count(string(body), "r.git.StartRebase(ctx, dir, tip, shasOf(")

	// Assert.
	if starts == 0 || starts != shared {
		t.Fatalf("%d rebase starts, %d through shasOf, want every one through it", starts, shared)
	}
}

// TestEveryRebaseRecordIsWrittenByRecordRebase covers the one record every
// rebase writes: the commits a rebase replays reach a record only through
// recordRebase, so no rebase start writes a record of its own shape.
func TestEveryRebaseRecordIsWrittenByRecordRebase(t *testing.T) {
	sourcescan.InPackage(t, internalMergeDir)
	// Arrange
	fset := token.NewFileSet()
	for _, source := range sourcescan.Production(t, internalMergeDir) {
		parsed, err := parser.ParseFile(fset, source.Name, source.Source, 0)
		if err != nil {
			t.Fatalf("parsing %s: %v", source.Name, err)
		}

		// Act
		var enclosing string
		ast.Inspect(parsed, func(n ast.Node) bool {
			if decl, ok := n.(*ast.FuncDecl); ok {
				enclosing = decl.Name.Name
			}
			assign, ok := n.(*ast.AssignStmt)
			if !ok {
				return true
			}
			for _, lhs := range assign.Lhs {
				sel, ok := lhs.(*ast.SelectorExpr)
				// Assert
				if ok && (sel.Sel.Name == "BranchHead" || sel.Sel.Name == "Commits") && enclosing != "recordRebase" {
					if id, isIdent := sel.X.(*ast.Ident); isIdent && id.Name == "d" {
						t.Errorf("%s: %s writes a rebase's record by hand, not through recordRebase", fset.Position(assign.Pos()), enclosing)
					}
				}
			}
			return true
		})
	}
}

func TestEveryCheckTurnMarkGoesThroughSubmitMarkedTurn(t *testing.T) {
	sourcescan.InPackage(t, internalMergeDir)
	// Arrange
	marks := regexp.MustCompile(`DB\.(MarkMediationResumed|MarkProductionCheckPrompted|MarkProductionCheckResumed)\(`)

	// Act, Assert: each such record is made inside submitMarkedTurn's mark.
	for _, file := range sourcescan.Production(t, internalMergeDir) {
		for _, at := range marks.FindAllIndex(file.Source, -1) {
			before := string(file.Source[max(0, at[0]-300):at[0]])
			if !strings.Contains(before, "submitMarkedTurn(") {
				t.Fatalf("%s records a check turn outside submitMarkedTurn: %s", file.Name, file.Source[at[0]:at[1]])
			}
		}
	}
}

func TestEveryQueueReorderingRunsThroughReorder(t *testing.T) {
	sourcescan.InPackage(t, internalMergeDir)
	// Arrange.
	var calls []string
	for _, file := range []string{"queue.go", "terminal.go"} {
		code, err := os.ReadFile(file)
		if err != nil {
			t.Fatalf("read %s: %v", file, err)
		}
		// Act.
		for _, call := range []string{"o.deps.DB.MoveMerge(", "o.deps.DB.MoveMergeTo(", "o.deps.DB.PlaceMerge(", "o.deps.DB.PrioritizeMerge(", "o.reorder(ctx, ws, reordering{"} {
			for range strings.Count(string(code), call) {
				calls = append(calls, file+":"+call)
			}
		}
	}

	// Assert: each store reorder is called once, inside the reordering its
	// verb hands to reorder.
	want := []string{
		"queue.go:o.deps.DB.MoveMerge(", "queue.go:o.deps.DB.MoveMergeTo(", "queue.go:o.deps.DB.PlaceMerge(",
		"queue.go:o.reorder(ctx, ws, reordering{", "queue.go:o.reorder(ctx, ws, reordering{", "queue.go:o.reorder(ctx, ws, reordering{",
		"terminal.go:o.deps.DB.PrioritizeMerge(", "terminal.go:o.reorder(ctx, ws, reordering{",
	}
	if !slices.Equal(calls, want) {
		t.Fatalf("reorder sites = %v, want %v", calls, want)
	}
}

func TestEveryScopedReadSharesReadScopedRepository(t *testing.T) {
	sourcescan.InPackage(t, internalMergeDir)
	// Arrange
	const message = "could not resolve the scoped repository's queue key"

	// Act
	count := sourcescan.Count(t, internalMergeDir, message)

	// Assert
	if count != 1 {
		t.Fatalf("%q is written %d times; want it only in readScopedRepository, so no scoped read records its failure by hand", message, count)
	}
}

func TestEveryContainingLogMatchRunsThroughLoggedLike(t *testing.T) {
	sourcescan.InPackage(t, internalMergeDir)
	// Arrange.
	files, err := filepath.Glob("*_test.go")
	if err != nil {
		t.Fatal(err)
	}
	for _, file := range files {
		body, err := os.ReadFile(file)
		if err != nil {
			t.Fatalf("reading %s: %v", file, err)
		}

		// Act / Assert: a hand loop matching a message by containment is a
		// second matcher; only loggedLike loops so.
		if n := strings.Count(string(body), "strings.Contains(r.Message,"); n > 0 && file != "recover_test.go" {
			t.Fatalf("%s matches log records by hand %d times instead of loggedLike", file, n)
		}
	}
}

// TestBothDisplacedPutBacksSubmitTheOneShape holds the merge's release and the
// boot sweep to displacedResubmission, so a hand-rolled put-back that mints its
// own turn (and so names a turn its claim never recorded) fails here.
func TestBothDisplacedPutBacksSubmitTheOneShape(t *testing.T) {
	sourcescan.InPackage(t, internalMergeDir)
	for _, file := range []string{"terminal.go", "recover.go"} {
		t.Run(file, func(t *testing.T) {
			// Arrange
			raw, err := os.ReadFile(file)
			if err != nil {
				t.Fatal(err)
			}
			source := string(raw)

			// Assert
			if !strings.Contains(source, "displacedResubmission(") {
				t.Errorf("%s does not put a displaced turn back through displacedResubmission", file)
			}
			if file == "terminal.go" && strings.Contains(source, "PROMPT_ORIGIN_MERGE_DISPLACED_TURN_RESUME") {
				t.Errorf("%s spells the displaced-turn submission by hand", file)
			}
		})
	}
}

// TestEveryTakeBackOfARecordGoesThroughTakeBackResume pins the shape both
// take-backs share: a merge resuming admitted (recoverResumable) and one
// waiting back in line from its repair (recoverBackInLine) are stood as
// resumes and drawn by the one helper, so neither hand-rolls a divergent
// draw.
func TestEveryTakeBackOfARecordGoesThroughTakeBackResume(t *testing.T) {
	sourcescan.InPackage(t, internalMergeDir)
	// Arrange + Act
	calls := sourcescan.Sites(t, internalMergeDir, "o.takeBackResume(")
	draws := sourcescan.Sites(t, internalMergeDir, "headWaiting, headRecover)")

	// Assert
	if !slices.Equal(calls, []string{"recover.go", "recover.go"}) {
		t.Fatalf("takeBackResume is called at %v, want once by each take-back in recover.go", calls)
	}
	if !slices.Equal(draws, []string{"recover.go"}) {
		t.Fatalf("a taken-back merge is drawn waiting at %v, want only in takeBackResume", draws)
	}
}

// TestEveryWalkOverAllTheQueuesTakesQueueRepos pins that no walk over all
// the merge queues builds its own list of their repositories: queueRepos is
// the one range over the map in the package.
func TestEveryWalkOverAllTheQueuesTakesQueueRepos(t *testing.T) {
	sourcescan.InPackage(t, internalMergeDir)
	// Act
	sites := sourcescan.Sites(t, internalMergeDir, "for repo := range queues {")

	// Assert
	if !slices.Equal(sites, []string{"recover.go"}) {
		t.Fatalf("ranges over all the queues in %v, want only queueRepos's in recover.go", sites)
	}
}

// TestEveryWriteThatMovesTheChainsEntriesHoldsTheLineLock covers the shape
// every such write shares: it runs inside a lineWrite or leaveLine closure,
// so no hand-rolled write moves the line under a decision that read it.
func TestEveryWriteThatMovesTheChainsEntriesHoldsTheLineLock(t *testing.T) {
	sourcescan.InPackage(t, internalMergeDir)
	// Arrange: the writes that move a chain member's entry, and the files
	// whose every call of them must hold the lock (nil: every file).
	guarded := map[string][]string{
		"StepAsideMerge":        nil,
		"ReturnRemediatedMerge": nil,
		"RemoveMergeQueueEntry": {"terminal.go"},
	}
	fset := token.NewFileSet()
	seen := map[string]int{}
	for _, source := range sourcescan.Production(t, internalMergeDir) {
		file := source.Name
		parsed, err := parser.ParseFile(fset, file, source.Source, 0)
		if err != nil {
			t.Fatalf("parsing %s: %v", file, err)
		}

		// Act: walk every call, tracking the lock closures it stands in.
		var stack []ast.Node
		ast.Inspect(parsed, func(n ast.Node) bool {
			if n == nil {
				stack = stack[:len(stack)-1]
				return true
			}
			stack = append(stack, n)
			call, ok := n.(*ast.CallExpr)
			if !ok {
				return true
			}
			sel, ok := call.Fun.(*ast.SelectorExpr)
			if !ok {
				return true
			}
			only, guardedCall := guarded[sel.Sel.Name]
			if !guardedCall || only != nil && !slices.Contains(only, file) || forwards(stack, sel.Sel.Name) {
				return true
			}
			seen[sel.Sel.Name]++

			// Assert
			if !underLineLock(stack) {
				t.Errorf("%s: %s is called outside a lineWrite or leaveLine closure", fset.Position(call.Pos()), sel.Sel.Name)
			}
			return true
		})
	}
	for name := range guarded {
		if seen[name] == 0 {
			t.Errorf("no call of %s was found; the guarded set is stale", name)
		}
	}
}

// forwards reports whether the innermost node of STACK stands in a method
// named NAME: a store decorator (rosterQueueDB, queueEventsDB) handing the
// write on to the store it wraps, which its caller's lock already covers.
func forwards(stack []ast.Node, name string) bool {
	for _, n := range stack {
		if decl, ok := n.(*ast.FuncDecl); ok {
			return decl.Recv != nil && decl.Name.Name == name
		}
	}
	return false
}

// underLineLock reports whether the innermost node of STACK stands inside a
// function literal passed to lineWrite or leaveLine.
func underLineLock(stack []ast.Node) bool {
	for i := len(stack) - 2; i > 0; i-- {
		lit, ok := stack[i].(*ast.FuncLit)
		if !ok {
			continue
		}
		call, ok := stack[i-1].(*ast.CallExpr)
		if !ok || !slices.Contains(call.Args, ast.Expr(lit)) {
			continue
		}
		if sel, ok := call.Fun.(*ast.SelectorExpr); ok && (sel.Sel.Name == "lineWrite" || sel.Sel.Name == "leaveLine") {
			return true
		}
	}
	return false
}

// TestEveryStepAsideGoesThroughHandOff covers the one path both triggers
// share: stepAside is reached only through handOff, and handOff is called for
// every reason there is.
func TestEveryStepAsideGoesThroughHandOff(t *testing.T) {
	sourcescan.InPackage(t, internalMergeDir)
	// Arrange
	fset := token.NewFileSet()
	reasons := map[string]int{}
	for _, source := range sourcescan.Production(t, internalMergeDir) {
		parsed, err := parser.ParseFile(fset, source.Name, source.Source, 0)
		if err != nil {
			t.Fatalf("parsing %s: %v", source.Name, err)
		}

		// Act
		var enclosing string
		ast.Inspect(parsed, func(n ast.Node) bool {
			if decl, ok := n.(*ast.FuncDecl); ok {
				enclosing = decl.Name.Name
			}
			call, ok := n.(*ast.CallExpr)
			if !ok {
				return true
			}
			sel, ok := call.Fun.(*ast.SelectorExpr)
			if !ok {
				return true
			}
			switch sel.Sel.Name {
			case "stepAside":
				// Assert
				if enclosing != "handOff" {
					t.Errorf("%s: stepAside is called from %s, not through handOff", fset.Position(call.Pos()), enclosing)
				}
			case "handOff":
				if len(call.Args) >= 2 {
					if id, ok := call.Args[1].(*ast.Ident); ok {
						reasons[id.Name]++
					}
				}
			}
			return true
		})
	}
	for _, why := range []string{"AsideForTests", "AsideForConflicts"} {
		if reasons[why] == 0 {
			t.Errorf("no step asks handOff to step aside for %s", why)
		}
	}
}

// TestEveryBriefTheQueueNamesIsCheckedIn pins that each brief name the merge
// package declares has its prompt in the repository: the remediation's
// gate-failure brief was named but never written, so a failed gate stranded
// the finish (2026-11-15, deploy-escalation-20261115-174608).
func TestEveryBriefTheQueueNamesIsCheckedIn(t *testing.T) {
	sourcescan.InPackage(t, internalMergeDir)
	// Arrange
	declared := regexp.MustCompile(`\bBrief[A-Za-z]+\s*=\s*"([a-z][a-z-]*)"`)
	var names []string
	for _, f := range sourcescan.Production(t, internalMergeDir) {
		for _, m := range declared.FindAllSubmatch(f.Source, -1) {
			names = append(names, string(m[1]))
		}
	}
	if len(names) == 0 {
		t.Fatal("no brief name was found; the scan reads nothing")
	}

	// Act, Assert
	for _, name := range names {
		if _, err := prompts.Load("../../../prompts", name); err != nil {
			t.Errorf("brief %q: %v", name, err)
		}
	}
}

// TestMergeRequestRefusalsAreRecordedThroughLogRefusal keeps every typed
// refusal of a merge request on the one INFO helper: a refusal recorded at
// WARN by hand is the drift this replaced.
func TestMergeRequestRefusalsAreRecordedThroughLogRefusal(t *testing.T) {
	sourcescan.InPackage(t, internalMergeDir)
	// Arrange
	byHand := regexp.MustCompile(`log\.(Warn|Info)\(op, "refused`)

	// Act
	var offenders []string
	for _, file := range sourcescan.Production(t, internalMergeDir) {
		if (file.Name == "request.go" || file.Name == "interrupttab.go") && byHand.Match(file.Source) {
			offenders = append(offenders, file.Name)
		}
	}

	// Assert
	if len(offenders) != 0 {
		t.Fatalf("%v record a refusal by hand; use logRefusal", offenders)
	}
}

func TestEveryLookupOfWhereATurnStandsGoesThroughBringTurnOn(t *testing.T) {
	sourcescan.InPackage(t, internalMergeDir)
	// Act
	held := sourcescan.Sites(t, internalMergeDir, "DB.HeldPromptByTurn(")
	delivered := sourcescan.Sites(t, internalMergeDir, "Queue.DeliverHeld(")
	submitted := sourcescan.Sites(t, internalMergeDir, "Queue.Submit(context.WithoutCancel(")

	// Assert: one lookup, in resume.go, and one hand-off, in run.go.
	if !slices.Equal(held, []string{"resume.go"}) || !slices.Equal(delivered, []string{"resume.go"}) || !slices.Equal(submitted, []string{"run.go"}) {
		t.Fatalf("held read at %v, delivered at %v, handed off at %v; want bringTurnOn and handOff alone", held, delivered, submitted)
	}
}

// TestEveryAgenticRoundOpensThroughDrawLive is the drift guard on the shared
// shape: a round addressed anywhere else is a hand-rolled copy of drawLive.
func TestEveryAgenticRoundOpensThroughDrawLive(t *testing.T) {
	sourcescan.InPackage(t, internalMergeDir)
	// Arrange.
	sources := []string{"run.go", "process.go", "resume.go", "gate.go", "terminal.go", "recover.go"}

	// Act.
	sites := 0
	for _, name := range sources {
		body, err := os.ReadFile(name)
		if err != nil {
			t.Fatalf("reading %s: %v", name, err)
		}
		sites += strings.Count(string(body), "r.address(")
	}

	// Assert.
	if sites != 1 {
		t.Fatalf("r.address( appears %d times, want once (inside drawLive)", sites)
	}
}

func TestTheMergeAsksWhetherAContextEndedOnlyThroughItsTwoHelpers(t *testing.T) {
	sourcescan.InPackage(t, internalMergeDir)
	// Act.
	var sites []string
	for _, f := range sourcescan.Production(t, internalMergeDir) {
		for i, line := range strings.Split(string(f.Source), "\n") {
			if strings.Contains(line, "ctxend.") && !strings.Contains(line, "\"agent-repld/internal/ctxend\"") {
				sites = append(sites, fmt.Sprintf("%s:%d", f.Name, i+1))
			}
		}
	}

	// Assert: one in cutUnder (process.go), one in recordFailed (run.go).
	if len(sites) != 2 || !strings.HasPrefix(sites[0], "process.go:") || !strings.HasPrefix(sites[1], "run.go:") {
		t.Fatalf("ctxend is asked at %v, want only inside cutUnder and recordFailed", sites)
	}
}

// TestEveryRunIsBuiltByBuildRun holds the package to its one run shape: only
// buildRun spells a run, and only newBackgroundRun marks one background.
func TestEveryRunIsBuiltByBuildRun(t *testing.T) {
	sourcescan.InPackage(t, internalMergeDir)
	// Arrange.
	cases := []struct{ pattern, wantSite string }{
		{pattern: "&run{", wantSite: "func (o *orchestrator) buildRun("},
		{pattern: ".background = true", wantSite: "func (o *orchestrator) newBackgroundRun("},
	}
	for _, tc := range cases {
		t.Run(tc.pattern, func(t *testing.T) {
			// Act: each occurrence's enclosing top-level func.
			var sites []string
			for _, f := range sourcescan.Production(t, internalMergeDir) {
				enclosing := ""
				for _, line := range strings.Split(string(f.Source), "\n") {
					if strings.HasPrefix(line, "func ") {
						enclosing = line
					}
					if strings.Contains(line, tc.pattern) {
						sites = append(sites, enclosing)
					}
				}
			}

			// Assert.
			if len(sites) != 1 || !strings.HasPrefix(sites[0], tc.wantSite) {
				t.Fatalf("%q appears in %v, want only in %s", tc.pattern, sites, tc.wantSite)
			}
		})
	}
}

// TestEveryTestRunWithACauseIsBuiltByCausedRun holds the suite's runs that
// read their cut's cause to causedRun, so no test hand-rolls a copy.
func TestEveryTestRunWithACauseIsBuiltByCausedRun(t *testing.T) {
	sourcescan.InPackage(t, internalMergeDir)
	// Arrange.
	files, err := filepath.Glob("*_test.go")
	if err != nil {
		t.Fatalf("listing the test files: %v", err)
	}

	// Act.
	sites := 0
	for _, name := range files {
		src, err := os.ReadFile(name)
		if err != nil {
			t.Fatalf("reading %s: %v", name, err)
		}
		sites += strings.Count(string(src), "func() error { return context.Cause(ctx) }")
	}

	// Assert: causedRun's own alone (this rule lives in reposcans, so its needle is not among them).
	if sites != 1 {
		t.Fatalf("%d sites read a run's cause, want causedRun's alone", sites)
	}
}

// TestEveryRunOpensItsLedgerThroughOpenLedger pins the one shape every run's
// ledger open shares: a run that opened the ledger by hand would number its
// rounds from one again and overwrite an earlier run's intervals.
func TestEveryRunOpensItsLedgerThroughOpenLedger(t *testing.T) {
	sourcescan.InPackage(t, internalMergeDir)
	// Act.
	callers := sourcescan.Sites(t, internalMergeDir, "DB.OpenMergeLedger(")

	// Assert.
	if !slices.Equal(callers, []string{"run.go"}) {
		t.Fatalf("DB.OpenMergeLedger is called from %v, want run.go's openLedger alone", callers)
	}
}

func TestEveryHeadChangeGoesThroughTheHeadHelpers(t *testing.T) {
	sourcescan.InPackage(t, internalMergeDir)
	// Arrange
	fset := token.NewFileSet()
	owners := map[string]bool{"takeHeadLocked": true, "vacateHeadLocked": true}
	for _, source := range sourcescan.Production(t, internalMergeDir) {
		parsed, err := parser.ParseFile(fset, source.Name, source.Source, 0)
		if err != nil {
			t.Fatalf("parsing %s: %v", source.Name, err)
		}

		// Act
		var enclosing string
		ast.Inspect(parsed, func(n ast.Node) bool {
			if decl, ok := n.(*ast.FuncDecl); ok {
				enclosing = decl.Name.Name
			}
			// Assert
			switch node := n.(type) {
			case *ast.AssignStmt:
				for _, lhs := range node.Lhs {
					if index, ok := lhs.(*ast.IndexExpr); ok && namesRunning(index.X) && !owners[enclosing] {
						t.Errorf("%s: %s sets the queue's head itself, not through takeHeadLocked", fset.Position(node.Pos()), enclosing)
					}
				}
			case *ast.CallExpr:
				if fn, ok := node.Fun.(*ast.Ident); ok && fn.Name == "delete" && len(node.Args) > 0 && namesRunning(node.Args[0]) && !owners[enclosing] {
					t.Errorf("%s: %s clears the queue's head itself, not through vacateHeadLocked", fset.Position(node.Pos()), enclosing)
				}
			}
			return true
		})
	}
}

// namesRunning reports whether EXPR is an orchestrator's head map,
// `<x>.running`.
func namesRunning(expr ast.Expr) bool {
	sel, ok := expr.(*ast.SelectorExpr)
	return ok && sel.Sel.Name == "running"
}

// TestResolveSubjectReadsWhatAMergeClosesFromLandingCloses pins the one
// statement of what a merge closes: every arm of resolveSubject reads it from
// wsm.MergeSource.LandingCloses, which the family rules read too, and none
// spells a closing workspace by hand.
func TestResolveSubjectReadsWhatAMergeClosesFromLandingCloses(t *testing.T) {
	sourcescan.InPackage(t, internalMergeDir)
	// Arrange.
	var byHand []string
	spelled := regexp.MustCompile(`closes(Also)?(:| =) *(r\.ws|r\.source\.Workspace|origin\.CloseOnLanding)`)

	// Act.
	calls := sourcescan.Count(t, internalMergeDir, "r.source.LandingCloses(")
	for _, f := range sourcescan.Production(t, internalMergeDir) {
		byHand = append(byHand, spelled.FindAllString(string(f.Source), -1)...)
	}

	// Assert.
	if calls != 2 {
		t.Fatalf("LandingCloses calls = %d, want one per closing arm (own branch, workspace)", calls)
	}
	if len(byHand) != 0 {
		t.Fatalf("closing workspaces spelled by hand: %v", byHand)
	}
}

func TestEveryAbandonedSentenceIsReadThroughAbandonedSentence(t *testing.T) {
	sourcescan.InPackage(t, internalMergeDir)
	// Arrange.
	for _, file := range []string{"terminal.go", "queue.go"} {
		body, err := os.ReadFile(file)
		if err != nil {
			t.Fatalf("reading %s: %v", file, err)
		}

		// Act.
		uses := strings.Count(string(body), "cause.summary()") + strings.Count(string(body), "cause.summaryRunning()")

		// Assert: only abandonedSentence (terminal.go) reads the sentences.
		if want := map[string]int{"terminal.go": 2, "queue.go": 0}[file]; uses != want {
			t.Fatalf("%s reads a cause's sentence %d times, want %d: every reading goes through abandonedSentence", file, uses, want)
		}
	}
}

// Every step the drain waits out under its one bound goes through drainStep,
// so a step added by hand cannot drift from the others' recording.
func TestEveryDrainStepGoesThroughDrainStep(t *testing.T) {
	sourcescan.InPackage(t, internalMergeDir)
	// Act
	steps := sourcescan.Sites(t, internalMergeDir, "drainStep(ctx, log, op, expired.C,")
	byHand := sourcescan.Sites(t, internalMergeDir, "case <-expired.C:")

	// Assert
	if len(steps) != 4 {
		t.Fatalf("drainStep is called at %v, want the off-queue judgement, the mediation, the admissions and the suspended runs", steps)
	}
	// The two left are not steps of that shape: the terminal waits record
	// every merge left undrained, and the git stop records what it waited.
	if want := []string{"terminal.go", "terminal.go"}; !slices.Equal(byHand, want) {
		t.Fatalf("the bound is waited by hand at %v, want only the terminal waits and the git stop", byHand)
	}
}

// TestEveryForcedSessionStopGoesThroughStopSessionForced pins that the
// queue's merges and a remediation's direct landing share the one teardown,
// and that an unlanded merge's session end is the one other forced stop: a
// forced session stop appears nowhere in the package's sources but
// stopSessionForced, whose callers are those two.
func TestEveryForcedSessionStopGoesThroughStopSessionForced(t *testing.T) {
	sourcescan.InPackage(t, internalMergeDir)
	// Act
	sites := sourcescan.SitesMatching(t, internalMergeDir, regexp.MustCompile(`\.StopSession\([^\n]*, true\)`))
	callers := callersOf(t, "o.stopSessionForced(")
	slices.Sort(callers)

	// Assert
	if !slices.Equal(sites, []string{"terminal.go"}) {
		t.Fatalf("forced session stops at %v, want only stopSessionForced's", sites)
	}
	if want := []string{"terminal.go:endSession", "terminal.go:retireLandedWorktree"}; !slices.Equal(callers, want) {
		t.Fatalf("stopSessionForced callers = %v, want %v", callers, want)
	}
}

// THE END OF A MERGE REACHES ITS WORKSPACES THROUGH ONE PATH: settle alone
// hands an end back and closes the merge workspace, and only ended and a merge
// never put in line settle. A site that handed an end back its own way would
// bypass the close (2026-11-23).
func TestEveryEndOfAMergeIsSettledThroughOnePath(t *testing.T) {
	sourcescan.InPackage(t, internalMergeDir)
	tests := []struct {
		needle string
		want   []string
	}{
		{needle: "o.settle(", want: []string{"terminal.go:ended", "terminal.go:publishAbandonedWith"}},
		{needle: "o.handBack(", want: []string{"terminal.go:settle"}},
		{needle: "o.closeEnded(", want: []string{"terminal.go:settle"}},
		// A finish withdrawn before it was in line is a sync merge's, in its
		// own workspace: there is no merge workspace to close.
		{needle: "o.handBackInPlace(", want: []string{"finish.go:withdrawFinished", "terminal.go:handBack"}},
		// A merge never forked has no merge workspace to close (dropDetached).
		{needle: "Detached.HandBack(", want: []string{"request.go:dropDetached", "terminal.go:handBack", "terminal.go:handBackInPlace"}},
	}
	for _, tt := range tests {
		t.Run(tt.needle, func(t *testing.T) {
			// Arrange, Act
			got := callersOf(t, tt.needle)

			// Assert
			slices.Sort(got)
			if !slices.Equal(got, tt.want) {
				t.Fatalf("callers of %s = %v, want %v", tt.needle, got, tt.want)
			}
		})
	}
}

// THE ROSTER IS REPUBLISHED THROUGH ONE HELPER: a site that called the
// registry itself would word and guard its failure its own way.
func TestTheRosterIsRepublishedOnlyThroughRepublishRoster(t *testing.T) {
	sourcescan.InPackage(t, internalMergeDir)
	// Arrange, Act
	got := callersOf(t, "deps.PublishRegistry(")

	// Assert
	if want := []string{"terminal.go:republishRoster"}; !slices.Equal(got, want) {
		t.Fatalf("callers of deps.PublishRegistry( = %v, want %v", got, want)
	}
}

func TestEveryLogIsCreatedThroughCreateLog(t *testing.T) {
	sourcescan.InPackage(t, internalMergeDir)
	// Act: the production sites that open one of the queue's logs.
	viaHelper := sourcescan.Count(t, internalMergeDir, "createLog(")
	byHand := sourcescan.Count(t, internalMergeDir, "os.Create(")
	written := sourcescan.Count(t, internalMergeDir, "os.WriteFile(")

	// Assert: the gate run, writeLog, the script hook and the helper's own
	// definition, and no hand-rolled create, which would write a log the
	// retention pass could prune mid-run. The one os.WriteFile is the failing
	// tests file in the worktree (writeFailingTests), which is no log.
	if viaHelper != 4 || byHand != 0 || written != 1 {
		t.Fatalf("createLog( appears %d times, os.Create( %d and os.WriteFile( %d; want 4, 0 and 1", viaHelper, byHand, written)
	}
}

// TestASuspensionAwaitsEveryOwnEndThroughAwaitOwnEnd holds suspendFor's runs
// that end by themselves -- in their terminal, or abandoned -- to the one
// bounded wait, so neither drifts into a hand-rolled copy.
func TestASuspensionAwaitsEveryOwnEndThroughAwaitOwnEnd(t *testing.T) {
	sourcescan.InPackage(t, internalMergeDir)
	// Arrange.
	body := orchestratorMethodBody(t, "suspendFor")

	// Act.
	calls := strings.Count(body, "o.awaitOwnEnd(")
	handRolled := strings.Count(body, "case <-mark.done") + strings.Count(body, "o.forgetMoved(ws)\n\t\tlog.Info(op, \"the merge was abandoned")

	// Assert.
	if calls != 2 || handRolled != 0 {
		t.Fatalf("suspendFor waits through awaitOwnEnd %d times with %d hand-rolled waits, want 2 and none", calls, handRolled)
	}
}

// orchestratorMethodBody answers the production source of the orchestrator's
// method NAME, from its declaration to the next one.
func orchestratorMethodBody(t *testing.T, name string) string {
	t.Helper()
	decl := "\nfunc (o *orchestrator) " + name + "("
	for _, f := range sourcescan.Production(t, internalMergeDir) {
		src := string(f.Source)
		start := strings.Index(src, decl)
		if start < 0 {
			continue
		}
		end := strings.Index(src[start+1:], "\nfunc ")
		if end < 0 {
			return src[start:]
		}
		return src[start : start+1+end]
	}
	t.Fatalf("%s was not found", name)
	return ""
}

func TestOrchestratorMethodBodyAnswersOneMethodWhole(t *testing.T) {
	sourcescan.InPackage(t, internalMergeDir)
	// Arrange + Act.
	body := orchestratorMethodBody(t, "forgetMoved")

	// Assert.
	if !strings.HasPrefix(body, "\nfunc (o *orchestrator) forgetMoved(") || strings.Count(body, "\nfunc ") != 1 || !strings.Contains(body, "delete(o.displaces, ws)") {
		t.Fatalf("body = %q, want forgetMoved alone and whole", body)
	}
}

// TestEverySuspensionCutsAndLetsGoThroughTheSharedHelpers holds every site
// that cuts a suspended run and waits for it to let go to cutSuspended and
// awaitLetGo, so no site drifts into a hand-rolled copy.
func TestEverySuspensionCutsAndLetsGoThroughTheSharedHelpers(t *testing.T) {
	sourcescan.InPackage(t, internalMergeDir)
	cases := []struct {
		method       string
		cuts, letsGo int
	}{
		{method: "suspendFor", cuts: 1, letsGo: 1},
		{method: "stepAside", cuts: 1},
		{method: "takeBackSteppedAside", letsGo: 1},
	}
	for _, tc := range cases {
		t.Run(tc.method, func(t *testing.T) {
			// Arrange.
			body := orchestratorMethodBody(t, tc.method)

			// Act.
			cuts, letsGo := strings.Count(body, "o.cutSuspended("), strings.Count(body, "o.awaitLetGo(")
			handRolled := strings.Count(body, "r.cancel(") + strings.Count(body, "case <-r.finished:\n\tcase <-time.After(")

			// Assert.
			if cuts != tc.cuts || letsGo != tc.letsGo || handRolled != 0 {
				t.Fatalf("%s cuts %d, lets go %d, hand-rolls %d, want %d, %d and none", tc.method, cuts, letsGo, handRolled, tc.cuts, tc.letsGo)
			}
		})
	}
}

// callersOf answers every production function of this package whose body
// mentions NEEDLE, as "file:function".
func callersOf(t *testing.T, needle string) []string {
	t.Helper()
	var out []string
	fset := token.NewFileSet()
	for _, file := range sourcescan.Production(t, internalMergeDir) {
		parsed, err := parser.ParseFile(fset, file.Name, file.Source, 0)
		if err != nil {
			t.Fatalf("parsing %s: %v", file.Name, err)
		}
		for _, decl := range parsed.Decls {
			fn, ok := decl.(*ast.FuncDecl)
			if !ok || fn.Body == nil {
				continue
			}
			body := string(file.Source[fset.Position(fn.Body.Pos()).Offset:fset.Position(fn.Body.End()).Offset])
			if strings.Contains(body, needle) {
				name := file.Name[strings.LastIndex(file.Name, "/")+1:]
				out = append(out, name+":"+fn.Name.Name)
			}
		}
	}
	return out
}

// THE MERGE'S VIEW IS DRAWN THROUGH ONE PATH (owner ruling, 2026-11-16): a
// tab row is written only by upsertTab, which only publishTab (each step's
// tab, drawn under the bubble's hold) and republishBubbles (the redraw when
// the hold moves) call; a roster stage is built only by rosterStages, from
// the view; and a step's hold is set only by publish, from the bubble's.
// A site that drew any of them its own way would be a second source.
func TestTheMergesViewIsDrawnThroughOnePath(t *testing.T) {
	sourcescan.InPackage(t, internalMergeDir)
	tests := []struct {
		needle string
		want   []string
	}{
		{needle: "tabRow(at.ws, lease, key, tab)", want: []string{"tabs.go:upsertTab"}},
		{needle: "o.upsertTab(", want: []string{"bubbleview.go:republishBubbles", "tabs.go:publishTab"}},
		{needle: "renderTab(", want: []string{"bubbleview.go:republishBubbles", "tabs.go:publishTab"}},
		{needle: "RosterMergeQueueStage{", want: []string{"rosterstages.go:rosterStages"}},
		{needle: "RosterMergeQueueStage_Held{", want: []string{"rosterstages.go:rosterStages"}},
		{needle: "FeedMergeTabHeld{", want: []string{"bubbleview.go:renderTab"}},
		{needle: "facts.Held =", want: []string{"orchestrator.go:publish"}},
		{needle: "viewOf(", want: []string{"rosterstages.go:stagesOf"}},
		{needle: "o.heldByQueue(", want: []string{"bubbleview.go:republishBubbles", "rosterqueue.go:rosterLine"}},
		{needle: "feedid.Encode(headRef(", want: []string{"tabs.go:headFeedID"}},
	}
	for _, tt := range tests {
		t.Run(tt.needle, func(t *testing.T) {
			// Act
			got := callersOf(t, tt.needle)

			// Assert
			if strings.Join(got, ",") != strings.Join(tt.want, ",") {
				t.Fatalf("functions mentioning %q = %v, want only %v", tt.needle, got, tt.want)
			}
		})
	}
}

// ONE LEDGER PER MERGE: a ledger is minted only as a merge takes its place in
// line (the request) or for a row an earlier build queued without one
// (ledgerOfEntry); every other step reads its queue entry's.
func TestALedgerIsMintedOnlyWhereAMergeTakesItsPlace(t *testing.T) {
	sourcescan.InPackage(t, internalMergeDir)
	// Act
	got := callersOf(t, "o.mintLedger(")

	// Assert
	// queueRequestGated is where a merge takes its place: queueRequestAt and
	// a requeue (Requeue) both put a merge in line through it.
	if strings.Join(got, ",") != "recover.go:ledgerOfEntry,request.go:queueRequestGated" {
		t.Fatalf("functions minting a ledger = %v", got)
	}
}

// declaredCauseNames reads, from the merge package's production sources, the name
// of every constant that is a cause: typed AbandonCause, or named cause*.
func declaredCauseNames(t *testing.T) []string {
	t.Helper()
	var names []string
	for _, file := range sourcescan.Production(t, internalMergeDir) {
		parsed, err := parser.ParseFile(token.NewFileSet(), file.Name, file.Source, 0)
		if err != nil {
			t.Fatalf("parse %s: %v", file.Name, err)
		}
		for _, decl := range parsed.Decls {
			gen, ok := decl.(*ast.GenDecl)
			if !ok || gen.Tok != token.CONST {
				continue
			}
			for _, spec := range gen.Specs {
				value := spec.(*ast.ValueSpec)
				typed := false
				if ident, ok := value.Type.(*ast.Ident); ok && ident.Name == "AbandonCause" {
					typed = true
				}
				for _, name := range value.Names {
					if typed || (strings.HasPrefix(name.Name, "cause") && len(name.Name) > len("cause")) {
						names = append(names, name.Name)
					}
				}
			}
		}
	}
	slices.Sort(names)
	return names
}

// tabledCauseNames reads the keys of the merge package's exitCauses table, in
// offqueue_test.go: the causes TestEveryExitCauseKeepsOrEndsItsRequest gives
// each its one fate.
func tabledCauseNames(t *testing.T) []string {
	t.Helper()
	path := filepath.Join(sourcescan.DaemonRoot(t), internalMergeDir, "offqueue_test.go")
	file, err := parser.ParseFile(token.NewFileSet(), path, nil, 0)
	if err != nil {
		t.Fatalf("parse %s: %v", path, err)
	}
	var names []string
	for _, decl := range file.Decls {
		gen, ok := decl.(*ast.GenDecl)
		if !ok || gen.Tok != token.VAR {
			continue
		}
		for _, raw := range gen.Specs {
			spec := raw.(*ast.ValueSpec)
			if len(spec.Names) != 1 || spec.Names[0].Name != "exitCauses" || len(spec.Values) != 1 {
				continue
			}
			for _, elt := range spec.Values[0].(*ast.CompositeLit).Elts {
				key, err := strconv.Unquote(elt.(*ast.KeyValueExpr).Key.(*ast.BasicLit).Value)
				if err != nil {
					t.Fatalf("unquote a key of exitCauses: %v", err)
				}
				names = append(names, key)
			}
		}
	}
	if len(names) == 0 {
		t.Fatalf("no exitCauses table in %s", path)
	}
	slices.Sort(names)
	return names
}

// TestTheExitCauseTableNamesEveryDeclaredCause holds the merge package's
// exitCauses table to every cause constant it declares, so each one is given
// its one fate (merge.TestEveryExitCauseKeepsOrEndsItsRequest): a cause with
// no fate would reach OffQueueWords undeclared and panic the daemon.
func TestTheExitCauseTableNamesEveryDeclaredCause(t *testing.T) {
	// Act
	declared := declaredCauseNames(t)
	tabled := tabledCauseNames(t)

	// Assert
	if !slices.Equal(declared, tabled) {
		t.Fatalf("declared causes %v, classified causes %v; want the same set", declared, tabled)
	}
}

// TestEveryBranchWorktreeLookupGoesThroughWorktreeHolding holds the merge
// package to its one lookup of a branch's worktree: a cleanup, a rule check
// or a subject that listed the worktrees itself could drift from it.
func TestEveryBranchWorktreeLookupGoesThroughWorktreeHolding(t *testing.T) {
	// Act
	sites := sourcescan.Sites(t, internalMergeDir, ".ListWorktrees(ctx, main)")

	// Assert
	if !slices.Equal(sites, []string{"request.go"}) {
		t.Fatalf("worktree listings = %v, want worktreeHolding's alone in request.go", sites)
	}
}
