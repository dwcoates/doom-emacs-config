package daemonrules

import (
	"go/ast"
	"go/parser"
	"go/token"
	"io/fs"
	"os"
	"path/filepath"
	"regexp"
	"slices"
	"strings"
	"testing"

	"agent-repld/reposcans/internal/sourcescan"
)

// internalResolveFeedDir is the package these rules hold, under the daemon module's root.
const internalResolveFeedDir = "internal/resolve/feed"

func TestEveryReaderOfAPromptsEntriesWalksThemThroughPromptEntries(t *testing.T) {
	sourcescan.InPackage(t, internalResolveFeedDir)
	// Arrange
	src, err := os.ReadFile("answers.go")
	if err != nil {
		t.Fatalf("read answers.go: %v", err)
	}

	// Act
	walks := strings.Count(string(src), ".GetEntries()")
	calls := strings.Count(string(src), "promptEntries(row)")

	// Assert: the one walk is promptEntries' own, and its one reader calls it.
	if walks != 1 || calls != 1 {
		t.Fatalf("answers.go reads GetEntries() %d times and calls promptEntries %d times, want 1 and 1", walks, calls)
	}
}

func TestToolCallsAndAsksShareOneStartPrecedence(t *testing.T) {
	sourcescan.InPackage(t, internalResolveFeedDir)
	// Arrange: the source of every settled card's clock start.
	src, err := os.ReadFile("toolcall.go")
	if err != nil {
		t.Fatal(err)
	}

	// Act, Assert: the tool call's settle takes its start through clockStart,
	// the same rule settledClock applies to asks and skills.
	if !strings.Contains(string(src), "u.startedAtMs = clockStart(u.startedAtMs, settled)") {
		t.Fatal("settledOutcome does not take its start through clockStart")
	}
}

func TestEveryComposerReadingTargetsThroughComposerTarget(t *testing.T) {
	sourcescan.InPackage(t, internalResolveFeedDir)
	// Arrange
	src, err := os.ReadFile("composer.go")
	if err != nil {
		t.Fatalf("read composer.go: %v", err)
	}

	// Act
	direct := strings.Count(string(src), "entryTarget(f, len(f.order)")
	shared := strings.Count(string(src), "composerTarget(f, reply, label)")

	// Assert: composerTarget's own call is the one direct reading.
	if direct != 1 || shared != 2 {
		t.Fatalf("composer.go reads entryTarget directly %d times and through composerTarget %d times, want 1 and 2", direct, shared)
	}
}

func TestEveryStoredRowSweepGoesThroughEachStoredRow(t *testing.T) {
	sourcescan.InPackage(t, internalResolveFeedDir)
	// Arrange: the order snapshot a safe sweep needs is taken in one place.
	var offenders []string

	// Act
	for _, file := range sourcescan.Production(t, internalResolveFeedDir) {
		if file.Name == "feeds.go" {
			continue
		}
		if strings.Contains(string(file.Source), "append([]string(nil), f.order...)") {
			offenders = append(offenders, file.Name)
		}
	}

	// Assert
	if len(offenders) > 0 {
		t.Fatalf("hand-rolled stored-row sweeps (use eachStoredRow): %s", strings.Join(offenders, ", "))
	}
}

func TestBothHeldCardsDrawThroughTheOneDrawing(t *testing.T) {
	sourcescan.InPackage(t, internalResolveFeedDir)
	// Arrange
	src, err := os.ReadFile("held.go")
	if err != nil {
		t.Fatalf("read held.go: %v", err)
	}

	// Act
	hands, draws := len(regexp.MustCompile(`, draw[,)]`).FindAllString(string(src), -1)), strings.Count(string(src), "r.drawUserBlocks(")

	// Assert: a held prompt's card and an ingress card are each handed the
	// one draw closure, and it is the only drawing of what was said.
	if hands != 2 || draws != 1 {
		t.Fatalf("held.go hands draw %d times and calls drawUserBlocks %d times, want 2 and 1", hands, draws)
	}
}

// THE REPLAYED PER-TURN READS SHARE ONE BODY: each is a readRecorded call and
// calls its Deps reader nowhere else, so a hand-rolled read that drifts from
// it fails here.
func TestTheReplayedPerTurnReadsAreReadRecordedCalls(t *testing.T) {
	sourcescan.InPackage(t, internalResolveFeedDir)
	// Arrange
	fset := token.NewFileSet()
	pkgs, err := parser.ParseDir(fset, ".", func(fi fs.FileInfo) bool { return !strings.HasSuffix(fi.Name(), "_test.go") }, 0)
	if err != nil {
		t.Fatalf("parse the package: %v", err)
	}
	want := map[string]bool{"recordedAddresses": false, "recordedMirrors": false, "recordedSenders": false, "recordedCloses": false}

	// Act
	for _, pkg := range pkgs {
		for _, file := range pkg.Files {
			for _, decl := range file.Decls {
				fn, ok := decl.(*ast.FuncDecl)
				if !ok || fn.Recv == nil {
					continue
				}
				if _, named := want[fn.Name.Name]; !named {
					continue
				}
				helper, direct := false, false
				ast.Inspect(fn.Body, func(n ast.Node) bool {
					call, ok := n.(*ast.CallExpr)
					if !ok {
						return true
					}
					switch fun := call.Fun.(type) {
					case *ast.Ident:
						helper = helper || fun.Name == "readRecorded"
					case *ast.SelectorExpr:
						if deps, ok := fun.X.(*ast.SelectorExpr); ok && deps.Sel.Name == "deps" {
							direct = true
						}
					}
					return true
				})
				if !helper || direct {
					t.Errorf("%s does not read through readRecorded alone", fn.Name.Name)
				}
				want[fn.Name.Name] = true
			}
		}
	}

	// Assert
	for name, found := range want {
		if !found {
			t.Errorf("%s was not found in the package", name)
		}
	}
}

func TestEveryRecordedHeadRewriteGoesThroughOneHelper(t *testing.T) {
	sourcescan.InPackage(t, internalResolveFeedDir)
	// Arrange
	source, err := os.ReadFile("mergefold_test.go")
	if err != nil {
		t.Fatalf("read: %v", err)
	}

	// The needle is assembled so this test's own source never matches it.
	needle := "encoded, err := proto." + "Marshal(row)"

	// Act
	marshals := strings.Count(string(source), needle)

	// Assert
	if marshals != 1 {
		t.Fatalf("a recorded head is re-marshalled at %d sites in mergefold_test.go, want one, in rewriteRecordedHead", marshals)
	}
}

func TestThePromptQueueMirrorDrawsThroughPromptRow(t *testing.T) {
	sourcescan.InPackage(t, internalResolveFeedDir)
	// Arrange
	raw, err := os.ReadFile("../../promptqueue/deliver.go")
	if err != nil {
		t.Fatalf("ReadFile: %v", err)
	}
	source := string(raw)

	// Act / Assert: the mirror composes no prompt row of its own.
	if !strings.Contains(source, "feed.PromptRow(") || strings.Contains(source, "FeedRow_UserPrompt{") {
		t.Fatal("the prompt queue's mirror does not draw its row through feed.PromptRow")
	}
}

// EVERY ROW RE-PUBLICATION GOES THROUGH republishRows: neither caller walks
// the feeds itself, so a hand-rolled loop that drifts from it fails here.
func TestTheRowRepublishersShareRepublishRows(t *testing.T) {
	sourcescan.InPackage(t, internalResolveFeedDir)
	files := map[string]string{
		"settleTurnPrompts":     "prompt.go",
		"restampPromptCommands": "commands.go",
	}
	for fn, file := range files {
		t.Run(fn, func(t *testing.T) {
			// Arrange.
			parsed, err := parser.ParseFile(token.NewFileSet(), file, nil, 0)
			if err != nil {
				t.Fatalf("parse %s: %v", file, err)
			}
			var body *ast.BlockStmt
			for _, decl := range parsed.Decls {
				if d, ok := decl.(*ast.FuncDecl); ok && d.Name.Name == fn {
					body = d.Body
				}
			}
			if body == nil {
				t.Fatalf("%s declares no %s", file, fn)
			}

			// Act.
			calls, ranges := false, false
			ast.Inspect(body, func(n ast.Node) bool {
				switch n := n.(type) {
				case *ast.SelectorExpr:
					if n.Sel.Name == "republishRows" {
						calls = true
					}
				case *ast.RangeStmt:
					if sel, ok := n.X.(*ast.SelectorExpr); ok && sel.Sel.Name == "feeds" {
						ranges = true
					}
				}
				return true
			})

			// Assert.
			if !calls || ranges {
				t.Fatalf("%s: calls republishRows = %v, walks the feeds itself = %v", fn, calls, ranges)
			}
		})
	}
}

// EVERY RULE THAT TREATS BOTH KINDS OF PROMPT ALIKE ASKS isPromptRow, so a
// hand-rolled pair of arm checks cannot drift from it.
func TestNoSourceHandRollsThePromptRowTest(t *testing.T) {
	sourcescan.InPackage(t, internalResolveFeedDir)
	// Arrange.
	pair := regexp.MustCompile(`GetUserPrompt\(\) != nil \|\| [a-z]+\.GetAgentPrompt\(\) != nil`)

	// Act.
	rolled := sourcescan.SitesMatching(t, internalResolveFeedDir, pair)

	// Assert: isPromptRow's own body is the one pair.
	if !slices.Equal(rolled, []string{"prompt.go"}) {
		t.Fatalf("hand-rolled prompt row tests = %v, want only isPromptRow's own in prompt.go", rolled)
	}
}

func TestTheChosenLabelsAreOrderedThroughInServedOrder(t *testing.T) {
	sourcescan.InPackage(t, internalResolveFeedDir)
	// Arrange: a hand-rolled ordering that drifted from the helper fails here.
	source, err := os.ReadFile("questiondraft.go")
	if err != nil {
		t.Fatalf("read questiondraft.go: %v", err)
	}

	// Act.
	calls := strings.Count(string(source), "inServedOrder(")

	// Assert: the definition plus toggleLabel's and pickByNumber's calls.
	if calls != 3 {
		t.Fatalf("inServedOrder( appears %d times, want its definition and two call sites", calls)
	}
}

// TestFeedTestsLookUpRowsThroughTheSharedHelper pins that no test in this
// package hand-rolls the row-by-id loop findRow and rowByID replaced.
func TestFeedTestsLookUpRowsThroughTheSharedHelper(t *testing.T) {
	sourcescan.InPackage(t, internalResolveFeedDir)
	// Arrange.
	files, err := filepath.Glob("*_test.go")
	if err != nil {
		t.Fatalf("glob: %v", err)
	}
	var offenders []string

	// Act.
	for _, file := range files {
		if file == "resolver_test.go" {
			continue
		}
		text, err := os.ReadFile(file)
		if err != nil {
			t.Fatalf("read %s: %v", file, err)
		}
		if strings.Contains(string(text), "if row.GetId().GetValue() == ") {
			offenders = append(offenders, file)
		}
	}

	// Assert.
	if len(offenders) != 0 {
		t.Fatalf("files hand-rolling a row lookup: %v (use findRow or rowByID)", offenders)
	}
}

func TestEveryStoredRowRestatementGoesThroughRestateRow(t *testing.T) {
	sourcescan.InPackage(t, internalResolveFeedDir)
	// Arrange: the only production files allowed to snapshot a row by hand are
	// the publication path itself and the restatement helper.
	allowed := map[string]bool{"resolver.go": true, "restate.go": true}
	var offenders []string

	// Act.
	for _, file := range sourcescan.Production(t, internalResolveFeedDir) {
		if allowed[file.Name] {
			continue
		}
		if strings.Contains(string(file.Source), "proto.Clone(") {
			offenders = append(offenders, file.Name)
		}
	}

	// Assert.
	if len(offenders) > 0 {
		t.Fatalf("hand-rolled row restatements (use restateRow): %s", strings.Join(offenders, ", "))
	}
}

// TestEveryRowToFoldLookupGoesThroughFoldOfRow fails a resolver file that walks
// the response folds comparing their rows by hand instead of asking foldOfRow.
func TestEveryRowToFoldLookupGoesThroughFoldOfRow(t *testing.T) {
	sourcescan.InPackage(t, internalResolveFeedDir)
	// Act.
	var offenders []string
	for _, file := range sourcescan.Production(t, internalResolveFeedDir) {
		if n := strings.Count(string(file.Source), "fold.row.GetValue() == "); n > 0 && (file.Name != "revision.go" || n > 1) {
			offenders = append(offenders, file.Name)
		}
	}

	// Assert.
	if len(offenders) != 0 {
		t.Fatalf("hand-rolled row-to-fold lookups in %v; ask foldOfRow", offenders)
	}
}

// TestEveryEntryPointPrimesItsWorkspace pins prime's one rule: every
// exported method of the resolver whose first argument names a workspace
// primes it as its first statement, so nothing it reaches under the lock
// reads the store (prime.go). The exceptions name why.
func TestEveryEntryPointPrimesItsWorkspace(t *testing.T) {
	sourcescan.InPackage(t, internalResolveFeedDir)
	// Arrange.
	exempt := map[string]string{
		"MarkMergeWorkspaceRemoved": "it never wakes a workspace whose feed is not held",
		"SetWorkspaceName":          "a push of a name, which reads nothing",
	}
	var unprimed []string

	// Act.
	sourcescan.EachSourceIn(t, ".", false, func(fset *token.FileSet, file *ast.File) {
		for _, decl := range file.Decls {
			fn, ok := decl.(*ast.FuncDecl)
			if !ok || fn.Recv == nil || !fn.Name.IsExported() || !receiverIsResolver(fn) || exempt[fn.Name.Name] != "" {
				continue
			}
			ws, ok := firstWorkspaceParam(fn)
			if !ok {
				continue
			}
			if !primesFirst(fn, ws) {
				unprimed = append(unprimed, fset.Position(fn.Pos()).String()+" "+fn.Name.Name)
			}
		}
	})

	// Assert.
	if len(unprimed) > 0 {
		t.Fatalf("entry points that do not prime their workspace first: %v", unprimed)
	}
}

func receiverIsResolver(fn *ast.FuncDecl) bool {
	star, ok := fn.Recv.List[0].Type.(*ast.StarExpr)
	if !ok {
		return false
	}
	id, ok := star.X.(*ast.Ident)
	return ok && id.Name == "resolver"
}

// firstWorkspaceParam answers the name of FN's first ids.WorkspaceID
// parameter when it is FN's first one past a context.
func firstWorkspaceParam(fn *ast.FuncDecl) (string, bool) {
	for _, field := range fn.Type.Params.List {
		if sourcescan.IsSelector(field.Type, "context", "Context") {
			continue
		}
		if !sourcescan.IsSelector(field.Type, "ids", "WorkspaceID") || len(field.Names) == 0 {
			return "", false
		}
		return field.Names[0].Name, true
	}
	return "", false
}

// primesFirst reports whether FN's first statement is r.prime(WS, ...).
func primesFirst(fn *ast.FuncDecl, ws string) bool {
	if fn.Body == nil || len(fn.Body.List) == 0 {
		return false
	}
	stmt, ok := fn.Body.List[0].(*ast.ExprStmt)
	if !ok {
		return false
	}
	call, ok := stmt.X.(*ast.CallExpr)
	if !ok || len(call.Args) == 0 {
		return false
	}
	sel, ok := call.Fun.(*ast.SelectorExpr)
	if !ok || sel.Sel.Name != "prime" {
		return false
	}
	arg, ok := call.Args[0].(*ast.Ident)
	return ok && strings.EqualFold(arg.Name, ws)
}

// The two ways a feed test hands the resolver a created shell share one shape.
func TestEveryCreatedShellInTheseTestsIsBuiltByCreatedShell(t *testing.T) {
	sourcescan.InPackage(t, internalResolveFeedDir)
	for _, file := range []string{"subagent_test.go", "turnended_test.go"} {
		t.Run(file, func(t *testing.T) {
			// Arrange.
			text, err := os.ReadFile(file)
			if err != nil {
				t.Fatal(err)
			}

			// Act.
			n := strings.Count(string(text), "DetachableWork_"+"Bash{")

			// Assert: only createdShell itself builds one by hand.
			want := 0
			if file == "subagent_test.go" {
				want = 1
			}
			if n != want {
				t.Fatalf("%s builds %d created shells by hand, want %d (use createdShell)", file, n, want)
			}
		})
	}
}

// TestEveryReleasedHoldIsDrawnThroughOneHelper pins the consolidation: a hold
// released without a naming frame is drawn by drawReleasedSpawn alone, so a
// hand-rolled second copy of its fold-compose-push cannot drift from it.
func TestEveryReleasedHoldIsDrawnThroughOneHelper(t *testing.T) {
	sourcescan.InPackage(t, internalResolveFeedDir)
	// Arrange.
	source, err := os.ReadFile("subagent.go")
	if err != nil {
		t.Fatalf("reading subagent.go: %v", err)
	}

	// Act.
	count := strings.Count(string(source), "r.composeSubagent(s, state.feed, unitID, state, commission)")

	// Assert.
	if count != 1 {
		t.Fatalf("subagent.go composes a released hold at %d sites, want exactly one (drawReleasedSpawn)", count)
	}
}

func TestEveryDiffLineIsBuiltThroughTheSharedConstructors(t *testing.T) {
	sourcescan.InPackage(t, internalResolveFeedDir)
	// Arrange: each arm's literal may appear once, in its constructor.
	arms := []string{"FeedDiffLine_Header{", "FeedDiffLine_Added{", "FeedDiffLine_Removed{", "FeedDiffLine_Context{"}

	// Act, Assert.
	for _, arm := range arms {
		if n := sourcescan.Count(t, internalResolveFeedDir, arm); n != 1 {
			var where []string
			for _, file := range sourcescan.Production(t, internalResolveFeedDir) {
				if strings.Contains(string(file.Source), arm) {
					where = append(where, file.Name)
				}
			}
			t.Fatalf("%s is built %d times (%s), want once, in its constructor", arm, n, strings.Join(where, ", "))
		}
	}
}

// TestOnlyInterruptedArmBuildsTheInterruptedOutcome pins that both paths that
// draw a stopped turn's ending — the terminal's and the daemon-built close's —
// go through interruptedArm: no other production function in this package
// spells the interrupted outcome, so the two cannot drift apart.
func TestOnlyInterruptedArmBuildsTheInterruptedOutcome(t *testing.T) {
	sourcescan.InPackage(t, internalResolveFeedDir)
	// Arrange
	files := sourcescan.Production(t, internalResolveFeedDir)
	fset := token.NewFileSet()

	// Act
	var builders []string
	for _, file := range files {
		name := file.Name
		parsed, err := parser.ParseFile(fset, name, file.Source, 0)
		if err != nil {
			t.Fatalf("parse %s: %v", name, err)
		}
		for _, declaration := range parsed.Decls {
			function, ok := declaration.(*ast.FuncDecl)
			if !ok || function.Body == nil {
				continue
			}
			ast.Inspect(function.Body, func(node ast.Node) bool {
				literal, ok := node.(*ast.CompositeLit)
				if !ok {
					return true
				}
				if selector, ok := literal.Type.(*ast.SelectorExpr); ok && selector.Sel.Name == "FeedTurnEnded_Interrupted" {
					builders = append(builders, name+":"+function.Name.Name)
				}
				return true
			})
		}
	}

	// Assert
	if len(builders) != 1 || builders[0] != "turnended.go:interruptedArm" {
		t.Fatalf("interrupted outcome built in %v, want only turnended.go:interruptedArm", builders)
	}
}
