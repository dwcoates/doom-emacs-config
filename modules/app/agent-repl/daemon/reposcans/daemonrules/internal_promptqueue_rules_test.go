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
	"strconv"
	"strings"
	"testing"

	"agent-repld/reposcans/internal/sourcescan"
)

// internalPromptqueueDir is the package these rules hold, under the daemon module's root.
const internalPromptqueueDir = "internal/promptqueue"

// TestAContextCutIsSpelledOnlyBySessionCommand pins that the queue spells no
// context cut's literal by hand: sessioncommand.Literal reads it off the
// schema, so the queue and the vendor's CLI cannot disagree on it.
func TestAContextCutIsSpelledOnlyBySessionCommand(t *testing.T) {
	sourcescan.InPackage(t, internalPromptqueueDir)
	for _, literal := range []string{`"/compact"`, `"/clear"`} {
		t.Run(literal, func(t *testing.T) {
			// Act
			n := sourcescan.Count(t, internalPromptqueueDir, literal)

			// Assert
			if n != 0 {
				t.Errorf("the queue's sources spell %s by hand %d times", literal, n)
			}
		})
	}
}

// THE CLASSIFIER'S FOLD ASKS holdfold.SameAuthor: no file of the queue
// compares two prompts' senders by hand.
func TestTheQueueAsksHoldfoldWhetherTwoPromptsShareAnAuthor(t *testing.T) {
	sourcescan.InPackage(t, internalPromptqueueDir)
	// Arrange
	handRolled := regexp.MustCompile(`SentBy\s*==\s*\*|\*[a-z.]*SentBy\s*==`)

	// Act
	var found []string
	for _, file := range sourcescan.Production(t, internalPromptqueueDir) {
		if handRolled.Match(file.Source) {
			found = append(found, file.Name)
		}
	}

	// Assert
	if len(found) > 0 {
		t.Fatalf("these files compare senders by hand instead of holdfold.SameAuthor: %v", found)
	}
}

// THE QUEUE ASKS holdfold.Seen whether a hold stands in the line the user
// sees: no file of the queue reads a hold's origin as a daemon notice by hand.
func TestTheQueueAsksHoldfoldWhetherAHoldIsSeen(t *testing.T) {
	sourcescan.InPackage(t, internalPromptqueueDir)
	// Arrange
	handRolled := regexp.MustCompile(`\bDaemonNotice(Named)?\([A-Za-z_.\[\]]*\.Origin\)`)

	// Act
	var found []string
	for _, file := range sourcescan.Production(t, internalPromptqueueDir) {
		if handRolled.Match(file.Source) {
			found = append(found, file.Name)
		}
	}

	// Assert
	if len(found) > 0 {
		t.Fatalf("these files read a hold as a daemon notice by hand instead of holdfold.Seen: %v", found)
	}
}

// TestTheQueueReadsTheLineThroughHoldfold pins itemAhead to holdfold.InLine,
// the reading the fold and the tray's "fold above" share: an ahead.go that
// tests a cold gate's kept prompt or a daemon notice by hand fails here
// rather than drifting from the tray.
func TestTheQueueReadsTheLineThroughHoldfold(t *testing.T) {
	sourcescan.InPackage(t, internalPromptqueueDir)
	// Arrange
	var source string
	for _, file := range sourcescan.Production(t, internalPromptqueueDir) {
		if file.Name == "ahead.go" {
			source = string(file.Source)
		}
	}

	// Act
	handRolled := regexp.MustCompile(`HoldColdGateAnswered|holdfold\.Seen\(`).FindAllString(source, -1)

	// Assert
	if !strings.Contains(source, "holdfold.InLine(") || len(handRolled) != 0 {
		t.Fatalf("ahead.go calls holdfold.InLine: %v, hand-rolls %v; want the one reading", strings.Contains(source, "holdfold.InLine("), handRolled)
	}
}

// THE LEASE'S HOLDER IS READ ONCE: the arrival path names its wait from the
// hold kind holdForLease projects, so no other function switches on a holder.
func TestOnlyHoldForLeaseReadsALeasesHolder(t *testing.T) {
	sourcescan.InPackage(t, internalPromptqueueDir)
	// Arrange
	fset, funcs := productionFuncs(t)

	// Act
	var found []string
	for _, fn := range funcs {
		if fn.Name.Name == "holdForLease" || fn.Name.Name == "holderName" || fn.Name.Name == "noteDrainRefusal" || fn.Name.Name == "exemptFromLease" {
			continue
		}
		ast.Inspect(fn, func(n ast.Node) bool {
			if clause, ok := n.(*ast.CaseClause); ok {
				for _, e := range clause.List {
					if sel, ok := e.(*ast.SelectorExpr); ok && sel.Sel.Name == "HolderMerge" {
						found = append(found, fset.Position(sel.Pos()).String()+" in "+fn.Name.Name)
					}
				}
			}
			return true
		})
	}

	// Assert
	if len(found) > 0 {
		t.Fatalf("these functions switch on a lease's holder instead of reading holdForLease's hold kind: %v", found)
	}
}

// EVERY BOUNCE DECISION ASKS THE ONE LEASE HOLD: no site in bounce.go asks the
// merge's hold alone, so none can take a bounce a stand-down's lease holds.
func TestEveryBounceDecisionAsksTheOneLeaseHold(t *testing.T) {
	sourcescan.InPackage(t, internalPromptqueueDir)
	// Arrange
	src, err := os.ReadFile("bounce.go")
	if err != nil {
		t.Fatal(err)
	}

	// Act
	direct := strings.Count(string(src), "q.heldBehindMergeLocked(")
	shared := strings.Count(string(src), "q.heldBehindLeaseLocked(")

	// Assert
	if direct != 1 || shared != 3 {
		t.Fatalf("heldBehindMergeLocked calls = %d, heldBehindLeaseLocked calls = %d, want only the shared hold to ask the merge's (1) and three decisions to ask the shared hold", direct, shared)
	}
}

func TestEveryLedgerARedriveSettlesIsTakenThroughTakeAll(t *testing.T) {
	sourcescan.InPackage(t, internalPromptqueueDir)
	tests := []struct {
		ledger string
	}{
		{ledger: "parked"},
		{ledger: "deferredJoins"},
	}
	for _, tc := range tests {
		t.Run(tc.ledger, func(t *testing.T) {
			// Arrange, Act
			taken := sourcescan.Count(t, internalPromptqueueDir, "takeAll(q, &d.state."+tc.ledger+")")
			byHand := sourcescan.Count(t, internalPromptqueueDir, "state."+tc.ledger+" = nil")

			// Assert: a ledger cleared by hand would drift from takeAll's
			// take-exactly-once shape.
			if taken != 1 || byHand != 0 {
				t.Fatalf("%s taken through takeAll %d times and cleared by hand %d times, want once and never", tc.ledger, taken, byHand)
			}
		})
	}
}

// TestEveryUnjudgedVerdictIsReadThroughNeverJudged pins that hold,
// classifyHeld and verdictFor share ONE reading: a site calling
// sessionActVerdict or deferredVerdict directly could keep a kind of prompt
// from the model on one path and hand it over on another.
func TestEveryUnjudgedVerdictIsReadThroughNeverJudged(t *testing.T) {
	sourcescan.InPackage(t, internalPromptqueueDir)
	// Arrange
	body, err := os.ReadFile("classify.go")
	if err != nil {
		t.Fatalf("read classify.go: %v", err)
	}

	// Act
	calls := map[string]int{}
	for _, name := range []string{"q.sessionActVerdict(", "q.deferredVerdict(", "q.neverJudged("} {
		calls[name] = strings.Count(string(body), name)
	}

	// Assert: each primitive is read once, inside neverJudged, and the three
	// deciding sites read neverJudged.
	if calls["q.sessionActVerdict("] != 1 || calls["q.deferredVerdict("] != 1 || calls["q.neverJudged("] != 3 {
		t.Fatalf("call counts = %v, want sessionActVerdict 1, deferredVerdict 1 (both in neverJudged) and neverJudged 3", calls)
	}
}

// THE ONE RETRACTION. Every production path in this package that takes an
// accepted prompt's mirrored row back goes through retractAccepted, so no
// site hand-rolls a retraction that could take down a different row.
func TestEveryMirroredRowRetractionGoesThroughRetractAccepted(t *testing.T) {
	sourcescan.InPackage(t, internalPromptqueueDir)
	// Arrange
	fset, funcs := productionFuncs(t)
	var sites []string
	// Act
	for _, fn := range funcs {
		ast.Inspect(fn, func(n ast.Node) bool {
			if sel, ok := n.(*ast.SelectorExpr); ok && sel.Sel.Name == "OnPromptRetired" && fn.Name.Name != "retractAccepted" {
				sites = append(sites, fset.Position(sel.Pos()).String()+" in "+fn.Name.Name)
			}
			return true
		})
	}
	// Assert
	if len(sites) != 0 {
		t.Fatalf("mirrored rows retracted outside retractAccepted: %v", sites)
	}
}

// Every refusal of a recorded prompt goes through closeRecorded: a site that
// hand-rolled its own close and record would drift from the rest.
func TestEveryRecordedRefusalGoesThroughCloseRecorded(t *testing.T) {
	sourcescan.InPackage(t, internalPromptqueueDir)
	// Arrange
	files := []string{"deliver.go", "join.go"}

	// Act, Assert
	for _, name := range files {
		source, err := os.ReadFile(name)
		if err != nil {
			t.Fatalf("ReadFile %s: %v", name, err)
		}
		if strings.Contains(string(source), "ErrPromptRecorded") && !strings.Contains(string(source), "q.closeRecorded(") {
			t.Fatalf("%s handles ErrPromptRecorded without closeRecorded", name)
		}
	}
}

// doorMethods are the store's three ways to close a turn row.
var doorMethods = map[string]bool{"CloseTurn": true, "CloseOrphans": true, "ClaimDisplacedTurn": true}

// doorFile is the one production file that may call the store's closes.
const doorFile = "internal/promptqueue/turnclose.go"

// storeCloseFuncs are the store functions that may hold a closing statement.
var storeCloseFuncs = map[string]bool{"CloseTurn": true, "CloseOrphans": true, "ClaimDisplacedTurn": true}

// storeReopenFuncs are the store functions that may hold a reopening statement.
var storeReopenFuncs = map[string]bool{"reopenAmbiguousClose": true}

// doorViolations reports every way FILE (at daemon-relative PATH, in package
// PKG) closes a turn outside the door.
func doorViolations(fset *token.FileSet, file *ast.File, path string) []string {
	var out []string
	pkg := file.Name.Name
	ast.Inspect(file, func(n ast.Node) bool {
		call, ok := n.(*ast.CallExpr)
		if !ok {
			return true
		}
		sel, ok := call.Fun.(*ast.SelectorExpr)
		if !ok || !doorMethods[sel.Sel.Name] || path == doorFile || pkg == "wsm" {
			return true
		}
		if throughQueue(sel.X, pkg) {
			return true
		}
		out = append(out, fset.Position(call.Pos()).String()+": "+sel.Sel.Name+" closes a turn outside the prompt queue's door")
		return true
	})
	for _, decl := range file.Decls {
		fn, isFunc := decl.(*ast.FuncDecl)
		inTurns := isFunc && pkg == "wsm" && filepath.Base(path) == "turns.go"
		allowed := inTurns && storeCloseFuncs[fn.Name.Name]
		reopenAllowed := inTurns && storeReopenFuncs[fn.Name.Name]
		ast.Inspect(decl, func(n ast.Node) bool {
			lit, ok := n.(*ast.BasicLit)
			if !ok || lit.Kind != token.STRING {
				return true
			}
			text, err := strconv.Unquote(lit.Value)
			if err != nil {
				text = lit.Value
			}
			switch {
			case reopensARow(text):
				if !reopenAllowed {
					out = append(out, fset.Position(lit.Pos()).String()+": a statement clears a turn's close outside the store's reopen function")
				}
			case closesARow(text):
				if !allowed {
					out = append(out, fset.Position(lit.Pos()).String()+": a statement writes a turn's close outside the store's door functions")
				}
			}
			return true
		})
	}
	return out
}

// throughQueue reports whether a close's receiver is the prompt queue: a
// selector ending in `Queue`, or this package's own queue receiver `q`.
func throughQueue(x ast.Expr, pkg string) bool {
	switch recv := x.(type) {
	case *ast.SelectorExpr:
		return recv.Sel.Name == "Queue"
	case *ast.Ident:
		return pkg == "promptqueue" && recv.Name == "q"
	}
	return false
}

// reopensARow reports whether an SQL text clears a turn's close.
func reopensARow(text string) bool {
	squeezed := strings.Join(strings.Fields(text), " ")
	return strings.Contains(squeezed, "closed_at = NULL") || strings.Contains(squeezed, "close_kind = NULL")
}

// closesARow reports whether an SQL text writes a turn's close.
func closesARow(text string) bool {
	squeezed := strings.Join(strings.Fields(text), " ")
	return strings.Contains(squeezed, "closed_at = ") || strings.Contains(squeezed, "close_kind = ")
}

func TestNoProductionCodeClosesATurnOutsideTheDoor(t *testing.T) {
	sourcescan.InPackage(t, internalPromptqueueDir)
	// Arrange
	root, err := filepath.Abs(filepath.Join("..", ".."))
	if err != nil {
		t.Fatalf("daemon root: %v", err)
	}
	fset := token.NewFileSet()
	var violations []string
	scanned := 0

	// Act
	err = filepath.WalkDir(root, func(path string, d fs.DirEntry, err error) error {
		if err != nil {
			return err
		}
		if d.IsDir() {
			switch d.Name() {
			case "testdata", "integration", "node_modules":
				return filepath.SkipDir
			}
			return nil
		}
		if !strings.HasSuffix(path, ".go") || strings.HasSuffix(path, "_test.go") {
			return nil
		}
		rel, err := filepath.Rel(root, path)
		if err != nil {
			return err
		}
		file, err := parser.ParseFile(fset, path, nil, 0)
		if err != nil {
			return err
		}
		scanned++
		violations = append(violations, doorViolations(fset, file, filepath.ToSlash(rel))...)
		return nil
	})

	// Assert
	if err != nil {
		t.Fatalf("scan: %v", err)
	}
	if scanned == 0 {
		t.Fatal("the guard scanned no production source")
	}
	if len(violations) > 0 {
		t.Fatalf("turn rows closed outside the door:\n%s", strings.Join(violations, "\n"))
	}
}

func TestTheDoorGuard(t *testing.T) {
	sourcescan.InPackage(t, internalPromptqueueDir)
	tests := []struct {
		name string
		path string
		src  string
		want int
	}{
		{
			name: "a direct store close is caught",
			path: "internal/boot/sequence.go",
			src:  "package boot\nfunc f() { s.deps.DB.CloseOrphans(ctx, ws, at) }\n",
			want: 1,
		},
		{
			name: "a store close through a local handle is caught",
			path: "internal/merge/run.go",
			src:  "package merge\nfunc f() { db.ClaimDisplacedTurn(ctx, turn, at) }\n",
			want: 1,
		},
		{
			name: "a close through the queue is the door",
			path: "internal/workspace/teardown.go",
			src:  "package workspace\nfunc f() { v.deps.Queue.CloseOrphans(ctx, ws, at) }\n",
			want: 0,
		},
		{
			name: "the queue's own door method is the door",
			path: "internal/promptqueue/lifecycle.go",
			src:  "package promptqueue\nfunc f() { q.CloseOrphans(ctx, ws, at) }\n",
			want: 0,
		},
		{
			name: "a store close inside the door is allowed",
			path: doorFile,
			src:  "package promptqueue\nfunc f() { q.deps.DB.CloseTurn(ctx, turn, at, how) }\n",
			want: 0,
		},
		{
			name: "a closing statement outside the store's door functions is caught",
			path: "internal/wsm/turns.go",
			src:  "package wsm\nfunc PutTurn() { exec(`UPDATE turns SET closed_at = ?, close_kind = ? WHERE id = ?`) }\n",
			want: 1,
		},
		{
			name: "a closing statement in another package is caught",
			path: "internal/drain/drain.go",
			src:  "package drain\nfunc f() { exec(\"UPDATE turns SET close_kind = 3\") }\n",
			want: 1,
		},
		{
			name: "a closing statement inside the store's CloseTurn is allowed",
			path: "internal/wsm/turns.go",
			src:  "package wsm\nfunc CloseTurn() { exec(`UPDATE turns SET closed_at = ?, close_kind = ? WHERE id = ?`) }\n",
			want: 0,
		},
		{
			name: "a reopening statement inside the store's reopen function is allowed",
			path: "internal/wsm/turns.go",
			src:  "package wsm\nfunc reopenAmbiguousClose() { exec(`UPDATE turns SET closed_at = NULL, close_kind = NULL WHERE id = ?`) }\n",
			want: 0,
		},
		{
			name: "a reopening statement outside the store's reopen function is caught",
			path: "internal/wsm/turns.go",
			src:  "package wsm\nfunc PutTurn() { exec(`UPDATE turns SET closed_at = NULL, close_kind = NULL WHERE id = ?`) }\n",
			want: 1,
		},
		{
			name: "a closing statement inside the store's reopen function is caught",
			path: "internal/wsm/turns.go",
			src:  "package wsm\nfunc reopenAmbiguousClose() { exec(`UPDATE turns SET closed_at = ?, close_kind = ? WHERE id = ?`) }\n",
			want: 1,
		},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange
			fset := token.NewFileSet()
			file, err := parser.ParseFile(fset, tc.path, tc.src, 0)
			if err != nil {
				t.Fatalf("parse: %v", err)
			}

			// Act
			got := doorViolations(fset, file, tc.path)

			// Assert
			if len(got) != tc.want {
				t.Fatalf("violations = %v, want %d", got, tc.want)
			}
		})
	}
}

// TestTurnsAreHandedOverAndRetiredThroughOneHelper fails a queue source that
// hands a turn to the watcher, or tombstones a delivered hold, by hand
// instead of through handOver and retireDelivered.
func TestTurnsAreHandedOverAndRetiredThroughOneHelper(t *testing.T) {
	sourcescan.InPackage(t, internalPromptqueueDir)
	// Arrange
	owners := map[string]string{"OnTurnOpened": "handOver", "tombstoneDelivered": "retireDelivered"}
	entries, err := os.ReadDir(".")
	if err != nil {
		t.Fatalf("read the package: %v", err)
	}
	var offenders []string

	// Act
	for _, e := range entries {
		name := e.Name()
		if !strings.HasSuffix(name, ".go") || strings.HasSuffix(name, "_test.go") {
			continue
		}
		file, err := parser.ParseFile(token.NewFileSet(), name, nil, 0)
		if err != nil {
			t.Fatalf("parse %s: %v", name, err)
		}
		for _, decl := range file.Decls {
			fn, ok := decl.(*ast.FuncDecl)
			if !ok || fn.Body == nil {
				continue
			}
			ast.Inspect(fn.Body, func(n ast.Node) bool {
				var used string
				switch x := n.(type) {
				case *ast.SelectorExpr:
					used = x.Sel.Name
				case *ast.CompositeLit:
					for _, elt := range x.Elts {
						if kv, ok := elt.(*ast.KeyValueExpr); ok {
							if id, ok := kv.Value.(*ast.Ident); ok && id.Name == "tombstoneDelivered" {
								used = "tombstoneDelivered"
							}
						}
					}
				}
				if owner, ok := owners[used]; ok && fn.Name.Name != owner {
					offenders = append(offenders, name+":"+fn.Name.Name+" uses "+used)
				}
				return true
			})
		}
	}

	// Assert
	if len(offenders) > 0 {
		t.Fatalf("hand-rolled hand-over or retirement: %v", offenders)
	}
}

// TestTheEpochIsComparedOnlyByVerdictStaleLocked fails a queue source that
// reads a turn's content epoch to judge a verdict by hand.
func TestTheEpochIsComparedOnlyByVerdictStaleLocked(t *testing.T) {
	sourcescan.InPackage(t, internalPromptqueueDir)
	// Arrange
	entries, err := os.ReadDir(".")
	if err != nil {
		t.Fatalf("read the package: %v", err)
	}
	var offenders []string

	// Act
	for _, e := range entries {
		name := e.Name()
		if !strings.HasSuffix(name, ".go") || strings.HasSuffix(name, "_test.go") {
			continue
		}
		src, err := os.ReadFile(name)
		if err != nil {
			t.Fatalf("read %s: %v", name, err)
		}
		if strings.Count(string(src), `"content_epoch"`) > 0 && name != "classify.go" {
			offenders = append(offenders, name)
		}
		if strings.Count(string(src), `"content_epoch"`) > 1 {
			offenders = append(offenders, name+" (twice)")
		}
	}

	// Assert
	if len(offenders) > 0 {
		t.Fatalf("a stale-verdict check written by hand in %v", offenders)
	}
}

// productionFuncs parses every production source of this package
// (sourcescan.Production) and answers each function declaration with the file
// set that positions it: what a structural rule over the package's own code
// walks.
func productionFuncs(t *testing.T) (*token.FileSet, []*ast.FuncDecl) {
	t.Helper()
	fset := token.NewFileSet()
	var funcs []*ast.FuncDecl
	for _, file := range sourcescan.Production(t, internalPromptqueueDir) {
		parsed, err := parser.ParseFile(fset, file.Name, file.Source, 0)
		if err != nil {
			t.Fatalf("parse %s: %v", file.Name, err)
		}
		for _, decl := range parsed.Decls {
			if fn, ok := decl.(*ast.FuncDecl); ok {
				funcs = append(funcs, fn)
			}
		}
	}
	return fset, funcs
}

// EVERY JOIN IS SENT BY sendJoinLocked: no other function of the queue asks
// the shim to join the running turn, so a held prompt's join and an
// arrival's cannot drift apart.
func TestEveryJoinIsSentBySendJoinLocked(t *testing.T) {
	sourcescan.InPackage(t, internalPromptqueueDir)
	// Arrange
	fset, funcs := productionFuncs(t)

	// Act
	var found []string
	for _, fn := range funcs {
		if fn.Name.Name == "sendJoinLocked" {
			continue
		}
		ast.Inspect(fn, func(n ast.Node) bool {
			if sel, ok := n.(*ast.SelectorExpr); ok && sel.Sel.Name == "JoinRunningTurn" {
				found = append(found, fset.Position(sel.Pos()).String()+" in "+fn.Name.Name)
			}
			return true
		})
	}

	// Assert
	if len(found) > 0 {
		t.Fatalf("these functions ask the shim to join the running turn instead of calling sendJoinLocked: %v", found)
	}
}

// TestNoSiteHandRollsTheStandingSessionHoldTest pins the call sites to the
// one predicate: a hand-rolled copy of its test fails here rather than
// drifting from it silently.
func TestNoSiteHandRollsTheStandingSessionHoldTest(t *testing.T) {
	sourcescan.InPackage(t, internalPromptqueueDir)
	// Arrange
	handRolled := regexp.MustCompile(`Tombstone\s*[!=]=\s*nil\s*(&&|\|\|)\s*\w+\.Hold\s*[!=]=\s*nil\s*(&&|\|\|)\s*\*\w+\.Hold\s*[!=]=\s*wsm\.HoldReconnect`)
	const definition = "return h.Tombstone == nil && h.Hold != nil && waitsOnSession(*h.Hold)"
	definitions := 0
	for _, file := range sourcescan.Production(t, internalPromptqueueDir) {
		// Act
		for i, line := range strings.Split(string(file.Source), "\n") {
			// Assert
			if strings.TrimSpace(line) == definition {
				definitions++
				continue
			}
			if handRolled.MatchString(line) {
				t.Errorf("%s:%d hand-rolls the standing session hold test; call standingSessionHold: %s", file.Name, i+1, strings.TrimSpace(line))
			}
		}
	}
	if definitions != 1 {
		t.Fatalf("found %d definitions of standingSessionHold's test, want exactly 1", definitions)
	}
}

// THE ONE SHAPE. Held prompts are put in queue order only by inQueueOrder, so
// no hand-rolled sort can order them by another key.
func TestEveryQueueOrderingGoesThroughInQueueOrder(t *testing.T) {
	sourcescan.InPackage(t, internalPromptqueueDir)
	// Arrange
	fset, funcs := productionFuncs(t)
	var sites []string
	// Act
	for _, fn := range funcs {
		if fn.Name.Name == "inQueueOrder" {
			continue
		}
		ast.Inspect(fn, func(n ast.Node) bool {
			call, ok := n.(*ast.CallExpr)
			if !ok {
				return true
			}
			if sel, ok := call.Fun.(*ast.SelectorExpr); ok && (sel.Sel.Name == "SliceStable" || sel.Sel.Name == "Slice") {
				sites = append(sites, fset.Position(call.Pos()).String()+" in "+fn.Name.Name)
			}
			return true
		})
	}
	// Assert
	if len(sites) != 0 {
		t.Fatalf("held prompts sorted outside inQueueOrder: %v", sites)
	}
}

// THE ONE SHAPE. A held prompt's turn is re-minted (`x.Turn = wsm.NewTurnID()`)
// only inside heldAgain, so no hand-rolled re-hold can keep a retirement or a
// schedule the old entry carried.
func TestEveryReHoldGoesThroughHeldAgain(t *testing.T) {
	sourcescan.InPackage(t, internalPromptqueueDir)
	// Arrange
	fset, funcs := productionFuncs(t)
	var sites []string
	// Act
	for _, fn := range funcs {
		if fn.Name.Name == "heldAgain" {
			continue
		}
		ast.Inspect(fn, func(n ast.Node) bool {
			assign, ok := n.(*ast.AssignStmt)
			if !ok || len(assign.Lhs) != 1 || len(assign.Rhs) != 1 {
				return true
			}
			lhs, ok := assign.Lhs[0].(*ast.SelectorExpr)
			if !ok || lhs.Sel.Name != "Turn" {
				return true
			}
			if call, ok := assign.Rhs[0].(*ast.CallExpr); ok {
				if sel, ok := call.Fun.(*ast.SelectorExpr); ok && sel.Sel.Name == "NewTurnID" {
					sites = append(sites, fset.Position(assign.Pos()).String()+" in "+fn.Name.Name)
				}
			}
			return true
		})
	}
	// Assert
	if len(sites) != 0 {
		t.Fatalf("a held prompt's turn re-minted outside heldAgain: %v", sites)
	}
}

// TestOnlyNoteSentNowStandingClearsTheReplaysShortcut scans the package's
// production sources: a site that clears sentNowNone by hand, rather than
// through noteSentNowStanding, fails here.
func TestOnlyNoteSentNowStandingClearsTheReplaysShortcut(t *testing.T) {
	sourcescan.InPackage(t, internalPromptqueueDir)
	// Arrange
	var offenders []string

	// Act
	sourcescan.EachSourceIn(t, ".", false, func(fset *token.FileSet, file *ast.File) {
		for _, decl := range file.Decls {
			fn, ok := decl.(*ast.FuncDecl)
			if !ok || fn.Name.Name == "noteSentNowStanding" || fn.Body == nil {
				continue
			}
			ast.Inspect(fn.Body, func(n ast.Node) bool {
				assign, ok := n.(*ast.AssignStmt)
				if !ok {
					return true
				}
				for i, lhs := range assign.Lhs {
					sel, ok := lhs.(*ast.SelectorExpr)
					if !ok || sel.Sel.Name != "sentNowNone" || i >= len(assign.Rhs) {
						continue
					}
					if ident, ok := assign.Rhs[i].(*ast.Ident); ok && ident.Name == "false" {
						offenders = append(offenders, fset.Position(assign.Pos()).String())
					}
				}
				return true
			})
		}
	})

	// Assert
	if len(offenders) != 0 {
		t.Fatalf("sentNowNone cleared outside noteSentNowStanding at %v", offenders)
	}
}

// TestEveryNoSessionRefusalStatesItsCause pins every refusal site to
// noSession: a bare ErrNoSession would reach the client with no account, which
// the server refuses as an invariant break.
func TestEveryNoSessionRefusalStatesItsCause(t *testing.T) {
	sourcescan.InPackage(t, internalPromptqueueDir)
	// Act
	sites := sourcescan.Sites(t, internalPromptqueueDir, "ErrNoSession")

	// Assert
	for _, site := range sites {
		if site != "api.go" {
			t.Fatalf("ErrNoSession named outside api.go in %v; refuse with noSession(cause)", sites)
		}
	}
}

// TestEveryShimlessEntryWaitsThroughHoldAndRevive pins that a prompt and a
// session act with no shim share holdAndRevive, so neither hand-rolls its own
// hold-then-revive.
func TestEveryShimlessEntryWaitsThroughHoldAndRevive(t *testing.T) {
	sourcescan.InPackage(t, internalPromptqueueDir)
	// Act
	sites := sourcescan.Sites(t, internalPromptqueueDir, "q.holdAndRevive(")

	// Assert
	if want := []string{"acts.go", "submit.go"}; !reflect.DeepEqual(sites, want) {
		t.Fatalf("holdAndRevive call sites = %v, want %v", sites, want)
	}
}

// TestEveryColdGateRefusalGoesThroughRefuseAtColdGate pins that no entry
// hand-rolls its own cold-gate refusal.
func TestEveryColdGateRefusalGoesThroughRefuseAtColdGate(t *testing.T) {
	sourcescan.InPackage(t, internalPromptqueueDir)
	// Act
	sites := sourcescan.Sites(t, internalPromptqueueDir, "&ColdGateRefusal{")

	// Assert
	if want := []string{"submit.go"}; !reflect.DeepEqual(sites, want) {
		t.Fatalf("ColdGateRefusal construction sites = %v, want %v", sites, want)
	}
}

// EVERY MERGE REFUSAL IS refuseMerging's: the held path and the arrival path
// refuse a submission under a refusing lease with the one answer.
func TestEveryMergeRefusalIsRefuseMergings(t *testing.T) {
	sourcescan.InPackage(t, internalPromptqueueDir)
	// Arrange
	fset, funcs := productionFuncs(t)

	// Act
	var found []string
	for _, fn := range funcs {
		if fn.Name.Name == "refuseMerging" {
			continue
		}
		ast.Inspect(fn, func(n ast.Node) bool {
			if kv, ok := n.(*ast.KeyValueExpr); ok {
				if key, ok := kv.Key.(*ast.Ident); ok && key.Name == "RefusedArm" {
					found = append(found, fset.Position(kv.Pos()).String()+" in "+fn.Name.Name)
				}
			}
			return true
		})
	}

	// Assert
	if len(found) > 0 {
		t.Fatalf("these functions compose a merge refusal instead of calling refuseMerging: %v", found)
	}
}

// EVERY "IS A TURN IN FLIGHT" READ OF THE WATCHER GOES THROUGH turnInFlight.
// This scans the package's production sources and fails an `if` that looks
// the watcher up in its init and reads TurnInFlight in its condition: the
// hand-rolled shape turnInFlight replaced.
func TestNoSiteHandRollsTheTurnInFlightLookup(t *testing.T) {
	sourcescan.InPackage(t, internalPromptqueueDir)
	// Arrange
	fset := token.NewFileSet()
	var violations []string

	// Act
	for _, source := range sourcescan.Production(t, internalPromptqueueDir) {
		file, err := parser.ParseFile(fset, source.Name, source.Source, 0)
		if err != nil {
			t.Fatalf("ParseFile(%s): %v", source.Name, err)
		}
		ast.Inspect(file, func(n ast.Node) bool {
			stmt, ok := n.(*ast.IfStmt)
			if ok && stmt.Init != nil && calls(stmt.Init, "Watcher") && calls(stmt.Cond, "TurnInFlight") {
				violations = append(violations, fset.Position(stmt.Pos()).String())
			}
			return true
		})
	}

	// Assert
	if len(violations) > 0 {
		t.Fatalf("hand-rolled turn-in-flight lookups, use q.turnInFlight:\n%s", strings.Join(violations, "\n"))
	}
}

// calls reports whether NODE calls a method named NAME.
func calls(node ast.Node, name string) bool {
	found := false
	ast.Inspect(node, func(n ast.Node) bool {
		if call, ok := n.(*ast.CallExpr); ok {
			if sel, ok := call.Fun.(*ast.SelectorExpr); ok && sel.Sel.Name == name {
				found = true
			}
		}
		return !found
	})
	return found
}
