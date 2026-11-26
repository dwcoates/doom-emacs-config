package daemonrules

import (
	"fmt"
	"go/ast"
	"go/parser"
	"go/token"
	"maps"
	"os"
	"path/filepath"
	"regexp"
	"slices"
	"strings"
	"testing"

	"agent-repld/reposcans/internal/sourcescan"
)

// internalServerDir is the package these rules hold, under the daemon module's root.
const internalServerDir = "internal/server"

// TestTheUserRequesterIsSpelledOnce keeps every server test asking as the user
// through userRequester: a second spelling (runtests_test.go's user() was one)
// is a copy to drift.
func TestTheUserRequesterIsSpelledOnce(t *testing.T) {
	sourcescan.InPackage(t, internalServerDir)
	// Arrange
	names, err := filepath.Glob("*_test.go")
	if err != nil {
		t.Fatalf("glob: %v", err)
	}

	// Act
	var offenders []string
	for _, name := range names {
		source, err := os.ReadFile(name)
		if err != nil {
			t.Fatalf("read %s: %v", name, err)
		}
		if name != "agentapi_helpers_test.go" && strings.Contains(string(source), "WorkspaceRequester_User{") {
			offenders = append(offenders, name)
		}
	}

	// Assert
	if len(offenders) != 0 {
		t.Fatalf("%v spell the user requester by hand; use userRequester", offenders)
	}
}

// TestInterruptAnswersEverySuccessThroughInterruptSucceeded pins that no
// Interrupt site hand-builds its success answer.
func TestInterruptAnswersEverySuccessThroughInterruptSucceeded(t *testing.T) {
	sourcescan.InPackage(t, internalServerDir)
	// Arrange.
	src, err := os.ReadFile("answers.go")
	if err != nil {
		t.Fatalf("read answers.go: %v", err)
	}

	// Act.
	sites := strings.Count(string(src), "InterruptResponse_Success{")

	// Assert.
	if sites != 1 {
		t.Fatalf("answers.go builds InterruptResponse_Success at %d sites, want only interruptSucceeded", sites)
	}
}

// TestNoTestHandBuildsTheSurfacesDeps fails a test that spells its own Deps
// literal instead of stripping the harness's complete set: a hand-built set
// passes or fails on New's check ORDER rather than on the dependency it names.
func TestNoTestHandBuildsTheSurfacesDeps(t *testing.T) {
	sourcescan.InPackage(t, internalServerDir)
	// Arrange.
	handBuilt := regexp.MustCompile(`(:= |New\()Deps\{[^}]`)
	files, err := filepath.Glob("*_test.go")
	if err != nil {
		t.Fatalf("glob: %v", err)
	}
	var offenders []string

	// Act.
	for _, file := range files {
		if file == "fakes_test.go" {
			continue
		}
		raw, err := os.ReadFile(file)
		if err != nil {
			t.Fatalf("read %s: %v", file, err)
		}
		if handBuilt.MatchString(string(raw)) {
			offenders = append(offenders, file)
		}
	}

	// Assert.
	if len(offenders) != 0 {
		t.Fatalf("hand-built Deps literals in %v; strip the harness's set with withoutDep", offenders)
	}
}

func TestEveryBackgroundCreateIsStartedByStartBackgroundCreate(t *testing.T) {
	sourcescan.InPackage(t, internalServerDir)
	// Act: the package's own production sources.
	sites := sourcescan.Sites(t, internalServerDir, "s.runBackgroundCreate(")

	// Assert: the one goroutine that runs a create detached lives in
	// startBackgroundCreate, so no op-carrying create can run it apart.
	if !slices.Equal(sites, []string{"workspaces.go"}) {
		t.Fatalf("runBackgroundCreate called from %v, want only startBackgroundCreate in workspaces.go", sites)
	}
}

// TestEveryWrapperAnswersABoundaryFailureThroughTheOneHelper fails a wrapper
// that hand-rolls its own answer instead of sharing boundaryFailure.
func TestEveryWrapperAnswersABoundaryFailureThroughTheOneHelper(t *testing.T) {
	sourcescan.InPackage(t, internalServerDir)
	// Arrange.
	raw, err := os.ReadFile("requestlog_server.go")
	if err != nil {
		t.Fatalf("read requestlog_server.go: %v", err)
	}
	source := string(raw)

	// Act.
	// A WRAPPER HAS TWO BOUNDARY STEPS: the forward to the daemon serving the
	// workspace (forwardTarget), then its own request's begin, and each one's
	// failure is a boundary failure.
	begins := strings.Count(source, "s.server.beginRequest(")
	forwards := strings.Count(source, "s.server.forwardTarget(")
	shared := strings.Count(source, "boundaryFailure(err)")

	// Assert.
	if begins == 0 || forwards != begins || begins+forwards != shared {
		t.Fatalf("%d wrappers call beginRequest, %d call forwardTarget, and %d answer through boundaryFailure; every boundary step of every wrapper must share the helper", begins, forwards, shared)
	}
	if strings.Contains(source, "connect.CodeInternal") {
		t.Fatal("a wrapper spells its own internal error instead of calling boundaryFailure")
	}
}

func TestEveryEditorPushGoesThroughPushToEditors(t *testing.T) {
	sourcescan.InPackage(t, internalServerDir)
	// Arrange / Act: only pushToEditors writes an Emacs stream's addressed queue.
	sites := sourcescan.Count(t, internalServerDir, "w.elisp <- ")

	// Assert
	if sites != 1 {
		t.Fatalf("Emacs addressed pushes are written at %d sites, want 1: pushToEditors", sites)
	}
}

func TestNoPushToEveryEditorSpellsItsOwnFilter(t *testing.T) {
	sourcescan.InPackage(t, internalServerDir)
	// Act
	sites := sourcescan.Count(t, internalServerDir, "func(*daemonWatcher) bool { return true }")

	// Assert
	if sites != 0 {
		t.Fatalf("%d push sites spell the every-editor filter inline, want 0: use everyEditor", sites)
	}
}

func TestEveryReloadElispFrameIsBuiltByReloadElispPush(t *testing.T) {
	sourcescan.InPackage(t, internalServerDir)
	// Arrange / Act: every production site that builds a reload_elisp frame.
	sites := sourcescan.Sites(t, internalServerDir, "WatchDaemonResponse_ReloadElisp{")

	// Assert: reloadElispPush's own, in deploy.go, and no other.
	if strings.Join(sites, ",") != "deploy.go" {
		t.Fatalf("reload_elisp frames built at %v, want only reloadElispPush's in deploy.go; use reloadElispPush", sites)
	}
}

func TestEveryAsyncRunIsBuiltByNewDetachedRun(t *testing.T) {
	sourcescan.InPackage(t, internalServerDir)
	// Act: the package's own production sources.
	n := sourcescan.Count(t, internalServerDir, "merge.DetachedRun{")

	// Assert: the async run is read off a request in merge.NewDetachedRun
	// alone, so this ingress cannot read it apart from the other.
	if n != 0 {
		t.Fatalf("%d hand-built merge.DetachedRun literals, want every run built by merge.NewDetachedRun", n)
	}
}

// TestEveryClientOnlyStandingTopicGoesThroughOneShape pins that the
// WatchDaemon loop takes each standing topic only one kind of client is told
// (the Emacs-only faults and persistent wifi, the webview-only news digest)
// through standingPush rather than a hand-rolled case of its own.
func TestEveryClientOnlyStandingTopicGoesThroughOneShape(t *testing.T) {
	sourcescan.InPackage(t, internalServerDir)
	// Arrange.
	src, err := os.ReadFile("streams.go")
	if err != nil {
		t.Fatalf("read streams.go: %v", err)
	}

	// Act.
	calls := strings.Count(string(src), "standingPush(s.log,")
	handRolled := strings.Count(string(src), `"a publisher raised an empty fault set`) +
		strings.Count(string(src), `"a publisher raised an empty persistent-wifi`) +
		strings.Count(string(src), `"a publisher raised an empty news digest`)

	// Assert.
	if calls != 3 || handRolled != 0 {
		t.Fatalf("standingPush call sites = %d (want 3), hand-rolled empty-value records = %d (want 0)", calls, handRolled)
	}
}

// EVERY REF CRITERION RESOLVES THROUGH registeredRef: a handler that resolves
// created_by, parent, caller or login.of_workspace by hand fails here.
func TestFindWorkspacesResolvesEveryWorkspaceRefThroughRegisteredRef(t *testing.T) {
	sourcescan.InPackage(t, internalServerDir)
	// Arrange
	src, err := os.ReadFile("findworkspaces.go")
	if err != nil {
		t.Fatalf("read the handler: %v", err)
	}

	// Act
	calls := strings.Count(string(src), "registeredRef(byID, ref)")
	byHand := strings.Count(string(src), "byID[wsm.WorkspaceID(ref.GetId())]")

	// Assert
	if calls != 4 || byHand != 1 {
		t.Fatalf("registeredRef calls = %d, hand lookups = %d; want all four refs through the helper and its own one lookup", calls, byHand)
	}
}

// TestOnlyPipeStreamCarriesAStreamFromAnotherDaemon keeps every stream carried
// from another daemon on pipeStream, so a forwarded stream and a relayed page
// subscription cannot come to state their target's acceptance differently.
func TestOnlyPipeStreamCarriesAStreamFromAnotherDaemon(t *testing.T) {
	sourcescan.InPackage(t, internalServerDir)
	// Act
	sites := sourcescan.Sites(t, internalServerDir, ".CallServerStream(")

	// Assert
	if len(sites) != 1 || sites[0] != "forward.go" {
		t.Fatalf("a server stream is opened at another daemon from %v, want only pipeStream in forward.go", sites)
	}
}

func TestAgentSentPromptIsTheOneAgentSentRequest(t *testing.T) {
	sourcescan.InPackage(t, internalServerDir)
	// Act
	sites := sourcescan.Sites(t, internalServerDir, "Origin: conversationv1.PromptOrigin_PROMPT_ORIGIN_AGENT_SENT")

	// Assert
	if len(sites) != 1 || sites[0] != "gather.go" {
		t.Fatalf("an agent-sent SubmitPrompt request is built at %v, want only agentSentPrompt in gather.go", sites)
	}
}

// TestHostReadsAreRecordedThroughOneHelper pins that every host read failure
// in host.go goes through logHostStreamRead: a hand-rolled cancelled-context
// branch beside it is the drift the helper exists to prevent.
func TestHostReadsAreRecordedThroughOneHelper(t *testing.T) {
	sourcescan.InPackage(t, internalServerDir)
	// Arrange.
	src, err := os.ReadFile("host.go")
	if err != nil {
		t.Fatalf("read host.go: %v", err)
	}

	// Act.
	branches := strings.Count(string(src), "ctxend.Is(")

	// Assert.
	if branches != 1 {
		t.Fatalf("host.go tests ctxend.Is %d times, want once, inside logHostStreamRead", branches)
	}
}

// TestEveryMinimumLevelArmIsSpelledOnlyInTheOneTable fails a second
// hand-kept mapping of the minimum level's arms in the server
// (logscli_test.go holds the CLI to none).
func TestEveryMinimumLevelArmIsSpelledOnlyInTheOneTable(t *testing.T) {
	sourcescan.InPackage(t, internalServerDir)
	// Arrange
	src, err := os.ReadFile("listlogsites.go")
	if err != nil {
		t.Fatalf("read listlogsites.go: %v", err)
	}

	// Act
	got := strings.Count(string(src), "ListLogSitesMinimumLevel_")

	// Assert
	if got != 4 {
		t.Fatalf("listlogsites.go spells a minimum level arm %d times, want 4, each once in logSitesMinimums", got)
	}
}

// TestPageTestsSubscribeThroughTheHelper keeps every subscription a test
// expects to be accepted on subscribePage, so the accepted path is spelled
// once: a hand-rolled SubscribePage that fails the test on its error is a
// second copy of the helper.
func TestPageTestsSubscribeThroughTheHelper(t *testing.T) {
	sourcescan.InPackage(t, internalServerDir)
	// Arrange
	helperDef := regexp.MustCompile(`(?s)\nfunc subscribePage\(.*?\n\}\n`)
	handRolled := regexp.MustCompile(`if _, err := h\.Client\.SubscribePage\(ctx, connect\.NewRequest\([^;]*?\)\); err != nil \{\n\s*t\.Fatalf`)
	names, err := filepath.Glob("*_test.go")
	if err != nil {
		t.Fatalf("glob: %v", err)
	}

	// Act
	var offenders []string
	for _, name := range names {
		source, err := os.ReadFile(name)
		if err != nil {
			t.Fatalf("read %s: %v", name, err)
		}
		// The helper's own body is the one spelling the rule allows.
		if handRolled.Match(helperDef.ReplaceAll(source, nil)) {
			offenders = append(offenders, name)
		}
	}

	// Assert
	if len(offenders) != 0 {
		t.Fatalf("%v subscribe a page by hand; use subscribePage", offenders)
	}
}

func TestEveryRefListIsResolvedThroughResolveAllNaming(t *testing.T) {
	sourcescan.InPackage(t, internalServerDir)
	// Arrange: a loop over refs calling resolveNaming is resolveAllNaming
	// written again.
	handRolled := regexp.MustCompile(`for _, ref := range [^\n]*\n[^\n]*resolveNaming\(`)
	var offenders []string

	// Act.
	for _, file := range sourcescan.Production(t, internalServerDir) {
		if handRolled.Match(file.Source) && file.Name != "workspaces.go" {
			offenders = append(offenders, file.Name)
		}
	}
	occurrences := 0
	for _, file := range sourcescan.Production(t, internalServerDir) {
		occurrences += len(handRolled.FindAll(file.Source, -1))
	}

	// Assert: the one loop left is resolveAllNaming's own.
	if len(offenders) != 0 || occurrences != 1 {
		t.Fatalf("hand-rolled ref loops in %v (%d in all), want only resolveAllNaming's", offenders, occurrences)
	}
}

// TestNoAccessorHandRollsPerWorkspace pins that every server method answering
// a *publish.Topic or *publish.Events for a workspace is a bare call of
// perWorkspace, so no accessor reimplements the lock-and-mint by hand.
func TestNoAccessorHandRollsPerWorkspace(t *testing.T) {
	sourcescan.InPackage(t, internalServerDir)
	// Arrange.
	fset := token.NewFileSet()
	checked := 0

	// Act and Assert.
	for _, f := range sourcescan.Production(t, internalServerDir) {
		name, src := f.Name, f.Source
		file, err := parser.ParseFile(fset, name, src, 0)
		if err != nil {
			t.Fatalf("parse %s: %v", name, err)
		}
		for _, decl := range file.Decls {
			fn, ok := decl.(*ast.FuncDecl)
			if !ok || fn.Recv == nil || fn.Type.Results == nil || len(fn.Type.Results.List) != 1 {
				continue
			}
			result := string(src[fset.Position(fn.Type.Results.Pos()).Offset:fset.Position(fn.Type.Results.End()).Offset])
			params := fn.Type.Params.List
			if !strings.HasPrefix(result, "*publish.") || len(params) != 1 ||
				string(src[fset.Position(params[0].Type.Pos()).Offset:fset.Position(params[0].Type.End()).Offset]) != "ids.WorkspaceID" {
				continue
			}
			checked++
			body := string(src[fset.Position(fn.Body.Lbrace).Offset+1 : fset.Position(fn.Body.Rbrace).Offset])
			if !strings.HasPrefix(strings.TrimSpace(body), "return perWorkspace(s, s.") || len(fn.Body.List) != 1 {
				t.Errorf("%s: %s hand-rolls a per-workspace accessor; call perWorkspace", name, fn.Name.Name)
			}
		}
	}
	if checked == 0 {
		t.Fatal("the scan found no per-workspace accessor, so it checks nothing")
	}
}

// THE CARD VERBS SHARE ONE PROLOGUE (askRow): a handler that decodes its row by
// hand drifts from the others' refusals, so the source is held to the helper.
func TestEveryCardVerbDecodesItsRowThroughAskRow(t *testing.T) {
	sourcescan.InPackage(t, internalServerDir)
	// Arrange.
	raw, err := os.ReadFile("answers.go")
	if err != nil {
		t.Fatalf("read answers.go: %v", err)
	}
	src := string(raw)

	// Act / Assert: each card verb calls askRow, and askIDFrom is called only
	// by askRow and by interruptRow, whose refusal arm is the target's own.
	for _, rpc := range []string{"AnswerPermission", "AnswerQuestion", "UpdateQuestionDraft"} {
		body := funcBody(t, src, "func (s *server) "+rpc+"(")
		if !strings.Contains(body, "s.askRow(") || strings.Contains(body, "askIDFrom(") {
			t.Errorf("%s does not decode its row through askRow", rpc)
		}
	}
	if n := strings.Count(src, ":= askIDFrom(rpc"); n != 2 {
		t.Errorf("askIDFrom is called %d times, want 2 (askRow and interruptRow)", n)
	}
}

// funcBody answers the text of the top-level function starting at header, up
// to the next top-level declaration.
func funcBody(t *testing.T, src, header string) string {
	t.Helper()
	start := strings.Index(src, header)
	if start < 0 {
		t.Fatalf("no %q in the source", header)
	}
	rest := src[start+len(header):]
	if end := strings.Index(rest, "\n}\n"); end >= 0 {
		return rest[:end]
	}
	return rest
}

// TestEveryStreamReaderIsAPickOverReceivePush pins that no receive* test
// reader loops over a stream by hand: each one hands receivePush its pick, so
// the skip-until-found loop and its failure message exist once.
func TestEveryStreamReaderIsAPickOverReceivePush(t *testing.T) {
	sourcescan.InPackage(t, internalServerDir)
	// Arrange
	files, err := filepath.Glob("*_test.go")
	if err != nil {
		t.Fatalf("glob: %v", err)
	}
	fset := token.NewFileSet()
	var handRolled []string

	// Act
	for _, name := range files {
		file, err := parser.ParseFile(fset, name, nil, 0)
		if err != nil {
			t.Fatalf("parse %s: %v", name, err)
		}
		for _, decl := range file.Decls {
			fn, ok := decl.(*ast.FuncDecl)
			if !ok || fn.Body == nil || !strings.HasPrefix(fn.Name.Name, "receive") || fn.Name.Name == "receivePush" {
				continue
			}
			ast.Inspect(fn.Body, func(n ast.Node) bool {
				if loop, ok := n.(*ast.ForStmt); ok && callsReceive(loop.Cond) {
					handRolled = append(handRolled, fmt.Sprintf("%s:%s", name, fn.Name.Name))
				}
				return true
			})
		}
	}

	// Assert
	if len(handRolled) != 0 {
		t.Fatalf("readers looping over a stream by hand: %v; hand receivePush a pick instead", handRolled)
	}
}

// callsReceive reports whether EXPR is a `<x>.Receive()` call.
func callsReceive(expr ast.Expr) bool {
	call, ok := expr.(*ast.CallExpr)
	if !ok {
		return false
	}
	sel, ok := call.Fun.(*ast.SelectorExpr)
	return ok && sel.Sel.Name == "Receive"
}

// TestEveryResolutionReadGoesThroughTheRegistryGate fails a resolution that
// reads the registry on its own, outside the gate Close takes: each reads the
// workspace's head through its shared helper (registryHead, or
// resolutionHead, which takes the boundary's handed read or reads through
// registryHead), never the state client directly.
func TestEveryResolutionReadGoesThroughTheRegistryGate(t *testing.T) {
	sourcescan.InPackage(t, internalServerDir)
	// readsInside marks the helpers the read itself is written in: their read
	// of the state client is the closure they hand readRegistryValue.
	cases := []struct {
		file, function, helper string
		readsInside            bool
	}{
		{"refuse.go", "resolveRegistered", "s.resolutionHead(", false},
		{"requestlog.go", "beginRequest", "s.registryHead(", false},
		{"refuse.go", "resolutionHead", "s.registryHead(", false},
		{"refuse.go", "registryHead", "readRegistryValue(", true},
		{"refuse.go", "registryWorkspace", "readRegistryValue(", true},
		{"refuse.go", "readRegistryValue", "s.readRegistry(", false},
	}
	for _, tc := range cases {
		t.Run(tc.function, func(t *testing.T) {
			// Arrange.
			fset := token.NewFileSet()
			parsed, err := parser.ParseFile(fset, tc.file, nil, 0)
			if err != nil {
				t.Fatalf("parse %s: %v", tc.file, err)
			}
			var body string
			for _, decl := range parsed.Decls {
				if fn, ok := decl.(*ast.FuncDecl); ok && fn.Name.Name == tc.function {
					raw, readErr := os.ReadFile(tc.file)
					if readErr != nil {
						t.Fatalf("read %s: %v", tc.file, readErr)
					}
					body = string(raw[fset.Position(fn.Body.Pos()).Offset:fset.Position(fn.Body.End()).Offset])
				}
			}
			if body == "" {
				t.Fatalf("%s declares no %s", tc.file, tc.function)
			}

			// Act.
			direct := !tc.readsInside && (strings.Contains(body, "s.deps.DB.Workspace(") || strings.Contains(body, "s.deps.DB.WorkspaceHead("))
			shared := strings.Contains(body, tc.helper)

			// Assert.
			if direct || !shared {
				t.Fatalf("%s: direct registry read %v, shared helper %v; its registry read must go through %s", tc.function, direct, shared, tc.helper)
			}
		})
	}
}

// TestDetachedFailuresMapTheirRefusalOnlyThroughRefuseOnto pins that every
// detached mutation maps its typed refusal through the one helper, so no site
// hand-rolls `asRefusal` then `refuse(...) == nil` and drifts from the others.
func TestDetachedFailuresMapTheirRefusalOnlyThroughRefuseOnto(t *testing.T) {
	sourcescan.InPackage(t, internalServerDir)
	// Arrange.
	entries, err := os.ReadDir(".")
	if err != nil {
		t.Fatalf("read the package: %v", err)
	}

	// Act.
	var offenders []string
	for _, entry := range entries {
		name := entry.Name()
		if !strings.HasSuffix(name, ".go") || strings.HasSuffix(name, "_test.go") {
			continue
		}
		src, err := os.ReadFile(name)
		if err != nil {
			t.Fatalf("read %s: %v", name, err)
		}
		for _, line := range strings.Split(string(src), "\n") {
			if strings.Contains(line, "s.refuse(") && strings.Contains(line, "== nil") && !strings.Contains(line, "return s.refuse(") {
				offenders = append(offenders, name+": "+strings.TrimSpace(line))
			}
		}
	}

	// Assert.
	if len(offenders) > 0 {
		t.Fatalf("hand-rolled typed-refusal mapping, use refuseOnto: %v", offenders)
	}
}

// TestNoResolutionFailureIsRenderedThroughFail fails a handler that renders a
// workspace resolution's error through fail. resolveRef already recorded that
// error where it was decided, and a cancelled caller is no failure, so fail's
// ERROR re-recorded it and named the caller leaving a daemon failure (an
// OpenFeed cancelled during the 0.439.1 handover). failResolution, endStream
// and answerResolution are the renderings that know both.
func TestNoResolutionFailureIsRenderedThroughFail(t *testing.T) {
	sourcescan.InPackage(t, internalServerDir)
	// Arrange.
	files := sourcescan.Production(t, internalServerDir)
	fset := token.NewFileSet()
	var sites, offenders []string

	// Act.
	for _, file := range files {
		parsed, parseErr := parser.ParseFile(fset, file.Name, file.Source, 0)
		if parseErr != nil {
			t.Fatalf("parse %s: %v", file.Name, parseErr)
		}
		ast.Inspect(parsed, func(n ast.Node) bool {
			block, ok := n.(*ast.BlockStmt)
			if !ok {
				return true
			}
			for i, stmt := range block.List {
				if !resolvesAWorkspace(stmt) || i+1 == len(block.List) {
					continue
				}
				site := fset.Position(stmt.Pos()).String()
				sites = append(sites, site)
				ast.Inspect(block.List[i+1], func(m ast.Node) bool {
					if call, ok := m.(*ast.CallExpr); ok {
						if fn, ok := call.Fun.(*ast.Ident); ok && fn.Name == "fail" {
							offenders = append(offenders, site)
						}
					}
					return true
				})
			}
			return true
		})
	}

	// Assert.
	if len(sites) == 0 {
		t.Fatal("found no workspace resolution call site; the scan no longer matches the code")
	}
	if len(offenders) != 0 {
		t.Fatalf("these resolutions render their error through fail, want failResolution: %v", offenders)
	}
}

// resolvesAWorkspace reports whether stmt assigns the answer of s.resolveRef or
// s.resolveRegistered.
func resolvesAWorkspace(stmt ast.Stmt) bool {
	assign, ok := stmt.(*ast.AssignStmt)
	if !ok || len(assign.Rhs) != 1 {
		return false
	}
	call, ok := assign.Rhs[0].(*ast.CallExpr)
	if !ok {
		return false
	}
	sel, ok := call.Fun.(*ast.SelectorExpr)
	return ok && (sel.Sel.Name == "resolveRef" || sel.Sel.Name == "resolveRegistered")
}

// TestEveryWrapperIsOneCallToTheSharedBoundary fails a wrapper that hand-rolls
// its own boundary instead of delegating through serveUnary or serveStream.
func TestEveryWrapperIsOneCallToTheSharedBoundary(t *testing.T) {
	sourcescan.InPackage(t, internalServerDir)
	// Arrange.
	raw, err := os.ReadFile("requestlog_server.go")
	if err != nil {
		t.Fatalf("read requestlog_server.go: %v", err)
	}
	source := string(raw)

	// Act.
	wrappers := strings.Count(source, "func (s *requestLoggingServer) ")
	shared := strings.Count(source, "return serveUnary(s, ctx, ") + strings.Count(source, "return serveStream(s, ctx, ")

	// Assert.
	if wrappers == 0 || wrappers != shared {
		t.Fatalf("%d wrappers and %d delegate through serveUnary or serveStream; every wrapper must", wrappers, shared)
	}
	if got := strings.Count(source, "s.server.beginRequest("); got != 2 {
		t.Fatalf("beginRequest is called %d times, want once in each shared boundary", got)
	}
}

// EVERY STOP THAT TOOK A PROMPT BACK ANSWERS THROUGH rolledBackUnanswered, so
// a hand-built rolled_back_unanswered arm in the server's source fails here.
func TestEveryRolledBackUnansweredAnswerIsBuiltByTheHelper(t *testing.T) {
	sourcescan.InPackage(t, internalServerDir)
	// Arrange.
	entries, err := os.ReadDir(".")
	if err != nil {
		t.Fatalf("read the server's source: %v", err)
	}
	var builds int

	// Act.
	for _, e := range entries {
		if !strings.HasSuffix(e.Name(), ".go") || strings.HasSuffix(e.Name(), "_test.go") {
			continue
		}
		raw, err := os.ReadFile(e.Name())
		if err != nil {
			t.Fatalf("read %s: %v", e.Name(), err)
		}
		builds += strings.Count(string(raw), "&agentreplv1.InterruptSuccess_RolledBackUnanswered{")
	}

	// Assert.
	if builds != 1 {
		t.Fatalf("the rolled_back_unanswered arm is built %d times, want once, in rolledBackUnanswered", builds)
	}
}

// EVERY PROMPT HANDED BACK TO THE COMPOSER IS BUILT BY composerWords, so a
// hand-built handback in the server's source fails here.
func TestEveryComposerHandbackIsBuiltByTheHelper(t *testing.T) {
	sourcescan.InPackage(t, internalServerDir)
	// Arrange.
	entries, err := os.ReadDir(".")
	if err != nil {
		t.Fatalf("read the server's source: %v", err)
	}
	var handbacks, helped int

	// Act.
	for _, e := range entries {
		if !strings.HasSuffix(e.Name(), ".go") || strings.HasSuffix(e.Name(), "_test.go") {
			continue
		}
		raw, err := os.ReadFile(e.Name())
		if err != nil {
			t.Fatalf("read %s: %v", e.Name(), err)
		}
		src := string(raw)
		handbacks += strings.Count(src, "&agentreplv1.RollBackSuccess{") + strings.Count(src, "&agentreplv1.InterruptRolledBackUnanswered{")
		helped += strings.Count(src, "&agentreplv1.RollBackSuccess{Prompt: composerWords(") +
			strings.Count(src, "&agentreplv1.InterruptRolledBackUnanswered{Prompt: composerWords(")
	}

	// Assert.
	if handbacks != 2 || helped != handbacks {
		t.Fatalf("composer handbacks = %d, built by composerWords = %d; want 2, both by composerWords", handbacks, helped)
	}
}

// THE TEST ENDPOINTS RESOLVE THEIR WORKTREE ONE WAY: through testWorktreeOf,
// never a hand-rolled resolve and script read of their own.
func TestTheTestEndpointsResolveThroughTestWorktreeOf(t *testing.T) {
	sourcescan.InPackage(t, internalServerDir)
	for _, file := range []string{"runtests.go", "runredundancy.go"} {
		// Arrange
		src, err := os.ReadFile(file)
		if err != nil {
			t.Fatal(err)
		}
		body := string(src)
		if file == "runtests.go" {
			body = body[:strings.Index(body, "func (s *server) testWorktreeOf(")]
		}

		// Act
		handRolled := strings.Contains(body, "s.resolveRef(") || strings.Contains(body, "testScriptOf(")

		// Assert
		if handRolled || !strings.Contains(body, "s.testWorktreeOf(") {
			t.Fatalf("%s resolves its worktree by hand; use testWorktreeOf", file)
		}
	}
}

// TestSelectFeedRowTestsBuildTheirLoggedHarnessThroughTheHelper pins that the
// recording-logger wiring lives only in newLoggedHarness, so no test in this
// file hand-rolls a variant that drifts from it.
func TestSelectFeedRowTestsBuildTheirLoggedHarnessThroughTheHelper(t *testing.T) {
	sourcescan.InPackage(t, internalServerDir)
	// Arrange.
	src, err := os.ReadFile("select_feed_row_test.go")
	if err != nil {
		t.Fatalf("read the test source: %v", err)
	}

	// Act.
	// The needle is split so this line is not itself counted.
	got := strings.Count(string(src), "deps.Log = &fakeSurfaces"+"{workspace: log, global: log}")

	// Assert.
	if got != 1 {
		t.Fatalf("hand-rolled logged harnesses = %d, want only newLoggedHarness's own", got)
	}
}

// TestEveryClickSelectsThroughSelectClicked pins that each click handler that
// selects (bubble, answer, entry, and a branch of an older response) selects
// through selectClicked, so none hand-rolls the lock and the store.
func TestEveryClickSelectsThroughSelectClicked(t *testing.T) {
	sourcescan.InPackage(t, internalServerDir)
	// Arrange.
	src, err := os.ReadFile("select_feed_row.go")
	if err != nil {
		t.Fatalf("read select_feed_row.go: %v", err)
	}

	// Act.
	calls := strings.Count(string(src), "s.selectClicked(")
	builds := strings.Count(string(src), "selectionOf(kind, row,")

	// Assert: four handlers call it, and only it builds a clicked selection.
	if calls != 4 || builds != 1 {
		t.Fatalf("select_feed_row.go calls selectClicked %d times and builds a clicked selection %d times, want 4 and 1", calls, builds)
	}
}

// TestEveryEndingStreamServesThroughServeThenEnd pins that no stream re-spells
// serveThenEnd: outside it, endStandingStream is called only by the page's own
// drain and the host stream, whose endings follow work of their own.
func TestEveryEndingStreamServesThroughServeThenEnd(t *testing.T) {
	sourcescan.InPackage(t, internalServerDir)
	// Act.
	sites := sourcescan.Sites(t, internalServerDir, "\tendStandingStream(s, ")
	sends := sourcescan.Sites(t, internalServerDir, "\tsendStandingEnding(")

	// Assert: the lifetime's ending is serveThenEnd's and the host stream's;
	// the page's drain sends its ending through the one sender at the exit
	// and at the hand-off to the successor alike.
	want := []string{"streams.go", "streams.go"}
	if !slices.Equal(sites, want) {
		t.Fatalf("endStandingStream call sites = %v, want %v (serveThenEnd, the host stream)", sites, want)
	}
	if wantSends := []string{"page.go", "streams.go"}; !slices.Equal(sends, wantSends) {
		t.Fatalf("sendStandingEnding call sites = %v, want %v (the page's drain, endStandingStream)", sends, wantSends)
	}
}

// TestEveryHostStreamDialInTheseTestsGoesThroughTheHelpers pins the shared
// shape: a test that opens the workspace's host stream and fails on a dial
// error does it through dialHostStream, so a hand-rolled copy fails here.
func TestEveryHostStreamDialInTheseTestsGoesThroughTheHelpers(t *testing.T) {
	sourcescan.InPackage(t, internalServerDir)
	// Arrange.
	files, err := filepath.Glob("*_test.go")
	if err != nil {
		t.Fatalf("list the test files: %v", err)
	}
	handRolled := regexp.MustCompile(`h\.Client\.WatchHostWorkspace\(ctx,\s*connect\.NewRequest\(&agentreplv1\.WatchHostWorkspaceRequest\{\s*Workspace: ref\(\),?\s*\}\)\)\s*if \w+ != nil \{\s*t\.Fatalf`)

	// Act.
	counts := map[string]int{}
	for _, file := range files {
		src, err := os.ReadFile(file)
		if err != nil {
			t.Fatalf("read %s: %v", file, err)
		}
		if n := len(handRolled.FindAllIndex(src, -1)); n > 0 {
			counts[file] = n
		}
	}

	// Assert: dialHostStream's own body is the one site.
	want := map[string]int{"streams_test.go": 1}
	if !maps.Equal(counts, want) {
		t.Fatalf("hand-rolled host stream dials = %v, want only dialHostStream's own %v; use dialHostStream or openHostStream", counts, want)
	}
}

// TestEveryParticipantEdgeStatesItsHopsThroughOneShape pins the shared shape:
// the hops are recorded only by participantHops.fields and stated to the
// resolvers only by publishParticipants, so a hand-rolled site that reads the
// held counts itself fails here rather than drawing its own connectivity.
func TestEveryParticipantEdgeStatesItsHopsThroughOneShape(t *testing.T) {
	sourcescan.InPackage(t, internalServerDir)
	tests := []struct {
		name   string
		substr string
		want   []string
	}{
		{name: "the hop record", substr: `"web_hop":`, want: []string{"streams.go"}},
		{name: "the footer's hops", substr: "Footer.SetParticipants(", want: []string{"streams.go"}},
		{name: "the topbar's hops", substr: "Topbar.SetParticipants(", want: []string{"streams.go"}},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Act.
			sites := sourcescan.Sites(t, internalServerDir, tt.substr)

			// Assert.
			if !slices.Equal(sites, tt.want) {
				t.Fatalf("%q sites = %v, want %v; read the hops through participantHopsLocked", tt.substr, sites, tt.want)
			}
		})
	}
}

// TestFindTakesSummaryFields fails when the find read maps a stored summary
// to its wire fields by hand instead of through workspace.SummaryFields.
func TestFindTakesSummaryFields(t *testing.T) {
	sourcescan.InPackage(t, internalServerDir)
	// Act
	callers := sourcescan.Sites(t, internalServerDir, "workspace.SummaryFields(")
	handRolled := sourcescan.Sites(t, internalServerDir, "LongSummary = &")

	// Assert: the find read maps a summary through the one mapping.
	if len(callers) != 1 || callers[0] != "findworkspaces.go" || len(handRolled) != 0 {
		t.Fatalf("SummaryFields sites = %v, hand-rolled = %v; want the find read alone", callers, handRolled)
	}
}
