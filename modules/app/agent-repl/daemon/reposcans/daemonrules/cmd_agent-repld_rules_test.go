package daemonrules

import (
	"go/ast"
	"go/parser"
	"go/token"
	"os"
	"path/filepath"
	"reflect"
	"regexp"
	"slices"
	"strings"
	"testing"

	"google.golang.org/protobuf/reflect/protoreflect"
	"google.golang.org/protobuf/reflect/protoregistry"

	"agent-repld/reposcans/internal/sourcescan"
	"agentrepl/logging/profiling/profilingtest"
	_ "agentrepl/proto/agentrepl/v1" // registers the agentrepl.v1 messages the rules look up
	"agentrepl/proto/agentrepl/v1/agentreplv1connect"
)

// cmdAgentRepldDir is the package these rules hold, under the daemon module's root.
const cmdAgentRepldDir = "cmd/agent-repld"

func TestEveryWorkspaceRequesterIsBuiltByCallingRequester(t *testing.T) {
	sourcescan.InPackage(t, cmdAgentRepldDir)
	// Arrange
	src, err := os.ReadFile("agentcli.go")
	if err != nil {
		t.Fatal(err)
	}

	// Assert: the one place a calling workspace becomes a requester.
	if got := strings.Count(string(src), "&agentreplv1.WorkspaceRequesterWorkspace{"); got != 1 {
		t.Fatalf("agentcli.go builds %d WorkspaceRequesterWorkspace values; want only callingRequester's", got)
	}
}

// EVERY ONE-WORKSPACE VERB GOES THROUGH onWorkspace: a verb that parses its
// one workspace by hand and then makes a single call fails here.
func TestEveryOneWorkspaceVerbIsOnWorkspace(t *testing.T) {
	sourcescan.InPackage(t, cmdAgentRepldDir)
	// Arrange
	var raw []byte
	for _, file := range sourcescan.Production(t, cmdAgentRepldDir) {
		if file.Name == "agentcli.go" {
			raw = file.Source
		}
	}
	verb := regexp.MustCompile(`(?s)func workspace(Kill|Select)\(c \*cliCall, args \[\]string\) int \{\n(.*?)\n}\n`)

	// Act
	matches := verb.FindAllSubmatch(raw, -1)

	// Assert
	if len(matches) == 0 {
		t.Fatal("found no one-workspace verb to check")
	}
	for _, m := range matches {
		if !strings.HasPrefix(strings.TrimSpace(string(m[2])), "return onWorkspace(") {
			t.Errorf("workspace%s parses its workspace by hand instead of onWorkspace", m[1])
		}
	}
}

// A ONE-WORKSPACE VERB WITH FLAGS OF ITS OWN PARSES THROUGH onlyWorkspace, the
// parse onWorkspace runs, so its refusal of anything after the workspace
// cannot drift from onWorkspace's.
func TestEveryFlaggedOneWorkspaceVerbParsesThroughOnlyWorkspace(t *testing.T) {
	sourcescan.InPackage(t, cmdAgentRepldDir)
	// Arrange
	var raw []byte
	for _, file := range sourcescan.Production(t, cmdAgentRepldDir) {
		if file.Name == "agentcli.go" {
			raw = file.Source
		}
	}
	body := regexp.MustCompile(`(?s)func (onWorkspace\[Res any\]|workspaceRestart)\(.*?\) int \{\n(.*?)\n}\n`)

	// Act
	matches := body.FindAllSubmatch(raw, -1)

	// Assert
	if len(matches) != 2 {
		t.Fatalf("found %d of onWorkspace and workspaceRestart, want both", len(matches))
	}
	for _, m := range matches {
		if !strings.Contains(string(m[2]), "c.onlyWorkspace(") {
			t.Errorf("%s parses its workspace without onlyWorkspace", m[1])
		}
	}
}

func TestTheTargetedVerbsShareOneArgumentPreamble(t *testing.T) {
	sourcescan.InPackage(t, cmdAgentRepldDir)
	// Arrange: the parse-then-dial preamble lives in targetArgs alone.
	src, err := os.ReadFile("agentcli.go")
	if err != nil {
		t.Fatalf("read agentcli.go: %v", err)
	}

	// Act
	handRolled := strings.Count(string(src), "c.usageFailed(\"the command names its workspace\")") +
		strings.Count(string(src), "c.usageFailed(\"the command names its merge\")")

	// Assert
	if handRolled != 0 || strings.Count(string(src), "c.targetArgs(fs, stateDir, args, ") != 2 {
		t.Fatalf("usage refusals %d, targetArgs calls %d; want the one preamble shared by workspaceArg and queuedArg",
			handRolled, strings.Count(string(src), "c.targetArgs(fs, stateDir, args, "))
	}
}

func TestEveryRepositoryFlagIsReadByRepositoryFlag(t *testing.T) {
	sourcescan.InPackage(t, cmdAgentRepldDir)
	// Arrange
	src, err := os.ReadFile("agentcli.go")
	if err != nil {
		t.Fatal(err)
	}

	// Assert
	if n := strings.Count(string(src), `strings.Cut(*repository, "/")`); n != 0 {
		t.Fatalf("%d -repository flags are split by hand; read every one through repositoryFlag", n)
	}
}

// describerName is the shape of a refusal describer's name.
var describerName = regexp.MustCompile(`^describe\w+Refusal$`)

// scanDescribedAnswers reads every answerDescribed and onWorkspace call in
// the production sources and answers the response each describes (the
// agentrepl.v1 message its describer closure takes), sorted and once each,
// with every call site that does not name a describer: a closure that is not
// one return of a describe…Refusal call, so a constant sentence such as the
// old "the daemon refused" cannot come back.
func scanDescribedAnswers(t *testing.T) (responses, problems []string) {
	t.Helper()
	fset := token.NewFileSet()
	seen := map[string]bool{}
	for _, file := range sourcescan.Production(t, cmdAgentRepldDir) {
		parsed, err := parser.ParseFile(fset, file.Name, file.Source, 0)
		if err != nil {
			t.Fatalf("parse %s: %v", file.Name, err)
		}
		for _, decl := range parsed.Decls {
			fn, ok := decl.(*ast.FuncDecl)
			if !ok {
				continue
			}
			inOnWorkspace := fn.Name.Name == "onWorkspace"
			ast.Inspect(fn, func(n ast.Node) bool {
				call, ok := n.(*ast.CallExpr)
				if !ok {
					return true
				}
				callee, ok := call.Fun.(*ast.Ident)
				if !ok || (callee.Name != "answerDescribed" && callee.Name != "onWorkspace") || len(call.Args) == 0 {
					return true
				}
				where := fset.Position(call.Pos()).String()
				lit, ok := call.Args[len(call.Args)-1].(*ast.FuncLit)
				if !ok {
					problems = append(problems, where+": the describer is not a closure")
					return true
				}
				params := lit.Type.Params.List
				last := params[len(params)-1]
				star, ok := last.Type.(*ast.StarExpr)
				if !ok {
					problems = append(problems, where+": the closure takes no response")
					return true
				}
				generic := false
				switch typ := star.X.(type) {
				case *ast.SelectorExpr:
					if pkg, ok := typ.X.(*ast.Ident); !ok || pkg.Name != "agentreplv1" {
						problems = append(problems, where+": the closure takes no agentrepl.v1 response")
						return true
					}
					if !seen[typ.Sel.Name] {
						seen[typ.Sel.Name] = true
						responses = append(responses, typ.Sel.Name)
					}
				case *ast.Ident:
					generic = inOnWorkspace && typ.Name == "Res"
				}
				if len(lit.Body.List) != 1 {
					problems = append(problems, where+": the closure is not one return")
					return true
				}
				ret, ok := lit.Body.List[0].(*ast.ReturnStmt)
				if !ok || len(ret.Results) != 1 {
					problems = append(problems, where+": the closure is not one return")
					return true
				}
				said, ok := ret.Results[0].(*ast.CallExpr)
				if !ok {
					problems = append(problems, where+": the closure returns no describer's sentence")
					return true
				}
				name, ok := said.Fun.(*ast.Ident)
				if !ok || !(describerName.MatchString(name.Name) || (generic && name.Name == "why")) {
					problems = append(problems, where+": the closure returns no describe…Refusal call")
				}
				return true
			})
		}
	}
	slices.Sort(responses)
	return responses, problems
}

// TestEveryAnsweredRefusalNamesItsDescriber pins that every verb answers
// through a describer of its rpc's error: no call site says a refusal with a
// sentence of its own, so none can fall back to "the daemon refused".
func TestEveryAnsweredRefusalNamesItsDescriber(t *testing.T) {
	sourcescan.InPackage(t, cmdAgentRepldDir)
	// Act
	responses, problems := scanDescribedAnswers(t)

	// Assert
	if len(responses) == 0 {
		t.Fatal("found no answerDescribed call site to check")
	}
	for _, p := range problems {
		t.Error(p)
	}
}

func TestEveryTimeFlagIsReadThroughWhenFlag(t *testing.T) {
	sourcescan.InPackage(t, cmdAgentRepldDir)
	// Arrange
	body, err := os.ReadFile("agentcli.go")
	if err != nil {
		t.Fatal(err)
	}

	// Act
	calls := strings.Count(string(body), "parseWhen(value, c.deps.now())")
	all := strings.Count(string(body), "parseWhen(")

	// Assert: the definition and whenFlag's one call.
	if calls != 1 || all != 2 {
		t.Fatalf("parseWhen is called %d times (%d through whenFlag); want only whenFlag to call it", all-1, calls)
	}
}

// TestEveryCallerOnlyFixtureSharesAtCaller fails a test file that hand-rolls
// the caller seams atCaller owns, so the copies cannot drift apart.
func TestEveryCallerOnlyFixtureSharesAtCaller(t *testing.T) {
	sourcescan.InPackage(t, cmdAgentRepldDir)
	// Arrange
	files, err := filepath.Glob("*_test.go")
	if err != nil {
		t.Fatal(err)
	}
	const shape = `locateOver(cliRow("ws-caller", "/wt/caller", "caller"))`

	for _, file := range files {
		if file == "caller_test.go" {
			continue
		}
		// Act
		src, err := os.ReadFile(file)
		if err != nil {
			t.Fatal(err)
		}

		// Assert
		if strings.Contains(string(src), shape) {
			t.Errorf("%s hand-rolls the caller seams; use atCaller", file)
		}
	}
}

// serverStreamMethods names the daemon client's server-streaming methods.
func serverStreamMethods() map[string]bool {
	client := reflect.TypeOf((*agentreplv1connect.AgentReplClient)(nil)).Elem()
	methods := map[string]bool{}
	for i := 0; i < client.NumMethod(); i++ {
		m := client.Method(i)
		if m.Type.NumOut() > 0 && strings.Contains(m.Type.Out(0).String(), "ServerStreamForClient") {
			methods[m.Name] = true
		}
	}
	return methods
}

func TestEveryVerbOpensItsServerStreamsThroughOpenDaemonStream(t *testing.T) {
	sourcescan.InPackage(t, cmdAgentRepldDir)
	// Arrange.
	streaming := serverStreamMethods()
	if len(streaming) == 0 {
		t.Fatal("the daemon client has no server-streaming method; the scan below would pass vacuously")
	}
	fset := token.NewFileSet()
	var direct []string

	// Act.
	for _, source := range sourcescan.Production(t, cmdAgentRepldDir) {
		file, err := parser.ParseFile(fset, source.Name, source.Source, 0)
		if err != nil {
			t.Fatalf("parse %s: %v", source.Name, err)
		}
		ast.Inspect(file, func(n ast.Node) bool {
			call, ok := n.(*ast.CallExpr)
			if !ok {
				return true
			}
			if sel, ok := call.Fun.(*ast.SelectorExpr); ok && streaming[sel.Sel.Name] {
				direct = append(direct, fset.Position(call.Pos()).String()+" "+sel.Sel.Name)
			}
			return true
		})
	}

	// Assert.
	if len(direct) > 0 {
		t.Fatalf("server streams opened by a direct call, so their Close can drain a standing stream; open them with openDaemonStream:\n%s",
			strings.Join(direct, "\n"))
	}
}

// TestNotifyProgramsResolveThroughOneHelper fails a resolver that hand-rolls
// its boot record instead of asking resolveNotifyProgram.
func TestNotifyProgramsResolveThroughOneHelper(t *testing.T) {
	sourcescan.InPackage(t, cmdAgentRepldDir)
	// Arrange
	src, err := os.ReadFile("desktopnotify.go")
	if err != nil {
		t.Fatalf("read desktopnotify.go: %v", err)
	}

	// Act
	asks := strings.Count(string(src), "return resolveNotifyProgram(")
	records := strings.Count(string(src), "log.Error(opDesktopNotify")

	// Assert
	if asks != 2 || records != 1 {
		t.Fatalf("resolveNotifyProgram( x%d (want 2), log.Error(opDesktopNotify x%d (want 1)", asks, records)
	}
}

// TestEveryEarlyRegistryRepublishGoesThroughTheForwarder pins the shared
// shape: a component built before the verbs republishes the roster through
// verbsForwarder.publishRegistry, never a hand-rolled closure of its own.
func TestEveryEarlyRegistryRepublishGoesThroughTheForwarder(t *testing.T) {
	sourcescan.InPackage(t, cmdAgentRepldDir)
	// Arrange.
	source, err := os.ReadFile("graph.go")
	if err != nil {
		t.Fatalf("read graph.go: %v", err)
	}

	// Act.
	handRolled := strings.Count(string(source), "verbsRef.verbs()")

	// Assert.
	if handRolled != 0 {
		t.Fatalf("graph.go reads verbsRef.verbs() %d times, want every republish through verbsRef.publishRegistry", handRolled)
	}
}

func TestEveryForwardedVerbsReadGoesThroughBound(t *testing.T) {
	sourcescan.InPackage(t, cmdAgentRepldDir)
	// Arrange.
	source, err := os.ReadFile("forward.go")
	if err != nil {
		t.Fatalf("read forward.go: %v", err)
	}

	// Act.
	reads := strings.Count(string(source), "f.verbs()")

	// Assert.
	if reads != 1 {
		t.Fatalf("forward.go reads f.verbs() %d times, want once, inside bound", reads)
	}
}

// TestEveryForwarderCarriesItsCollaboratorThroughLateBound pins the shared
// shape: no forwarder guards its one late-bound collaborator with a mutex of
// its own. historyForwarder binds three sources at once and mediationHook's
// lock guards its owed deploy too, so each keeps its own.
func TestEveryForwarderCarriesItsCollaboratorThroughLateBound(t *testing.T) {
	sourcescan.InPackage(t, cmdAgentRepldDir)
	// Arrange
	forwarder := regexp.MustCompile(`type (\w+(?:Forwarder|Hook)) struct \{\n\tmu +sync\.RWMutex`)
	allowed := map[string]bool{"historyForwarder": true, "mediationHook": true}
	embedded := regexp.MustCompile(`\tlateBound\[`)

	// Act
	var handRolled []string
	carried := 0
	for _, file := range sourcescan.Production(t, cmdAgentRepldDir) {
		for _, m := range forwarder.FindAllStringSubmatch(string(file.Source), -1) {
			if !allowed[m[1]] {
				handRolled = append(handRolled, file.Name+": "+m[1])
			}
		}
		carried += len(embedded.FindAllIndex(file.Source, -1))
	}

	// Assert
	if len(handRolled) != 0 {
		t.Fatalf("forwarders guarding their collaborator by hand: %v; embed lateBound", handRolled)
	}
	if carried < 16 {
		t.Fatalf("found %d forwarders embedding lateBound, want the 16 the extraction moved", carried)
	}
}

func TestTheGraphRegistersMergeTestLogsForRetention(t *testing.T) {
	sourcescan.InPackage(t, cmdAgentRepldDir)
	// Act.
	registered := sourcescan.Count(t, cmdAgentRepldDir, "p.Surfaces.RetainDirectory(merge.LogsDir(p.Layout.Dir()), merge.IsLog)")

	// Assert.
	if registered != 1 {
		t.Fatalf("the merge test logs directory is registered for retention %d times, want once", registered)
	}
}

// TestEveryLifecycleEdgeCloseGoesThroughCloseOnEdge keeps the sink's
// stand-alone recovery edges on one helper: each is named once in
// lifecycle.go, in its closeOnEdge call, so a hand-rolled close of either
// fails here.
func TestEveryLifecycleEdgeCloseGoesThroughCloseOnEdge(t *testing.T) {
	sourcescan.InPackage(t, cmdAgentRepldDir)
	src, err := os.ReadFile("lifecycle.go")
	if err != nil {
		t.Fatal(err)
	}
	for _, edge := range []string{"health.EdgeWatchOpened", "health.EdgeLinkRestored"} {
		if all, viaHelper := strings.Count(string(src), edge), strings.Count(string(src), "s.closeOnEdge(ws, "+edge); all != 1 || viaHelper != 1 {
			t.Fatalf("lifecycle.go names %s %d times, %d through closeOnEdge; want once, through it", edge, all, viaHelper)
		}
	}
}

// TestLogsSitesSpellsNoMinimumLevelArm fails a hand-kept mapping of the
// minimum level beside the server's one table (server.LogSitesMinimum).
func TestLogsSitesSpellsNoMinimumLevelArm(t *testing.T) {
	sourcescan.InPackage(t, cmdAgentRepldDir)
	// Arrange
	src, err := os.ReadFile("logscli.go")
	if err != nil {
		t.Fatalf("read logscli.go: %v", err)
	}

	// Act
	got := strings.Count(string(src), "ListLogSitesMinimumLevel_")

	// Assert
	if got != 0 || !strings.Contains(string(src), "server.LogSitesMinimum(") {
		t.Fatalf("logscli.go spells a minimum level arm %d times, want none: it reads server.LogSitesMinimum", got)
	}
}

// TestEveryReadOfTheHooksBindingGoesThroughOrchestrator keeps one read of the
// binding: an edge reading h.o by hand fails here.
func TestEveryReadOfTheHooksBindingGoesThroughOrchestrator(t *testing.T) {
	sourcescan.InPackage(t, cmdAgentRepldDir)
	source, err := os.ReadFile("mediationhook.go")
	if err != nil {
		t.Fatal(err)
	}
	if n := strings.Count(string(source), "return h.o\n"); n != 1 || strings.Count(string(source), ":= h.o\n") != 0 {
		t.Fatalf("the binding is read %d times by return and some by assignment, want orchestrator alone", n)
	}
}

func TestProfilingTakesTheSharedShape(t *testing.T) {
	sourcescan.InPackage(t, cmdAgentRepldDir)
	// Arrange
	source, err := os.ReadFile("profiling.go")
	if err != nil {
		t.Fatal(err)
	}

	// Act
	problems := profilingtest.SharedShapeProblems(string(source), "daemon")

	// Assert
	for _, p := range problems {
		t.Error("profiling.go " + p)
	}
}

// TestTheWorktreeLockSitesShareOnePath fails a site that spells the worktree
// lock's path by hand: the reaper and the merge workspace sweep exclude each
// other only while both name the same file.
func TestTheWorktreeLockSitesShareOnePath(t *testing.T) {
	sourcescan.InPackage(t, cmdAgentRepldDir)
	// Arrange
	var text strings.Builder
	for _, name := range []string{"reaper.go", "graph.go"} {
		src, err := os.ReadFile(name)
		if err != nil {
			t.Fatalf("read %s: %v", name, err)
		}
		text.Write(src)
	}

	// Act
	shared := strings.Count(text.String(), "worktreeLockPath(")
	byHand := strings.Count(text.String(), "worktreeReapLockName)")

	// Assert: the helper's definition, the reaper and the merge workspace
	// sweep.
	if shared != 3 || byHand != 1 {
		t.Fatalf("the worktree lock is named through the helper %d times and by hand %d times, want 3 and 1 (the helper's own body)", shared, byHand)
	}
}

func TestEveryNotOnGithubSentenceIsSaidBySayNotOnGithub(t *testing.T) {
	sourcescan.InPackage(t, cmdAgentRepldDir)
	// Arrange
	src, err := os.ReadFile("subscriptionrefusal.go")
	if err != nil {
		t.Fatal(err)
	}

	// Assert
	if n := strings.Count(string(src), "is not on github.com"); n != 0 {
		t.Fatalf("subscriptionrefusal.go says the not_on_github sentence by hand %d times; every describer says it through sayNotOnGithub", n)
	}
	if n := strings.Count(string(src), "sayNotOnGithub(subscriber, r.NotOnGithub.GetOrigin())"); n != 2 {
		t.Fatalf("%d describers say not_on_github through sayNotOnGithub, want the merge queue's and the workflow run's", n)
	}
}

// TestEveryRepeatedFlagIsTheSharedType pins that no command hand-rolls its own
// repeated string flag beside repeatedFlag.
func TestEveryRepeatedFlagIsTheSharedType(t *testing.T) {
	sourcescan.InPackage(t, cmdAgentRepldDir)
	// Arrange
	setter := regexp.MustCompile(`\*f\.values = append\(\*f\.values, value\)`)
	files, err := os.ReadDir(".")
	if err != nil {
		t.Fatalf("read the package: %v", err)
	}

	// Act
	var offenders []string
	for _, file := range files {
		name := file.Name()
		if name == "repeatedflag.go" || !regexp.MustCompile(`^[^.]+\.go$`).MatchString(name) || regexp.MustCompile(`_test\.go$`).MatchString(name) {
			continue
		}
		source, err := os.ReadFile(name)
		if err != nil {
			t.Fatalf("read %s: %v", name, err)
		}
		if setter.Match(source) {
			offenders = append(offenders, name)
		}
	}

	// Assert
	if len(offenders) > 0 {
		t.Fatalf("these files hand-roll a repeated flag; use repeatedFlag: %v", offenders)
	}
}

// TestOnlyFollowRootPathsBuildsTheRootWatchsSource pins that the refusal
// cannot be stepped around: this package follows paths only through
// followRootPaths, whose one kernelevent.FollowPaths call is the only follow
// in its production sources.
func TestOnlyFollowRootPathsBuildsTheRootWatchsSource(t *testing.T) {
	sourcescan.InPackage(t, cmdAgentRepldDir)
	// Arrange
	var sites []string

	// Act
	sourcescan.EachSourceIn(t, ".", false, func(fset *token.FileSet, f *ast.File) {
		for _, decl := range f.Decls {
			fn, _ := decl.(*ast.FuncDecl)
			ast.Inspect(decl, func(n ast.Node) bool {
				if sourcescan.IsSelector(n, "kernelevent", "Follow") || sourcescan.IsSelector(n, "kernelevent", "FollowPaths") {
					owner := ""
					if fn != nil {
						owner = fn.Name.Name
					}
					sites = append(sites, owner+"@"+filepath.Base(fset.Position(n.Pos()).Filename))
				}
				return true
			})
		}
	})

	// Assert
	if want := []string{"followRootPaths@rootwatch.go"}; !slices.Equal(sites, want) {
		t.Fatalf("kernelevent follows at %v, want only %v", sites, want)
	}
}

// TestEveryRosterSinkWritesThroughRosterWrite pins the extraction: each
// roster sink that keeps a change durable is built by rosterWrite and records
// nothing itself, so a hand-rolled sink that logs its own failure fails here.
func TestEveryRosterSinkWritesThroughRosterWrite(t *testing.T) {
	sourcescan.InPackage(t, cmdAgentRepldDir)
	for _, file := range []string{"roster_results.go", "roster_status_changes.go", "roster_visibilities.go"} {
		t.Run(file, func(t *testing.T) {
			// Arrange
			parsed, err := parser.ParseFile(token.NewFileSet(), file, nil, 0)
			if err != nil {
				t.Fatalf("parse %s: %v", file, err)
			}

			// Act
			calls := map[string]int{}
			ast.Inspect(parsed, func(n ast.Node) bool {
				call, ok := n.(*ast.CallExpr)
				if !ok {
					return true
				}
				switch fn := call.Fun.(type) {
				case *ast.Ident:
					calls[fn.Name]++
				case *ast.SelectorExpr:
					calls["."+fn.Sel.Name]++
				}
				return true
			})

			// Assert
			if calls["rosterWrite"] != 1 || calls[".Error"] != 0 {
				t.Fatalf("%s calls rosterWrite %d times and .Error %d times, want once and never",
					file, calls["rosterWrite"], calls[".Error"])
			}
		})
	}
}

// TestOnlyInstanceAddressResolvesADaemonsPrivateAddress keeps the lookup of
// another daemon's private address in one place: a second site that probes
// and reads it by hand would judge "runs" and "records an address" apart from
// the one every forward rests on.
func TestOnlyInstanceAddressResolvesADaemonsPrivateAddress(t *testing.T) {
	sourcescan.InPackage(t, cmdAgentRepldDir)
	// Act
	sites := sourcescan.Sites(t, cmdAgentRepldDir, "daemonaddr.InstanceAddress(")

	// Assert
	if len(sites) != 1 || sites[0] != "sharedtake.go" {
		t.Fatalf("daemonaddr.InstanceAddress is called from %v, want only instanceAddress in sharedtake.go", sites)
	}
}

// TestTheDaemonsHolderTakeIsSocketholdsTimedTake pins the one bounded, timed holder take: the daemon calls
// sockethold.TakeTimed and keeps no take bound of its own.
func TestTheDaemonsHolderTakeIsSocketholdsTimedTake(t *testing.T) {
	sourcescan.InPackage(t, cmdAgentRepldDir)
	// Arrange
	calls, ownBound := 0, ""

	// Act
	sourcescan.EachSourceIn(t, ".", false, func(fset *token.FileSet, f *ast.File) {
		ast.Inspect(f, func(n ast.Node) bool {
			if v, ok := n.(*ast.ValueSpec); ok {
				for _, id := range v.Names {
					if strings.Contains(id.Name, "TakeBound") || strings.Contains(id.Name, "takeBound") {
						ownBound = fset.Position(id.Pos()).String()
					}
				}
			}
			if sourcescan.IsSelector(n, "sockethold", "TakeTimed") {
				calls++
			}
			return true
		})
	})

	// Assert
	if ownBound != "" || calls != 1 {
		t.Fatalf("own take bound at %q, %d sockethold.TakeTimed calls; want none of its own and exactly one call", ownBound, calls)
	}
}

// THE SPAWN PREFLIGHT RUNS ONCE, AFTER THE DEPLOYER IS BUILT: the deployer's
// daemon-boot recovery edge (CloseEarlierFailures) closes every standing
// successor_spawn_failed fault, so a preflight before it would have its own
// fault closed by the same boot.
func TestTheGraphPreflightsTheSpawnOnceAfterTheDeployerIsBuilt(t *testing.T) {
	// Arrange
	var graph string
	for _, f := range sourcescan.Production(t, cmdAgentRepldDir) {
		if f.Name == "graph.go" {
			graph = string(f.Source)
		}
	}

	// Act
	preflights := strings.Count(graph, "rolloutController.PreflightSpawn(ctx)")
	preflight := strings.Index(graph, "rolloutController.PreflightSpawn(ctx)")
	deployer := strings.Index(graph, "buildDeployer(ctx, deployerParams{")

	// Assert
	if preflights != 1 || deployer < 0 || preflight < deployer {
		t.Fatalf("PreflightSpawn sites = %d at %d, buildDeployer at %d; want one preflight after the deployer is built", preflights, preflight, deployer)
	}
}

// describedResponses is every response an answerDescribed call site
// describes, for the reflection test over their errors' arms.
func describedResponses(t *testing.T) []string {
	t.Helper()
	responses, _ := scanDescribedAnswers(t)
	return responses
}

// describedErrors reads, from the CLI's tests (cmd/agent-repld/*_test.go),
// the agentrepl.v1 error every describedRefusal entry describes: the type its
// describe closure asserts the message to. It is the set
// everyDescribedRefusal holds, read as written, since a test's table cannot
// be listed from outside its package.
func describedErrors(t *testing.T) map[string]bool {
	t.Helper()
	names, err := filepath.Glob(filepath.Join(sourcescan.DaemonRoot(t), cmdAgentRepldDir, "*_test.go"))
	if err != nil {
		t.Fatalf("glob the CLI's tests: %v", err)
	}
	out := map[string]bool{}
	fset := token.NewFileSet()
	for _, name := range names {
		file, err := parser.ParseFile(fset, name, nil, 0)
		if err != nil {
			t.Fatalf("parse %s: %v", name, err)
		}
		ast.Inspect(file, func(n ast.Node) bool {
			lit, ok := n.(*ast.CompositeLit)
			if !ok {
				return true
			}
			for _, elt := range lit.Elts {
				kv, ok := elt.(*ast.KeyValueExpr)
				if !ok {
					continue
				}
				if key, ok := kv.Key.(*ast.Ident); !ok || key.Name != "describe" {
					continue
				}
				ast.Inspect(kv.Value, func(n ast.Node) bool {
					assert, ok := n.(*ast.TypeAssertExpr)
					if !ok {
						return true
					}
					if star, ok := assert.Type.(*ast.StarExpr); ok {
						if sel, ok := star.X.(*ast.SelectorExpr); ok && sourcescan.IsSelector(sel, "agentreplv1", sel.Sel.Name) {
							out["agentrepl.v1."+sel.Sel.Name] = true
						}
					}
					return true
				})
			}
			return true
		})
	}
	if len(out) == 0 {
		t.Fatalf("found no describedRefusal entry in the CLI's tests")
	}
	return out
}

// TestEveryDescribedRpcIsADescribedRefusal holds every rpc whose answer the
// CLI describes (each answerDescribed call site's response) to an entry of
// everyDescribedRefusal, whose every arm
// agent-repld.TestEveryDescribedRefusalSaysEveryArm holds to a sentence.
func TestEveryDescribedRpcIsADescribedRefusal(t *testing.T) {
	// Arrange
	described := describedErrors(t)
	responses := describedResponses(t)
	if len(responses) == 0 {
		t.Fatal("found no described response to check")
	}
	for _, response := range responses {
		t.Run(response, func(t *testing.T) {
			mt, err := protoregistry.GlobalTypes.FindMessageByName(protoreflect.FullName("agentrepl.v1." + response))
			if err != nil {
				t.Fatalf("no message agentrepl.v1.%s: %v", response, err)
			}

			// Act
			field := mt.Descriptor().Fields().ByName("error")

			// Assert
			if field == nil || field.Message() == nil {
				t.Fatalf("%s has no error arm to describe", response)
			}
			if !described[string(field.Message().FullName())] {
				t.Fatalf("%s's describer is not in everyDescribedRefusal", field.Message().FullName())
			}
		})
	}
}
