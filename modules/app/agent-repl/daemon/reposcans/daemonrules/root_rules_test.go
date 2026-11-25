package daemonrules

import (
	"bufio"
	"fmt"
	"go/ast"
	"go/parser"
	"go/token"
	"io"
	"io/fs"
	"os"
	"path/filepath"
	"slices"
	"sort"
	"strconv"
	"strings"
	"testing"

	"agent-repld/reposcans/internal/sourcescan"
)

// daemonRootDir is the package these rules hold, under the daemon module's root.
const daemonRootDir = "."

// No daemon source hand-rolls the broadcast: a close of a channel followed by
// a fresh make into the same name is a Signal.
func TestNoDaemonSourceHandRollsAChangeBroadcast(t *testing.T) {
	sourcescan.InPackage(t, daemonRootDir)
	// Arrange
	root := sourcescan.DaemonRoot(t)

	// Act
	var handRolled []string
	sourcescan.EachDaemonSource(t, root, []string{"internal/changesignal"}, func(fset *token.FileSet, file *ast.File) {
		ast.Inspect(file, func(n ast.Node) bool {
			block, ok := n.(*ast.BlockStmt)
			if !ok {
				return true
			}
			for i := 0; i+1 < len(block.List); i++ {
				if name, ok := closedName(block.List[i]); ok && remadeName(block.List[i+1]) == name {
					handRolled = append(handRolled, fset.Position(block.List[i].Pos()).String())
				}
			}
			return true
		})
	})

	// Assert
	if len(handRolled) != 0 {
		t.Fatalf("hand-rolled change broadcasts at %v; use changesignal.Signal", handRolled)
	}
}

// closedName answers X for a `close(X)` statement.
func closedName(stmt ast.Stmt) (string, bool) {
	expr, ok := stmt.(*ast.ExprStmt)
	if !ok {
		return "", false
	}
	call, ok := expr.X.(*ast.CallExpr)
	if !ok || len(call.Args) != 1 {
		return "", false
	}
	if fn, ok := call.Fun.(*ast.Ident); !ok || fn.Name != "close" {
		return "", false
	}
	return exprName(call.Args[0]), true
}

// remadeName answers X for an `X = make(chan ...)` statement.
func remadeName(stmt ast.Stmt) string {
	assign, ok := stmt.(*ast.AssignStmt)
	if !ok || len(assign.Lhs) != 1 || len(assign.Rhs) != 1 {
		return ""
	}
	call, ok := assign.Rhs[0].(*ast.CallExpr)
	if !ok || len(call.Args) == 0 {
		return ""
	}
	if fn, ok := call.Fun.(*ast.Ident); !ok || fn.Name != "make" {
		return ""
	}
	if _, ok := call.Args[0].(*ast.ChanType); !ok {
		return ""
	}
	return exprName(assign.Lhs[0])
}

func exprName(e ast.Expr) string {
	switch v := e.(type) {
	case *ast.Ident:
		return v.Name
	case *ast.SelectorExpr:
		return exprName(v.X) + "." + v.Sel.Name
	}
	return ""
}

// TestNoDaemonSourceReadsAFenceByHand holds every fence delimiter the daemon
// reads to the length rule mdfence.Parse, Opens and Closes keep: a strings
// call handed a literal opening on ``` or ~~~ (strings.HasPrefix(line,
// "```"), say) reads a fence by hand, and toggles where the rule would read
// content.
func TestNoDaemonSourceReadsAFenceByHand(t *testing.T) {
	sourcescan.InPackage(t, daemonRootDir)
	// Arrange.
	var sites []string

	// Act.
	sourcescan.EachDaemonSource(t, sourcescan.DaemonRoot(t), []string{"internal/mdfence"}, func(fset *token.FileSet, file *ast.File) {
		ast.Inspect(file, func(n ast.Node) bool {
			call, ok := n.(*ast.CallExpr)
			if !ok {
				return true
			}
			sel, ok := call.Fun.(*ast.SelectorExpr)
			if !ok {
				return true
			}
			if pkg, ok := sel.X.(*ast.Ident); !ok || pkg.Name != "strings" {
				return true
			}
			for _, arg := range call.Args {
				lit, ok := arg.(*ast.BasicLit)
				if !ok || lit.Kind != token.STRING {
					continue
				}
				if s, err := strconv.Unquote(lit.Value); err == nil && (strings.HasPrefix(s, "```") || strings.HasPrefix(s, "~~~")) {
					sites = append(sites, fset.Position(call.Pos()).String())
				}
			}
			return true
		})
	})

	// Assert.
	if len(sites) != 0 {
		t.Errorf("these calls read a fence by hand; read it through mdfence.Parse: %v", sites)
	}
}

// fieldsHome is the one package that may build a record's fields from other
// fields: dlog.Merge and dlog.WithCause.
const fieldsHome = "internal/dlog"

// fieldsBuilderExempt are the field builders outside dlog that are NOT copies
// of its two, each with the reason it is its own convention.
var fieldsBuilderExempt = map[string]string{
	// The store's records name their error under "error", not "cause": a
	// convention of its own every wsm record and reader keeps.
	"internal/wsm/open.go: withError": "the store's records key their error as error",
}

// TestNoPackageHandRollsARecordFieldsBuilder guards dlog.Merge and
// dlog.WithCause as the ONE pair of record-field builders. A function outside
// dlog shaped (dlog.Context, dlog.Context) dlog.Context or
// (dlog.Context, error) dlog.Context is a hand-rolled copy of one of them;
// such a package calls dlog's instead.
func TestNoPackageHandRollsARecordFieldsBuilder(t *testing.T) {
	sourcescan.InPackage(t, daemonRootDir)
	// Arrange.
	files := token.NewFileSet()
	var copies []string

	// Act.
	for _, root := range []string{"cmd", "internal"} {
		err := filepath.WalkDir(root, func(path string, entry fs.DirEntry, walkErr error) error {
			if walkErr != nil {
				return walkErr
			}
			if entry.IsDir() || !strings.HasSuffix(path, ".go") || filepath.Dir(path) == fieldsHome {
				return nil
			}
			parsed, err := parser.ParseFile(files, path, nil, 0)
			if err != nil {
				return err
			}
			for _, name := range fieldsBuilders(parsed) {
				site := path + ": " + name
				if _, exempt := fieldsBuilderExempt[site]; !exempt {
					copies = append(copies, site)
				}
			}
			return nil
		})
		if err != nil {
			t.Fatalf("walk %s: %v", root, err)
		}
	}

	// Assert.
	if len(copies) != 0 {
		t.Fatalf("functions hand-roll a record-fields builder; call dlog.Merge or dlog.WithCause instead:\n%s", strings.Join(copies, "\n"))
	}
}

func TestTheFieldsGuardRecognizesAHandRolledCopy(t *testing.T) {
	sourcescan.InPackage(t, daemonRootDir)
	tests := []struct {
		name string
		src  string
		want []string
	}{
		{name: "a cause stamper is a copy", src: `package p
func withCause(fields dlog.Context, err error) dlog.Context { return nil }`, want: []string{"withCause"}},
		{name: "a context merger is a copy", src: `package p
func merge(base, extra dlog.Context) dlog.Context { return nil }`, want: []string{"merge"}},
		{name: "a one-field builder is not", src: `package p
func withField(fields dlog.Context, name string, value any) dlog.Context { return nil }`},
		{name: "a method is not", src: `package p
func (r *run) fields(base dlog.Context, err error) dlog.Context { return nil }`},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange.
			parsed, err := parser.ParseFile(token.NewFileSet(), "p.go", tt.src, 0)
			if err != nil {
				t.Fatalf("parse: %v", err)
			}

			// Act.
			got := fieldsBuilders(parsed)

			// Assert.
			if strings.Join(got, ",") != strings.Join(tt.want, ",") {
				t.Fatalf("fieldsBuilders = %v, want %v", got, tt.want)
			}
		})
	}
}

// fieldsBuilders names every top-level function FILE declares shaped
// (dlog.Context, dlog.Context) dlog.Context or (dlog.Context, error)
// dlog.Context.
func fieldsBuilders(file *ast.File) []string {
	var names []string
	for _, decl := range file.Decls {
		fn, ok := decl.(*ast.FuncDecl)
		if !ok || fn.Recv != nil {
			continue
		}
		params := flatTypes(fn.Type.Params)
		results := flatTypes(fn.Type.Results)
		if len(params) != 2 || len(results) != 1 || results[0] != "dlog.Context" || params[0] != "dlog.Context" {
			continue
		}
		if params[1] == "dlog.Context" || params[1] == "error" {
			names = append(names, fn.Name.Name)
		}
	}
	return names
}

// flatTypes spells each parameter's type once per name it declares.
func flatTypes(list *ast.FieldList) []string {
	if list == nil {
		return nil
	}
	var out []string
	for _, field := range list.List {
		spelled := typeName(field.Type)
		count := len(field.Names)
		if count == 0 {
			count = 1
		}
		for i := 0; i < count; i++ {
			out = append(out, spelled)
		}
	}
	return out
}

// typeName spells a simple type expression.
func typeName(expr ast.Expr) string {
	switch t := expr.(type) {
	case *ast.Ident:
		return t.Name
	case *ast.SelectorExpr:
		if pkg, ok := t.X.(*ast.Ident); ok {
			return pkg.Name + "." + t.Sel.Name
		}
	}
	return ""
}

// A FORCED KILL ENDS DETACHED WORK, SO ONLY THE USER MAY ASK FOR ONE. An
// interrupt ends only the synchronous turn; background agents, shells,
// monitors and workflows end by their own per-task stop, or by a forced kill
// the user explicitly asked for. Every KillTurn whose force is anything but
// the literal `false`, and every KillTurnRequest whose Force is anything but
// the literal `false`, is a forced-kill site, and each one is listed here with
// the user request that authorizes it. A new site fails this test until it is
// listed with its reason, which is where the question "did the user ask for
// this?" gets asked.
func TestForcedKillsHappenOnlyWhereTheUserAskedForThem(t *testing.T) {
	sourcescan.InPackage(t, daemonRootDir)
	type allowance struct {
		path   string
		fn     string
		callee string
		count  int
		reason string
	}
	allowed := []allowance{
		{path: "internal/workspace/interrupt.go", fn: "(*verbs).interruptTurn", callee: "KillTurn", count: 1, reason: "force is the user's answer to the confirm challenge; an unconfirmed interrupt passes false"},
		{path: "internal/workspace/restart.go", fn: "(*verbs).forceEndTurn", callee: "KillTurn", count: 1, reason: "the user asked for the workspace to be restarted"},
		{path: "internal/workspace/sender.go", fn: "killTurn", callee: "KillTurnRequest.Force", count: 1, reason: "the one KillTurn request builder, shared by the queue's sender and the verbs' shim adapter, forwards its caller's force unchanged"},
	}

	// Arrange.
	want := make(map[string]allowance, len(allowed))
	for _, item := range allowed {
		if item.reason == "" {
			t.Fatalf("allowance %s %s has no reason", item.path, item.fn)
		}
		want[forcedKillKey(item.path, item.fn, item.callee)] = item
	}

	// Act.
	sites, err := forcedKillSites(".", "cmd", "internal")
	if err != nil {
		t.Fatalf("scan production Go: %v", err)
	}

	// Assert.
	seen := make(map[string]int, len(allowed))
	var unexpected []string
	for _, site := range sites {
		key := forcedKillKey(site.path, site.fn, site.callee)
		if _, ok := want[key]; ok {
			seen[key]++
			continue
		}
		unexpected = append(unexpected, site.String())
	}
	sort.Strings(unexpected)
	if len(unexpected) > 0 {
		t.Errorf("forced kills the user never asked for; an interrupt ends only the turn, so pass force=false, or list the site with the user request that authorizes it:\n  %s", strings.Join(unexpected, "\n  "))
	}
	for key, item := range want {
		if seen[key] != item.count {
			t.Errorf("allowance %s saw %d forced kill(s), want %d; update the explicit site and reason when a forced kill moves", key, seen[key], item.count)
		}
	}
}

// TestTheForcedKillScanFindsEveryForcedShape pins the scanner itself, so the
// guard above cannot pass by no longer seeing the thing it guards.
func TestTheForcedKillScanFindsEveryForcedShape(t *testing.T) {
	sourcescan.InPackage(t, daemonRootDir)
	tests := []struct {
		name string
		file string
		body string
		want []string
	}{
		{
			name: "a literal true is forced",
			file: "internal/x/x.go",
			body: "func f(s S) { s.KillTurn(nil, \"t\", true) }",
			want: []string{"internal/x/x.go|f|KillTurn"},
		},
		{
			name: "a variable is forced until proven otherwise",
			file: "internal/x/x.go",
			body: "func f(s S, force bool) { s.KillTurn(nil, \"t\", force) }",
			want: []string{"internal/x/x.go|f|KillTurn"},
		},
		{
			name: "a literal true beside a stated command is forced",
			file: "internal/x/x.go",
			body: "func f(s S) { s.KillTurn(nil, \"t\", true, nil) }",
			want: []string{"internal/x/x.go|f|KillTurn"},
		},
		{
			name: "a literal false is an interrupt",
			file: "internal/x/x.go",
			body: "func f(s S) { s.KillTurn(nil, \"t\", false) }",
		},
		{
			name: "a request carrying Force true is forced",
			file: "internal/x/x.go",
			body: "type T struct{}\nfunc (t *T) g(c C) { c.KillTurn(nil, &shimv1.KillTurnRequest{Force: true}) }",
			want: []string{"internal/x/x.go|(*T).g|KillTurnRequest.Force"},
		},
		{
			name: "a request carrying Force false is an interrupt",
			file: "internal/x/x.go",
			body: "func f(c C) { c.KillTurn(nil, &shimv1.KillTurnRequest{Force: false}) }",
		},
		{
			name: "a request with no Force is an interrupt",
			file: "internal/x/x.go",
			body: "func f(c C) { c.KillTurn(nil, &shimv1.KillTurnRequest{}) }",
		},
		{
			name: "a test file is not production",
			file: "internal/x/x_test.go",
			body: "func f(s S) { s.KillTurn(nil, \"t\", true) }",
		},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange.
			root := t.TempDir()
			path := filepath.Join(root, filepath.FromSlash(tt.file))
			for _, dir := range []string{filepath.Join(root, "cmd"), filepath.Dir(path)} {
				if err := os.MkdirAll(dir, 0o755); err != nil {
					t.Fatal(err)
				}
			}
			if err := os.WriteFile(path, []byte("package x\n\n"+tt.body+"\n"), 0o644); err != nil {
				t.Fatal(err)
			}

			// Act.
			sites, err := forcedKillSites(root, "cmd", "internal")

			// Assert.
			if err != nil {
				t.Fatalf("scan: %v", err)
			}
			var got []string
			for _, site := range sites {
				got = append(got, forcedKillKey(site.path, site.fn, site.callee))
			}
			if !slices.Equal(got, tt.want) {
				t.Fatalf("forced kill sites = %v, want %v", got, tt.want)
			}
		})
	}
}

// forcedKill is one forced-kill site: the file, the enclosing function, what
// forces (a KillTurn call or a KillTurnRequest's Force field) and its line.
type forcedKill struct {
	path   string
	fn     string
	callee string
	line   int
}

func (k forcedKill) String() string {
	return fmt.Sprintf("%s:%d %s forces %s", k.path, k.line, k.fn, k.callee)
}

func forcedKillKey(path, fn, callee string) string { return path + "|" + fn + "|" + callee }

// forcedKillSites walks the production Go under each root of base and answers
// every forced-kill site, in walk order.
func forcedKillSites(base string, roots ...string) ([]forcedKill, error) {
	files := token.NewFileSet()
	var sites []forcedKill
	for _, root := range roots {
		err := filepath.WalkDir(filepath.Join(base, root), func(path string, entry fs.DirEntry, walkErr error) error {
			if walkErr != nil {
				return walkErr
			}
			if entry.IsDir() || filepath.Ext(path) != ".go" || strings.HasSuffix(path, "_test.go") {
				return nil
			}
			parsed, err := parser.ParseFile(files, path, nil, 0)
			if err != nil {
				return err
			}
			rel, err := filepath.Rel(base, path)
			if err != nil {
				return err
			}
			rel = filepath.ToSlash(rel)
			for _, declaration := range parsed.Decls {
				function, ok := declaration.(*ast.FuncDecl)
				if !ok || function.Body == nil {
					continue
				}
				name := funcName(function)
				ast.Inspect(function.Body, func(node ast.Node) bool {
					if callee, ok := forcedKillIn(node); ok {
						sites = append(sites, forcedKill{path: rel, fn: name, callee: callee, line: files.Position(node.Pos()).Line})
					}
					return true
				})
			}
			return nil
		})
		if err != nil {
			return nil, err
		}
	}
	return sites, nil
}

// forcedKillIn answers what forces a kill at node, if anything does.
func forcedKillIn(node ast.Node) (string, bool) {
	switch n := node.(type) {
	case *ast.CallExpr:
		selector, ok := n.Fun.(*ast.SelectorExpr)
		if !ok || selector.Sel.Name != "KillTurn" || len(n.Args) < 3 {
			return "", false
		}
		return "KillTurn", !isFalse(n.Args[2])
	case *ast.CompositeLit:
		if !namesKillTurnRequest(n.Type) {
			return "", false
		}
		for _, element := range n.Elts {
			field, ok := element.(*ast.KeyValueExpr)
			if !ok {
				continue
			}
			if key, ok := field.Key.(*ast.Ident); ok && key.Name == "Force" && !isFalse(field.Value) {
				return "KillTurnRequest.Force", true
			}
		}
	}
	return "", false
}

func namesKillTurnRequest(expr ast.Expr) bool {
	switch t := expr.(type) {
	case *ast.SelectorExpr:
		return t.Sel.Name == "KillTurnRequest"
	case *ast.Ident:
		return t.Name == "KillTurnRequest"
	}
	return false
}

func isFalse(expr ast.Expr) bool {
	ident, ok := expr.(*ast.Ident)
	return ok && ident.Name == "false"
}

// funcName names a function the way the allowances do: `f` for a function,
// `(*T).m` or `(T).m` for a method.
func funcName(function *ast.FuncDecl) string {
	if function.Recv == nil || len(function.Recv.List) == 0 {
		return function.Name.Name
	}
	receiver := function.Recv.List[0].Type
	pointer := ""
	if star, ok := receiver.(*ast.StarExpr); ok {
		pointer, receiver = "*", star.X
	}
	if index, ok := receiver.(*ast.IndexExpr); ok {
		receiver = index.X
	}
	name := "?"
	if ident, ok := receiver.(*ast.Ident); ok {
		name = ident.Name
	}
	return "(" + pointer + name + ")." + function.Name.Name
}

// TestProductionDiagnosticsDoNotBypassDlog guards the daemon's one logging
// function. The allowlist names every bootstrap or emergency write that must
// work before dlog exists or after its sink has failed. treefmt's writes are
// command output to injected streams, not daemon diagnostics.
func TestProductionDiagnosticsDoNotBypassDlog(t *testing.T) {
	sourcescan.InPackage(t, daemonRootDir)
	type allowance struct {
		path   string
		fn     string
		callee string
		count  int
		reason string
	}
	allowed := []allowance{
		{path: "cmd/agent-repld/main.go", fn: "answerMigrationKind", callee: "fmt.Fprintln", count: 2, reason: "-migration-kind-from opens no log surfaces: its whole answer is `additive` or `breaking` on stdout for the deploy that asked, or the reason on stderr, which the asking deploy carries into its own ERROR record (deploy.askBinary)"},
		{path: "cmd/agent-repld/main.go", fn: "main", callee: "fmt.Fprintln", count: 8, reason: "process bootstrap and final exit reporting exist before or after the durable surfaces; the move to the stable directory adds one, refusing a relative path flag before any surface opens; the account roots' check adds one, refusing a launch without them before the boot claim and before any surface opens; -probe-boot-claim adds two, and it opens no log surfaces at all because its whole answer is its exit status; -layout-version adds one, its whole answer being the layout number on stdout for the deploy that asked"},
		{path: "internal/gitclient/fetchlock.go", fn: "helperFailed", callee: "fmt.Fprintf", count: 1, reason: "the fetch lock's helper is the daemon's child for one fetch and opens no log surface: its failure is its stderr, which the daemon carries into its own ERROR record of the git that failed (client.run)"},
		{path: "internal/dlog/logger.go", fn: "emergency", callee: "os.Stderr.Write", count: 1, reason: "a durable sink cannot record its own write failure; the echo is itself a marshalled record, written raw because dlog is what just failed"},
		{path: "cmd/agent-repld/appfreeze.go", fn: "runThawGuard", callee: "fmt.Fprintln", count: 1, reason: "the thaw-guard verb's bootstrap: without -state-dir there is no logs directory to open a surface in, so the refusal can only go to its stderr"},
		{path: "cmd/agent-repld/appfreeze.go", fn: "runThawGuard", callee: "fmt.Fprintf", count: 1, reason: "the thaw-guard verb's bootstrap: its log surface failed to open, so the reason can only go to its stderr"},
		{path: "cmd/agent-repld/deployverb.go", fn: "runDeployVerb", callee: "fmt.Fprintf", count: 7, reason: "the deploy verb is a CLI whose product is its output: every refusal is the verb's injected stderr, and the daemon logs every decision itself"},
		{path: "cmd/agent-repld/deployverb.go", fn: "runDeployVerb", callee: "fmt.Fprintln", count: 1, reason: "the deploy verb prints each component's decision on its injected stdout"},
		{path: "cmd/agent-repld/audiooutputverb.go", fn: "runAudioOutputVerb", callee: "fmt.Fprintln", count: 1, reason: "the audio-output verb is the daemon's child for one read and opens no log surface: a device that cannot be read is its stderr, which the daemon's Runner carries into the speech's own ERROR record of the failed read"},
		{path: "cmd/agent-repld/audiooutputverb.go", fn: "runAudioOutputVerb", callee: "fmt.Fprintf", count: 1, reason: "the audio-output verb is the daemon's child for one read and opens no log surface: a device that cannot be written is its stderr, which the daemon's Runner carries into the speech's own ERROR record of the failed read"},
		{path: "cmd/agent-repld/callverb.go", fn: "runCallVerb", callee: "fmt.Fprintf", count: 8, reason: "the call verb is a CLI whose product is its output: every refusal is the verb's injected stderr, and the daemon logs every rpc it answers itself"},
		{path: "cmd/agent-repld/callverb.go", fn: "runCallVerb", callee: "fmt.Fprintln", count: 3, reason: "the call verb prints its usage on its injected stderr and the daemon's answer on its injected stdout"},
		{path: "cmd/agent-repld/agentcli.go", fn: "runNoun", callee: "fmt.Fprintf", count: 2, reason: "the agent CLI is a command whose product is its output: its usage, its refusals and the daemon's answer are its injected stdout and stderr, and the daemon logs every rpc it answers itself"},
		{path: "cmd/agent-repld/agentcli.go", fn: "flags", callee: "fmt.Fprintf", count: 1, reason: "the agent CLI is a command whose product is its output: its usage, its refusals and the daemon's answer are its injected stdout and stderr, and the daemon logs every rpc it answers itself"},
		{path: "cmd/agent-repld/agentcli.go", fn: "failed", callee: "fmt.Fprintf", count: 1, reason: "the agent CLI is a command whose product is its output: its usage, its refusals and the daemon's answer are its injected stdout and stderr, and the daemon logs every rpc it answers itself"},
		{path: "cmd/agent-repld/agentcli.go", fn: "noted", callee: "fmt.Fprintf", count: 1, reason: "the agent CLI is a command whose product is its output: its usage, its refusals and the daemon's answer are its injected stdout and stderr, and the daemon logs every rpc it answers itself"},
		{path: "cmd/agent-repld/dryrun.go", fn: "print", callee: "fmt.Fprintf", count: 1, reason: "a dry run's whole product is the request it did not send, printed on the agent CLI's injected stdout; nothing reached the daemon to log"},
		{path: "cmd/agent-repld/agentcli.go", fn: "usageFailed", callee: "fmt.Fprintf", count: 1, reason: "the agent CLI is a command whose product is its output: its usage, its refusals and the daemon's answer are its injected stdout and stderr, and the daemon logs every rpc it answers itself"},
		{path: "cmd/agent-repld/agentcli.go", fn: "print", callee: "fmt.Fprintln", count: 1, reason: "the agent CLI is a command whose product is its output: its usage, its refusals and the daemon's answer are its injected stdout and stderr, and the daemon logs every rpc it answers itself"},
		{path: "cmd/agent-repld/testredundancycli.go", fn: "streamRedundancy", callee: "fmt.Fprintln", count: 2, reason: "the agent CLI is a command whose product is its output: each line of a measurement, and a refusal frame, is a line of its injected stdout, and the daemon logs the measurement it answers itself"},
		{path: "cmd/agent-repld/testcli.go", fn: "streamTestRun", callee: "fmt.Fprintln", count: 1, reason: "the agent CLI is a command whose product is its output: each event of a test run is a line of its injected stdout, and the daemon logs the run it answers itself"},
		{path: "cmd/agent-repld/agentcli.go", fn: "runtimeHealth", callee: "fmt.Fprintln", count: 1, reason: "the agent CLI is a command whose product is its output: its usage, its refusals and the daemon's answer are its injected stdout and stderr, and the daemon logs every rpc it answers itself"},
		{path: "cmd/agent-repld/roster.go", fn: "say", callee: "fmt.Fprintf", count: 1, reason: "the agent CLI is a command whose product is its output: its usage, its refusals and the daemon's answer are its injected stdout and stderr, and the daemon logs every rpc it answers itself"},
		{path: "internal/recordlock/locktest/locktest.go", fn: "Hold", callee: "fmt.Printf", count: 1, reason: "a re-exec'd lock helper is a test process that opens no log surfaces: its ready line on stdout is its protocol with the test that started it, and its failure on stderr is carried into that test's output"},
		{path: "internal/recordlock/locktest/locktest.go", fn: "fail", callee: "fmt.Fprintln", count: 1, reason: "a re-exec'd lock helper is a test process that opens no log surfaces: its ready line on stdout is its protocol with the test that started it, and its failure on stderr is carried into that test's output"},
		{path: "internal/dlog/surfaces.go", fn: "write", callee: "fmt.Fprintf", count: 1, reason: "a request racing closed surfaces must be answered while the first dropped record is surfaced"},
	}

	// Arrange.
	want := make(map[string]allowance, len(allowed))
	for _, item := range allowed {
		if item.reason == "" {
			t.Fatalf("allowance %s %s has no reason", item.path, item.callee)
		}
		want[bypassKey(item.path, item.fn, item.callee)] = item
	}
	seen := make(map[string]int, len(allowed))
	var unexpected []string

	// Act.
	files := token.NewFileSet()
	for _, root := range []string{"cmd", "internal"} {
		err := filepath.WalkDir(root, func(path string, entry fs.DirEntry, walkErr error) error {
			if walkErr != nil {
				return walkErr
			}
			if entry.IsDir() || filepath.Ext(path) != ".go" || strings.HasSuffix(path, "_test.go") {
				return nil
			}
			parsed, err := parser.ParseFile(files, path, nil, 0)
			if err != nil {
				return err
			}
			imports := importedPackages(parsed)
			for _, declaration := range parsed.Decls {
				function, ok := declaration.(*ast.FuncDecl)
				if !ok || function.Body == nil {
					continue
				}
				ast.Inspect(function.Body, func(node ast.Node) bool {
					call, ok := node.(*ast.CallExpr)
					if !ok {
						return true
					}
					callee, ok := bypassCallee(call, imports)
					if !ok {
						return true
					}
					rel := filepath.ToSlash(path)
					key := bypassKey(rel, function.Name.Name, callee)
					if _, ok := want[key]; ok {
						seen[key]++
					} else {
						position := files.Position(call.Pos())
						unexpected = append(unexpected, fmt.Sprintf("%s:%d %s.%s calls %s", rel, position.Line, rel, function.Name.Name, callee))
					}
					return true
				})
			}
			return nil
		})
		if err != nil {
			t.Fatalf("scan production Go under %s: %v", root, err)
		}
	}

	// Assert.
	sort.Strings(unexpected)
	if len(unexpected) > 0 {
		t.Errorf("production diagnostics bypass internal/dlog:\n  %s", strings.Join(unexpected, "\n  "))
	}
	for key, item := range want {
		if seen[key] != item.count {
			t.Errorf("allowance %s saw %d call(s), want %d; update the explicit site and reason when bootstrap behavior changes", key, seen[key], item.count)
		}
	}
}

func bypassKey(path, function, callee string) string {
	return path + "|" + function + "|" + callee
}

func importedPackages(file *ast.File) map[string]string {
	packages := make(map[string]string, len(file.Imports))
	for _, spec := range file.Imports {
		path := strings.Trim(spec.Path.Value, `"`)
		name := filepath.Base(path)
		if spec.Name != nil {
			name = spec.Name.Name
		}
		packages[name] = path
	}
	return packages
}

func bypassCallee(call *ast.CallExpr, imports map[string]string) (string, bool) {
	selector, ok := call.Fun.(*ast.SelectorExpr)
	if !ok {
		return "", false
	}
	if pkg, ok := selector.X.(*ast.Ident); ok {
		path := imports[pkg.Name]
		if path == "fmt" && isFmtPrint(selector.Sel.Name) || path == "log" && strings.HasPrefix(selector.Sel.Name, "Print") {
			return path + "." + selector.Sel.Name, true
		}
	}
	receiver, ok := selector.X.(*ast.SelectorExpr)
	if !ok || selector.Sel.Name != "Write" && selector.Sel.Name != "WriteString" {
		return "", false
	}
	pkg, ok := receiver.X.(*ast.Ident)
	if ok && imports[pkg.Name] == "os" && receiver.Sel.Name == "Stderr" {
		return "os.Stderr." + selector.Sel.Name, true
	}
	return "", false
}

func isFmtPrint(name string) bool {
	switch name {
	case "Print", "Printf", "Println", "Fprint", "Fprintf", "Fprintln":
		return true
	default:
		return false
	}
}

// retiredKeepaliveIdentifiers are the names of the daemon's retired keep-alive
// machinery: the StartTurn re-drive behind a keep-alive, its interrupt cancel,
// the keep-alive hold badge and the cold keep-alive alarm. The shim makes a
// prompt that arrives during its own keep-alive wait inside the shim, so the
// daemon never sees a keep-alive collision and never holds a prompt behind one.
// None of these names may come back in production code. The shim's
// `keepalive_failed` SESSION FAULT is a different arm and is not listed.
var retiredKeepaliveIdentifiers = []string{
	"isKeepaliveCollision",
	"KeepaliveTurnAlreadyOpen",
	"TransientKeepalive",
	"CancelKeepaliveRedrive",
	"redriveBehindKeepalive",
	"keepaliveRedrive",
	"HeldPrompt_KeepAlive",
	"HeldPromptKeepAliveHold",
	"coldKeepalive",
	"GetKeepalive(",
}

// TestNoProductionCodeNamesTheRetiredKeepaliveMachinery walks every non-test
// Go file under the daemon and fails naming the file and line of any retired
// keep-alive identifier.
func TestNoProductionCodeNamesTheRetiredKeepaliveMachinery(t *testing.T) {
	sourcescan.InPackage(t, daemonRootDir)
	// Arrange.
	var hits []string

	// Act.
	err := filepath.WalkDir(".", func(path string, entry fs.DirEntry, walkErr error) error {
		if walkErr != nil {
			return walkErr
		}
		if entry.IsDir() || filepath.Ext(path) != ".go" || strings.HasSuffix(path, "_test.go") {
			return nil
		}
		file, err := os.Open(path)
		if err != nil {
			return err
		}
		defer file.Close()
		found, err := retiredKeepaliveHits(filepath.ToSlash(path), file)
		if err != nil {
			return err
		}
		hits = append(hits, found...)
		return nil
	})

	// Assert.
	if err != nil {
		t.Fatalf("scan production Go: %v", err)
	}
	if len(hits) > 0 {
		t.Errorf("production code names retired keep-alive machinery; the shim makes a prompt wait inside its own keep-alive, so the daemon has none:\n  %s", strings.Join(hits, "\n  "))
	}
}

// retiredKeepaliveHits answers one "path:line: identifier" entry for each
// retired identifier on each line of source.
func retiredKeepaliveHits(path string, source io.Reader) ([]string, error) {
	var hits []string
	scanner := bufio.NewScanner(source)
	scanner.Buffer(make([]byte, 0, 64*1024), 1024*1024)
	for line := 1; scanner.Scan(); line++ {
		text := scanner.Text()
		for _, name := range retiredKeepaliveIdentifiers {
			if strings.Contains(text, name) {
				hits = append(hits, fmt.Sprintf("%s:%d: %s", path, line, name))
			}
		}
	}
	if err := scanner.Err(); err != nil {
		return nil, fmt.Errorf("read %s: %w", path, err)
	}
	return hits, nil
}

// TestNoOtherDaemonSourceBuildsAPriorityByHand holds every site to
// prioritywire, the one mapping between wsm.Priority, the wire's
// agentrepl.v1.WorkspacePriority and the -priority spelling: a composite
// literal of a priority arm anywhere else in the daemon is a second mapping
// that can drift from it.
func TestNoOtherDaemonSourceBuildsAPriorityByHand(t *testing.T) {
	sourcescan.InPackage(t, daemonRootDir)
	// Arrange
	var offenders []string

	// Act
	sourcescan.EachDaemonSource(t, sourcescan.DaemonRoot(t), []string{"internal/prioritywire"}, func(fset *token.FileSet, file *ast.File) {
		ast.Inspect(file, func(n ast.Node) bool {
			if lit, ok := n.(*ast.CompositeLit); ok {
				if sel, ok := lit.Type.(*ast.SelectorExpr); ok && strings.HasPrefix(sel.Sel.Name, "WorkspacePriority_P") {
					offenders = append(offenders, fset.Position(lit.Pos()).String())
				}
			}
			return true
		})
	})

	// Assert
	if len(offenders) > 0 {
		t.Fatalf("these daemon sources build a priority by hand; use prioritywire: %v", offenders)
	}
}

// TestNoTestHelperHandRollsARecordLookup guards dlog.FindRecord and
// dlog.FilterRecords as the ONE record lookup the daemon's tests share. A
// helper over dlog records that loops comparing their Level, Operation or
// Message against its own parameters is a hand-rolled copy of them: it
// delegates instead, so the matching rule lives in one place. Helpers over
// other record types (the integration harness's parsed log files, a test's
// own recording logger) are not dlog's to serve and are not judged.
func TestNoTestHelperHandRollsARecordLookup(t *testing.T) {
	sourcescan.InPackage(t, daemonRootDir)
	// Arrange.
	files := token.NewFileSet()
	var copies []string

	// Act.
	for _, root := range []string{"cmd", "internal", "integration"} {
		err := filepath.WalkDir(root, func(path string, entry fs.DirEntry, walkErr error) error {
			if walkErr != nil {
				return walkErr
			}
			if entry.IsDir() || !strings.HasSuffix(path, "_test.go") {
				return nil
			}
			parsed, err := parser.ParseFile(files, path, nil, 0)
			if err != nil {
				return err
			}
			for _, decl := range parsed.Decls {
				fn, ok := decl.(*ast.FuncDecl)
				if ok && fn.Body != nil && handRollsRecordLookup(fn) {
					copies = append(copies, path+": "+fn.Name.Name)
				}
			}
			return nil
		})
		if err != nil {
			t.Fatalf("walk %s: %v", root, err)
		}
	}

	// Assert.
	if len(copies) != 0 {
		t.Fatalf("test helpers hand-roll a record lookup; delegate to dlog.FindRecord instead:\n%s", strings.Join(copies, "\n"))
	}
}

func TestTheRecordLookupGuardRecognizesAHandRolledCopy(t *testing.T) {
	sourcescan.InPackage(t, daemonRootDir)
	tests := []struct {
		name string
		src  string
		want bool
	}{
		{
			name: "a loop matching the helper's own parameters is a copy",
			src: `package p
func find(records []dlog.Record, level, op string) bool {
	for _, r := range records {
		if r.Level == level && r.Operation == op {
			return true
		}
	}
	return false
}`,
			want: true,
		},
		{
			name: "a delegation to dlog.FindRecord is not",
			src: `package p
func find(records []dlog.Record, level, op string) bool {
	_, ok := dlog.FindRecord(records, dlog.RecordQuery{Level: level, Operation: op})
	return ok
}`,
			want: false,
		},
		{
			name: "a test's inline loop against literals is not a helper",
			src: `package p
func TestX(t *testing.T) {
	sourcescan.InPackage(t, daemonRootDir)
	for _, r := range records {
		if r.Level == "error" && r.Context["cause"] == "x" {
			return
		}
	}
}`,
			want: false,
		},
		{
			name: "a loop that also matches by substring is its own rule, not a copy",
			src: `package p
func logged(records []dlog.Record, level, substr string) bool {
	for _, r := range records {
		if r.Level == level && strings.Contains(r.Message, substr) {
			return true
		}
	}
	return false
}`,
			want: false,
		},
		{
			name: "a loop over another package's record type is not dlog's to serve",
			src: `package p
func holds(records []harness.LogRecord, operation string) bool {
	for _, r := range records {
		if r.Operation == operation {
			return true
		}
	}
	return false
}`,
			want: false,
		},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange.
			parsed, err := parser.ParseFile(token.NewFileSet(), "x_test.go", tc.src, 0)
			if err != nil {
				t.Fatalf("parse: %v", err)
			}
			fn := parsed.Decls[0].(*ast.FuncDecl)

			// Act.
			got := handRollsRecordLookup(fn)

			// Assert.
			if got != tc.want {
				t.Fatalf("handRollsRecordLookup = %t, want %t", got, tc.want)
			}
		})
	}
}

// handRollsRecordLookup reports whether FN, a function that names dlog, ranges
// over something with an if whose condition is nothing but == comparisons of
// a record's Level, Operation or Message against FN's own parameters.
func handRollsRecordLookup(fn *ast.FuncDecl) bool {
	if !namesDlog(fn) {
		return false
	}
	params := map[string]bool{}
	for _, field := range fn.Type.Params.List {
		for _, name := range field.Names {
			params[name.Name] = true
		}
	}
	found := false
	ast.Inspect(fn.Body, func(n ast.Node) bool {
		loop, ok := n.(*ast.RangeStmt)
		if !ok || found {
			return !found
		}
		for _, stmt := range loop.Body.List {
			if cond, ok := stmt.(*ast.IfStmt); ok && onlyRecordFieldsAgainst(cond.Cond, params) {
				found = true
			}
		}
		return !found
	})
	return found
}

// onlyRecordFieldsAgainst reports whether EXPR is a conjunction of
// `x.<Level|Operation|Message> == <parameter>` comparisons alone.
func onlyRecordFieldsAgainst(expr ast.Expr, params map[string]bool) bool {
	bin, ok := expr.(*ast.BinaryExpr)
	if !ok {
		return false
	}
	switch bin.Op {
	case token.LAND:
		return onlyRecordFieldsAgainst(bin.X, params) && onlyRecordFieldsAgainst(bin.Y, params)
	case token.EQL:
		sel, ok := bin.X.(*ast.SelectorExpr)
		if !ok {
			return false
		}
		switch sel.Sel.Name {
		case "Level", "Operation", "Message":
		default:
			return false
		}
		ident, ok := bin.Y.(*ast.Ident)
		return ok && params[ident.Name]
	default:
		return false
	}
}

// namesDlog reports whether FN refers to the dlog package anywhere.
func namesDlog(fn *ast.FuncDecl) bool {
	named := false
	ast.Inspect(fn, func(n ast.Node) bool {
		if sel, ok := n.(*ast.SelectorExpr); ok {
			if pkg, ok := sel.X.(*ast.Ident); ok && pkg.Name == "dlog" {
				named = true
			}
		}
		return !named
	})
	return named
}

// submitterHome is the one package that may declare the submission
// interface: prompthandler.Submitter.
const submitterHome = "internal/prompthandler"

// TestNoPackageHandRollsTheSubmitterInterface guards prompthandler.Submitter
// as the ONE declaration of the submission path a daemon producer holds. An
// interface elsewhere that declares a Submit taking a
// *conversationv1.UserSaid is a hand-rolled copy of it, free to drift from
// the handler it is handed; such a package aliases prompthandler.Submitter
// instead.
func TestNoPackageHandRollsTheSubmitterInterface(t *testing.T) {
	sourcescan.InPackage(t, daemonRootDir)
	// Arrange.
	files := token.NewFileSet()
	var copies []string

	// Act.
	for _, root := range []string{"cmd", "internal"} {
		err := filepath.WalkDir(root, func(path string, entry fs.DirEntry, walkErr error) error {
			if walkErr != nil {
				return walkErr
			}
			if entry.IsDir() || !strings.HasSuffix(path, ".go") || strings.HasSuffix(path, "_test.go") {
				return nil
			}
			if filepath.Dir(path) == submitterHome {
				return nil
			}
			parsed, err := parser.ParseFile(files, path, nil, 0)
			if err != nil {
				return err
			}
			for _, name := range handRolledSubmitters(parsed) {
				copies = append(copies, path+": "+name)
			}
			return nil
		})
		if err != nil {
			t.Fatalf("walk %s: %v", root, err)
		}
	}

	// Assert.
	if len(copies) != 0 {
		t.Fatalf("interfaces hand-roll the submission path; alias prompthandler.Submitter instead:\n%s", strings.Join(copies, "\n"))
	}
}

func TestTheSubmitterGuardRecognizesAHandRolledCopy(t *testing.T) {
	sourcescan.InPackage(t, daemonRootDir)
	tests := []struct {
		name string
		src  string
		want []string
	}{
		{
			name: "an interface declaring Submit over a UserSaid is a copy",
			src: `package p
type Prompts interface {
	Submit(ctx context.Context, ws ids.WorkspaceID, said *conversationv1.UserSaid, key string) (prompthandler.Outcome, error)
}`,
			want: []string{"Prompts"},
		},
		{
			name: "an alias of prompthandler.Submitter is not",
			src: `package p
type Prompts = prompthandler.Submitter`,
		},
		{
			name: "a Submit over another payload is not",
			src: `package p
type Queue interface {
	Submit(ctx context.Context, sub promptqueue.Submission) (promptqueue.Disposition, error)
}`,
		},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange.
			parsed, err := parser.ParseFile(token.NewFileSet(), "p.go", tt.src, 0)
			if err != nil {
				t.Fatalf("parse: %v", err)
			}

			// Act.
			got := handRolledSubmitters(parsed)

			// Assert.
			if strings.Join(got, ",") != strings.Join(tt.want, ",") {
				t.Fatalf("handRolledSubmitters = %v, want %v", got, tt.want)
			}
		})
	}
}

// handRolledSubmitters names every interface FILE declares with a Submit
// method taking a *conversationv1.UserSaid.
func handRolledSubmitters(file *ast.File) []string {
	var names []string
	ast.Inspect(file, func(node ast.Node) bool {
		spec, ok := node.(*ast.TypeSpec)
		if !ok || spec.Assign.IsValid() {
			return true
		}
		iface, ok := spec.Type.(*ast.InterfaceType)
		if !ok {
			return true
		}
		for _, method := range iface.Methods.List {
			fn, ok := method.Type.(*ast.FuncType)
			if !ok || len(method.Names) != 1 || method.Names[0].Name != "Submit" {
				continue
			}
			if takesUserSaid(fn) {
				names = append(names, spec.Name.Name)
			}
		}
		return true
	})
	return names
}

// takesUserSaid reports whether FN has a *conversationv1.UserSaid parameter.
func takesUserSaid(fn *ast.FuncType) bool {
	for _, param := range fn.Params.List {
		star, ok := param.Type.(*ast.StarExpr)
		if !ok {
			continue
		}
		sel, ok := star.X.(*ast.SelectorExpr)
		if ok && sel.Sel.Name == "UserSaid" {
			return true
		}
	}
	return false
}

// TestTheRetiredKeepaliveScanFlagsAPlantedIdentifier pins the scanner itself,
// so the guard above cannot pass by no longer seeing what it guards.
func TestTheRetiredKeepaliveScanFlagsAPlantedIdentifier(t *testing.T) {
	sourcescan.InPackage(t, daemonRootDir)
	tests := []struct {
		name string
		body string
		want []string
	}{
		{
			name: "a planted re-drive call is flagged with its file and line",
			body: "package x\n\nfunc f() {\n\tq.redriveBehindKeepalive(ctx)\n}\n",
			want: []string{"internal/x/x.go:4: redriveBehindKeepalive"},
		},
		{
			name: "a prefix of the re-drive's bound is flagged",
			body: "package x\n\nconst n = keepaliveRedriveMaxAttempts\n",
			want: []string{"internal/x/x.go:3: keepaliveRedrive"},
		},
		{
			name: "a read of the retired keepalive flag is flagged",
			body: "package x\n\nvar b = tao.GetKeepalive()\n",
			want: []string{"internal/x/x.go:3: GetKeepalive("},
		},
		{
			name: "a doc comment naming the retired hold is flagged",
			body: "package x\n\n// HeldPromptKeepAliveHold is gone.\n",
			want: []string{"internal/x/x.go:3: HeldPromptKeepAliveHold"},
		},
		{
			name: "the shim's keepalive_failed session fault is not flagged",
			body: "package x\n\nvar k = f.GetKeepaliveFailed()\n",
		},
		{
			name: "a file without any retired name is not flagged",
			body: "package x\n\nfunc f() {}\n",
		},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange.
			source := strings.NewReader(tt.body)

			// Act.
			got, err := retiredKeepaliveHits("internal/x/x.go", source)

			// Assert.
			if err != nil {
				t.Fatalf("scan: %v", err)
			}
			if !slices.Equal(got, tt.want) {
				t.Fatalf("hits = %q, want %q", got, tt.want)
			}
		})
	}
}
