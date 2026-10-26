package daemon_test

import (
	"fmt"
	"go/ast"
	"go/parser"
	"go/token"
	"io/fs"
	"path/filepath"
	"sort"
	"strings"
	"testing"
)

// TestProductionDiagnosticsDoNotBypassDlog guards the daemon's one logging
// function. The allowlist names every bootstrap or emergency write that must
// work before dlog exists or after its sink has failed. treefmt's writes are
// command output to injected streams, not daemon diagnostics.
func TestProductionDiagnosticsDoNotBypassDlog(t *testing.T) {
	type allowance struct {
		path   string
		fn     string
		callee string
		count  int
		reason string
	}
	allowed := []allowance{
		{path: "cmd/agent-repld/main.go", fn: "answerMigrationKind", callee: "fmt.Fprintln", count: 2, reason: "-migration-kind-from opens no log surfaces: its whole answer is `additive` or `breaking` on stdout for the deploy that asked, or the reason on stderr, which the asking deploy carries into its own ERROR record (deploy.askBinary)"},
		{path: "cmd/agent-repld/main.go", fn: "main", callee: "fmt.Fprintln", count: 7, reason: "process bootstrap and final exit reporting exist before or after the durable surfaces; the move to the stable directory adds one, refusing a relative path flag before any surface opens; -probe-boot-claim adds two, and it opens no log surfaces at all because its whole answer is its exit status; -layout-version adds one, its whole answer being the layout number on stdout for the deploy that asked"},
		{path: "internal/dlog/logger.go", fn: "emergency", callee: "os.Stderr.Write", count: 1, reason: "a durable sink cannot record its own write failure; the echo is itself a marshalled record, written raw because dlog is what just failed"},
		{path: "cmd/agent-repld/deployverb.go", fn: "runDeployVerb", callee: "fmt.Fprintf", count: 7, reason: "the deploy verb is a CLI whose product is its output: every refusal is the verb's injected stderr, and the daemon logs every decision itself"},
		{path: "cmd/agent-repld/deployverb.go", fn: "runDeployVerb", callee: "fmt.Fprintln", count: 1, reason: "the deploy verb prints each component's decision on its injected stdout"},
		{path: "cmd/agent-repld/callverb.go", fn: "runCallVerb", callee: "fmt.Fprintf", count: 8, reason: "the call verb is a CLI whose product is its output: every refusal is the verb's injected stderr, and the daemon logs every rpc it answers itself"},
		{path: "cmd/agent-repld/callverb.go", fn: "runCallVerb", callee: "fmt.Fprintln", count: 3, reason: "the call verb prints its usage on its injected stderr and the daemon's answer on its injected stdout"},
		{path: "cmd/agent-repld/agentcli.go", fn: "runNoun", callee: "fmt.Fprintf", count: 2, reason: "the agent CLI is a command whose product is its output: its usage, its refusals and the daemon's answer are its injected stdout and stderr, and the daemon logs every rpc it answers itself"},
		{path: "cmd/agent-repld/agentcli.go", fn: "flags", callee: "fmt.Fprintf", count: 1, reason: "the agent CLI is a command whose product is its output: its usage, its refusals and the daemon's answer are its injected stdout and stderr, and the daemon logs every rpc it answers itself"},
		{path: "cmd/agent-repld/agentcli.go", fn: "failed", callee: "fmt.Fprintf", count: 1, reason: "the agent CLI is a command whose product is its output: its usage, its refusals and the daemon's answer are its injected stdout and stderr, and the daemon logs every rpc it answers itself"},
		{path: "cmd/agent-repld/agentcli.go", fn: "noted", callee: "fmt.Fprintf", count: 1, reason: "the agent CLI is a command whose product is its output: its usage, its refusals and the daemon's answer are its injected stdout and stderr, and the daemon logs every rpc it answers itself"},
		{path: "cmd/agent-repld/dryrun.go", fn: "print", callee: "fmt.Fprintf", count: 1, reason: "a dry run's whole product is the request it did not send, printed on the agent CLI's injected stdout; nothing reached the daemon to log"},
		{path: "cmd/agent-repld/agentcli.go", fn: "usageFailed", callee: "fmt.Fprintf", count: 1, reason: "the agent CLI is a command whose product is its output: its usage, its refusals and the daemon's answer are its injected stdout and stderr, and the daemon logs every rpc it answers itself"},
		{path: "cmd/agent-repld/agentcli.go", fn: "print", callee: "fmt.Fprintln", count: 1, reason: "the agent CLI is a command whose product is its output: its usage, its refusals and the daemon's answer are its injected stdout and stderr, and the daemon logs every rpc it answers itself"},
		{path: "cmd/agent-repld/agentcli.go", fn: "answer", callee: "fmt.Fprintf", count: 1, reason: "the agent CLI is a command whose product is its output: its usage, its refusals and the daemon's answer are its injected stdout and stderr, and the daemon logs every rpc it answers itself"},
		{path: "cmd/agent-repld/testredundancycli.go", fn: "testRedundancy", callee: "fmt.Fprintln", count: 2, reason: "the agent CLI is a command whose product is its output: each line of a measurement, and a refusal frame, is a line of its injected stdout, and the daemon logs the measurement it answers itself"},
		{path: "cmd/agent-repld/testcli.go", fn: "streamTestRun", callee: "fmt.Fprintln", count: 1, reason: "the agent CLI is a command whose product is its output: each event of a test run is a line of its injected stdout, and the daemon logs the run it answers itself"},
		{path: "cmd/agent-repld/agentcli.go", fn: "runtimeHealth", callee: "fmt.Fprintln", count: 1, reason: "the agent CLI is a command whose product is its output: its usage, its refusals and the daemon's answer are its injected stdout and stderr, and the daemon logs every rpc it answers itself"},
		{path: "cmd/agent-repld/roster.go", fn: "say", callee: "fmt.Fprintf", count: 1, reason: "the agent CLI is a command whose product is its output: its usage, its refusals and the daemon's answer are its injected stdout and stderr, and the daemon logs every rpc it answers itself"},
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
