package daemon_test

import (
	"go/ast"
	"go/parser"
	"go/token"
	"io/fs"
	"path/filepath"
	"strings"
	"testing"
)

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
