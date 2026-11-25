package daemonrules

import (
	"go/ast"
	"go/token"
	"os"
	"strings"
	"testing"

	"agent-repld/reposcans/internal/sourcescan"
)

// internalPrwatchDir is the package these rules hold, under the daemon module's root.
const internalPrwatchDir = "internal/prwatch"

func TestEveryPollGroupsAndJudgesThroughTheSharedHelpers(t *testing.T) {
	sourcescan.InPackage(t, internalPrwatchDir)
	for _, file := range []string{"poller.go", "queuewatch.go", "runwatch.go"} {
		t.Run(file, func(t *testing.T) {
			// Arrange
			src, err := os.ReadFile(file)
			if err != nil {
				t.Fatal(err)
			}

			// Assert
			for _, handRolled := range []string{"; !seen {", "make([]*wsm.PullRequestFailure"} {
				if strings.Contains(string(src), handRolled) {
					t.Fatalf("%s hand-rolls %q; use grouped and failingOf", file, handRolled)
				}
			}
		})
	}
}

func TestEveryNoticeKindIsDeliveredThroughDeliverKind(t *testing.T) {
	sourcescan.InPackage(t, internalPrwatchDir)
	// Arrange
	src, err := os.ReadFile("poller.go")
	if err != nil {
		t.Fatal(err)
	}

	// Assert
	if n := strings.Count(string(src), "could not read the owed "); n != 1 {
		t.Fatalf("poller.go reads owed notices in %d places; every kind goes through deliverKind", n)
	}
	if n := strings.Count(string(src), "deliverKind(ctx, w, prompts, "); n != 3 {
		t.Fatalf("deliverOwed delivers %d kinds through deliverKind, want all 3", n)
	}
}

func TestOnlyAnchorAsAnchorsAndRecordsASubscription(t *testing.T) {
	sourcescan.InPackage(t, internalPrwatchDir)
	// Arrange
	var sites []string

	// Act
	sourcescan.EachSourceIn(t, ".", true, func(fset *token.FileSet, file *ast.File) {
		for _, decl := range file.Decls {
			fn, ok := decl.(*ast.FuncDecl)
			if !ok || fn.Body == nil {
				continue
			}
			anchors, records := false, false
			ast.Inspect(fn.Body, func(n ast.Node) bool {
				if sel, ok := n.(*ast.SelectorExpr); ok {
					anchors = anchors || sel.Sel.Name == "Anchor"
					records = records || sel.Sel.Name == "SubscribePullRequest"
				}
				return true
			})
			if anchors && records {
				sites = append(sites, fset.Position(fn.Pos()).String()+" "+fn.Name.Name)
			}
		}
	})

	// Assert
	if len(sites) != 1 || !strings.HasSuffix(sites[0], " anchorAs") {
		t.Fatalf("functions that anchor and record a subscription = %v, want rig.anchorAs alone", sites)
	}
}
