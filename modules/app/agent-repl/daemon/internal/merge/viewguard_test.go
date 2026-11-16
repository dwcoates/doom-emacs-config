package merge

import (
	"go/ast"
	"go/parser"
	"go/token"
	"strings"
	"testing"

	"agent-repld/internal/sourcescan"
)

// callersOf answers every production function of this package whose body
// mentions NEEDLE, as "file:function".
func callersOf(t *testing.T, needle string) []string {
	t.Helper()
	var out []string
	fset := token.NewFileSet()
	for _, file := range sourcescan.Production(t) {
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
	// Act
	got := callersOf(t, "o.mintLedger(")

	// Assert
	if strings.Join(got, ",") != "recover.go:ledgerOfEntry,request.go:queueRequestAt" {
		t.Fatalf("functions minting a ledger = %v", got)
	}
}
