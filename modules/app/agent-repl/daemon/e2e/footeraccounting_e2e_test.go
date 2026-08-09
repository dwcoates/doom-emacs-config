// The FOOTER'S TURN-ACCOUNTING CELL, once a turn has settled.
//
// footer.proto: a turn's accounting is a RECONCILIATION — "the daemon compares
// the usage each response reported against the totals the terminal result
// claimed, and the cell reports whether that reconciliation held. The
// comparison, the verdict and the prose are all the daemon's — the client
// renders a string and picks a class from the verdict arm."
//
// The verdict is arms "rather than a flag, so that a verdict and the evidence
// it implies arrive together and a client cannot render 'invalid' with nothing
// to say about why". This test holds the daemon to EXACTLY ONE arm: none set is
// a cell with no class to render, and the reconciliation cannot have held and
// failed at once.
package e2e

import (
	"testing"

	frontendv1 "agentrepl/proto/agentshim/frontend/v1"
)

// TestE2EASettledTurnProducesAnAccountingCellWithExactlyOneVerdict covers the
// FOOTER ACCOUNTING edge.
func TestE2EASettledTurnProducesAnAccountingCellWithExactlyOneVerdict(t *testing.T) {
	// Arrange
	h := newUDSHarness(t)
	cwd := t.TempDir()
	id := h.createSession(t, cwd)
	conn := h.dial(t, id)
	if readFrame(t, conn).GetSnapshot() == nil {
		t.Fatal("first frame on the scoped stream was not a StateSnapshot")
	}

	// Act — one real turn through the whole stack. The turn SETTLING is what
	// produces the cell; the proto states it is absent until then, so a cell
	// observed before the turn ends would prove nothing about reconciliation.
	submitPrompt(t, conn, "e2e-footer-accounting", "hello footer accounting")

	// Assert
	frame := awaitFrame(t, conn, "a ProgressView carrying the accounting cell", func(f *frontendv1.FrontendFrame) bool {
		pv := f.GetProgress()
		return pv.GetWorkspace() == cwd && pv.GetAccounting() != nil
	})
	cell := frame.GetProgress().GetAccounting()
	if cell.GetVerdict() == nil {
		t.Fatalf("the accounting cell arrived with NO verdict arm set: the client has no class to render it with, and cannot tell a healthy reconciliation from one that did not add up")
	}
	if cell.GetSummary() == "" {
		t.Errorf("the accounting cell arrived with an empty summary: the client renders this string verbatim, so an empty one is a blank footer cell beside a set verdict")
	}
	if verdicts := setVerdictArms(cell); verdicts != 1 {
		t.Errorf("the accounting cell carries %d verdict arms, want exactly 1: the reconciliation cannot have held and failed at the same time", verdicts)
	}
}

// setVerdictArms counts the verdict arms a cell carries.
//
// A oneof cannot hold two arms on the wire, so this counts what the ACCESSORS
// report — which is the shape a client actually branches on, and where a daemon
// composing the cell field-by-field rather than through the oneof would show
// up.
func setVerdictArms(cell *frontendv1.FooterAccountingCell) int {
	count := 0
	if cell.GetComplete() != nil {
		count++
	}
	if cell.GetIncomplete() != nil {
		count++
	}
	if cell.GetInvalid() != nil {
		count++
	}
	return count
}
