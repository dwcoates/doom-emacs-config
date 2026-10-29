package workspace

import (
	"context"
	"testing"

	"google.golang.org/protobuf/proto"

	agentreplv1 "agentrepl/proto/agentrepl/v1"
	conversationv1 "agentrepl/proto/conversation/v1"
	shimv1 "agentrepl/proto/shim/v1"

	"agent-repld/internal/dlog"
	"agent-repld/internal/wsm"
)

// paidRead is the payment for the read coldResponse states on vendor-1.
func paidRead() wsm.ColdReadPayment {
	return wsm.ColdReadPayment{VendorSessionID: "vendor-1", LastRequestAtMs: 1, Model: "opus", PaidAt: fixedNow}
}

// coldAt is coldResponse's refusal for the read whose last request was at AT.
func coldAt(at int64) *shimv1.StartSessionResponse {
	response := coldResponse()
	response.GetFailure().GetCold().LastRequestAtMs = at
	return response
}

// clearRemediation discards the context.
func clearRemediation() *conversationv1.SessionColdRemediation {
	return &conversationv1.SessionColdRemediation{
		Remediation: &conversationv1.SessionColdRemediation_Clear{Clear: &conversationv1.SessionColdClear{}},
	}
}

// coldPaymentRecord finds the cold read payment record carrying MESSAGE.
func coldPaymentRecord(t *testing.T, f *fleetFixture, message string) dlog.Record {
	t.Helper()
	for _, record := range f.log.logger.Records() {
		if record.Operation == opColdReadPayment && record.Message == message {
			return record
		}
	}
	t.Fatalf("no %s record %q in %+v", opColdReadPayment, message, f.log.logger.Records())
	return dlog.Record{}
}

// startedWithPay answers whether REQ resumes paying the cold read.
func startedWithPay(req *shimv1.StartSessionRequest) bool {
	return req.GetResume().GetColdRemediation().GetPay() != nil
}

func TestResumeColdWithPayRecordsTheReadTheGateNamed(t *testing.T) {
	// Arrange.
	f := newFleetFixture(t)
	ws := f.workspace("w1")
	parkedGate(t, f, ws)

	// Act.
	if err := f.fleet.ResumeCold(context.Background(), ws.ID,
		ColdResume{VendorSessionID: "vendor-1", Remediation: payRemediation()}); err != nil {
		t.Fatalf("ResumeCold: %v", err)
	}

	// Assert.
	if got, ok := f.db.coldPayment(ws.ID); !ok || got != paidRead() {
		t.Fatalf("payment = %+v (found=%v), want %+v", got, ok, paidRead())
	}
	record := coldPaymentRecord(t, f, "recorded the cold read the user paid for")
	if record.Context["vendor_session_id"] != "vendor-1" || record.Context["last_request_at_ms"] != int64(1) {
		t.Fatalf("record context = %+v, want the conversation and its last request", record.Context)
	}
}

func TestResumeColdWithClearDropsTheStandingPayment(t *testing.T) {
	// Arrange.
	f := newFleetFixture(t)
	ws := f.workspace("w1")
	parkedGate(t, f, ws)
	if err := f.db.PutColdReadPayment(context.Background(), ws.ID, paidRead()); err != nil {
		t.Fatalf("seed the payment: %v", err)
	}

	// Act.
	if err := f.fleet.ResumeCold(context.Background(), ws.ID,
		ColdResume{VendorSessionID: "vendor-1", Remediation: clearRemediation()}); err != nil {
		t.Fatalf("ResumeCold: %v", err)
	}

	// Assert.
	if _, ok := f.db.coldPayment(ws.ID); ok {
		t.Fatal("the payment still stands after a clear rewrote the context")
	}
}

func TestResumeColdRefusesTheReopenWhenItsAnswerCannotBeRecorded(t *testing.T) {
	tests := []struct {
		name    string
		arrange func(f *fleetFixture)
		remedy  *conversationv1.SessionColdRemediation
		message string
	}{
		{
			name:    "a pay whose payment will not write",
			arrange: func(f *fleetFixture) { f.db.putColdErr = errFake },
			remedy:  payRemediation(),
			message: "the cold read payment could not be recorded; the re-open is refused",
		},
		{
			name:    "a clear whose standing payment will not drop",
			arrange: func(f *fleetFixture) { f.db.dropColdErr = errFake },
			remedy:  clearRemediation(),
			message: "the standing cold read payment could not be dropped; the re-open is refused",
		},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange.
			f := newFleetFixture(t)
			ws := f.workspace("w1")
			parkedGate(t, f, ws)
			tt.arrange(f)
			asked := len(f.client.requests)

			// Act.
			err := f.fleet.ResumeCold(context.Background(), ws.ID,
				ColdResume{VendorSessionID: "vendor-1", Remediation: tt.remedy})

			// Assert.
			if err == nil {
				t.Fatal("ResumeCold = nil, want the record's failure")
			}
			if len(f.client.requests) != asked {
				t.Fatalf("the shim was asked %d more times, want the re-open refused before it", len(f.client.requests)-asked)
			}
			record := coldPaymentRecord(t, f, tt.message)
			if record.Context["cause"] != errFake.Error() || record.Context["vendor_session_id"] != "vendor-1" {
				t.Fatalf("record context = %+v, want the cause and the conversation", record.Context)
			}
		})
	}
}

func TestResumeColdWithPayRefusesWhenTheGatesFactsAreGone(t *testing.T) {
	// Arrange: a live session with no gate ever raised on it.
	f := newFleetFixture(t)
	ws := f.workspace("w1")
	f.db.sessions[ws.ID] = wsm.Session{Workspace: ws.ID, VendorSessionID: "vendor-1", HostSessionID: "host-1"}
	f.fleet.remember(ws.ID, &live{client: f.client})

	// Act.
	err := f.fleet.ResumeCold(context.Background(), ws.ID,
		ColdResume{VendorSessionID: "vendor-1", Remediation: payRemediation()})

	// Assert.
	if err == nil {
		t.Fatal("ResumeCold = nil, want a refusal naming the missing facts")
	}
	if len(f.client.requests) != 0 {
		t.Fatalf("the shim was asked %d times, want none", len(f.client.requests))
	}
	if _, ok := f.db.coldPayment(ws.ID); ok {
		t.Fatal("a payment was recorded with no read to name")
	}
	// NO GATE STANDS, so the re-open is refused before any payment is
	// touched: a gate is raised with its cold facts in one step, so a standing
	// gate without them is unrepresentable.
	asRefusal(t, err, ArmNoSession)
}

func TestABringUpRefusedColdForAPaidReadPaysIt(t *testing.T) {
	// Arrange: the user paid for this read; then a bounce restarted the shim.
	f := newFleetFixture(t)
	ws := f.workspace("w1")
	f.db.sessions[ws.ID] = wsm.Session{Workspace: ws.ID, VendorSessionID: "vendor-1"}
	if err := f.db.PutColdReadPayment(context.Background(), ws.ID, paidRead()); err != nil {
		t.Fatalf("seed the payment: %v", err)
	}
	f.client.responses = []*shimv1.StartSessionResponse{coldResponse(), startedResponse("vendor-1")}

	// Act.
	if err := f.fleet.Start(context.Background(), ws.ID); err != nil {
		t.Fatalf("Start: %v", err)
	}

	// Assert.
	if len(f.client.requests) != 2 || startedWithPay(f.client.requests[0]) || !startedWithPay(f.client.requests[1]) {
		t.Fatalf("requests = %v, want a plain resume then one paying the read", f.client.requests)
	}
	if _, serving := f.fleet.Health(ws.ID); !serving {
		t.Fatal("the paid resume is not serving")
	}
	if f.footer.coldGates[ws.ID].Standing {
		t.Fatal("the footer stood a gate for a read the user already paid for")
	}
	if _, gated := f.fleet.ColdGateDetail(ws.ID); gated {
		t.Fatal("a gate stands for a read the user already paid for")
	}
}

func TestABringUpRefusedColdForADifferentReadDropsThePaymentAndAsks(t *testing.T) {
	tests := []struct {
		name     string
		response *shimv1.StartSessionResponse
	}{
		{name: "a request was made since the payment", response: coldAt(2)},
		{name: "the read is priced for another model", response: func() *shimv1.StartSessionResponse {
			r := coldResponse()
			r.GetFailure().GetCold().RequestedModel = &conversationv1.AgentModel{Name: "sonnet"}
			return r
		}()},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange.
			f := newFleetFixture(t)
			ws := f.workspace("w1")
			f.db.sessions[ws.ID] = wsm.Session{Workspace: ws.ID, VendorSessionID: "vendor-1"}
			if err := f.db.PutColdReadPayment(context.Background(), ws.ID, paidRead()); err != nil {
				t.Fatalf("seed the payment: %v", err)
			}
			f.client.response = tt.response

			// Act.
			if err := f.fleet.Start(context.Background(), ws.ID); err != nil {
				t.Fatalf("Start: %v", err)
			}

			// Assert.
			if len(f.client.requests) != 1 {
				t.Fatalf("requests = %d, want the one plain resume", len(f.client.requests))
			}
			if !f.footer.coldGates[ws.ID].Standing {
				t.Fatal("no gate stands for a read nobody paid for")
			}
			if _, ok := f.db.coldPayment(ws.ID); ok {
				t.Fatal("the spent payment still stands")
			}
			coldPaymentRecord(t, f, "the standing cold read payment names a different read; it is spent and dropped")
		})
	}
}

func TestABringUpRefusesWhenThePaymentCannotBeSettled(t *testing.T) {
	tests := []struct {
		name     string
		arrange  func(f *fleetFixture)
		response *shimv1.StartSessionResponse
		message  string
	}{
		{
			name:     "the payment will not read",
			arrange:  func(f *fleetFixture) { f.db.coldPaymentOfErr = errFake },
			response: coldResponse(),
			message:  "the cold read payment could not be read; the bring-up is refused",
		},
		{
			name:     "the spent payment will not drop",
			arrange:  func(f *fleetFixture) { f.db.dropColdErr = errFake },
			response: coldAt(2),
			message:  "the spent cold read payment could not be dropped; the bring-up is refused",
		},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange.
			f := newFleetFixture(t)
			ws := f.workspace("w1")
			f.db.sessions[ws.ID] = wsm.Session{Workspace: ws.ID, VendorSessionID: "vendor-1"}
			if err := f.db.PutColdReadPayment(context.Background(), ws.ID, paidRead()); err != nil {
				t.Fatalf("seed the payment: %v", err)
			}
			tt.arrange(f)
			f.client.response = tt.response

			// Act.
			err := f.fleet.Start(context.Background(), ws.ID)

			// Assert.
			if err == nil {
				t.Fatal("Start = nil, want the payment's failure")
			}
			if f.footer.coldGates[ws.ID].Standing {
				t.Fatal("a gate stood although whether the read was paid is unknown")
			}
			record := coldPaymentRecord(t, f, tt.message)
			if record.Context["cause"] != errFake.Error() {
				t.Fatalf("record context = %+v, want the cause", record.Context)
			}
		})
	}
}

func TestCoversColdReadNamesOneRead(t *testing.T) {
	cold := coldResponse().GetFailure().GetCold()
	tests := []struct {
		name    string
		payment wsm.ColdReadPayment
		vendor  string
		want    bool
	}{
		{name: "the same read", payment: paidRead(), vendor: "vendor-1", want: true},
		{name: "another conversation", payment: paidRead(), vendor: "vendor-2", want: false},
		{name: "another last request", payment: wsm.ColdReadPayment{VendorSessionID: "vendor-1", LastRequestAtMs: 2, Model: "opus"}, vendor: "vendor-1", want: false},
		{name: "another model", payment: wsm.ColdReadPayment{VendorSessionID: "vendor-1", LastRequestAtMs: 1, Model: "sonnet"}, vendor: "vendor-1", want: false},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Act.
			got := coversColdRead(tt.payment, tt.vendor, cold)

			// Assert.
			if got != tt.want {
				t.Fatalf("coversColdRead = %v, want %v", got, tt.want)
			}
		})
	}
}

// TestEveryPayAnswerResumesWithOnePayRemediation pins that the gate's own pay
// answer and the bring-up paying a paid read resume with the one remediation.
func TestEveryPayAnswerResumesWithOnePayRemediation(t *testing.T) {
	// Arrange.
	answer := &agentreplv1.AnswerColdGateRequest{Choice: &agentreplv1.AnswerColdGateRequest_Pay{Pay: &agentreplv1.AnswerColdGatePay{}}}

	// Act.
	got, err := coldRemediation(dlog.NewTestLogger(), ServedColdGate{}, answer)

	// Assert.
	if err != nil || !proto.Equal(got, payRemediation()) {
		t.Fatalf("coldRemediation(pay) = %v, %v, want payRemediation()", got, err)
	}
}
