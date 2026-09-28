package health

import (
	"context"
	"testing"

	"google.golang.org/protobuf/proto"

	"claude-repld/internal/dlog"
	"claude-repld/internal/ids"
	"claude-repld/internal/wsm"
)

// loudLine is a failed deploy's fault line as the sink hands it on.
func loudLine(id ids.FaultID, detail string) FaultLine {
	record := wsm.Fault{
		Kind: KindDeployFailed, OpenedAt: fixedNow,
		Evidence: DeployFailure{Step: DeployStepBuild, BuildStep: "webapp", Detail: detail}.Evidence(),
	}
	return FaultLine{
		ID: id, Kind: KindDeployFailed, At: fixedNow, Record: record,
		Topbar: FaultTopbarLine(record, true),
	}
}

// standingIDs answers the fault ids of the latest published set, and whether
// one was published at all.
func standingIDs(l *LoudFaults) ([]string, bool) {
	latest, ok := l.Topic().Latest()
	if !ok {
		return nil, false
	}
	out := []string{}
	for _, f := range latest.GetFaults() {
		out = append(out, f.GetFaultId())
	}
	return out, true
}

func TestAnOpenedLoudFaultStandsWithItsLineAndItsTypedFault(t *testing.T) {
	// Arrange
	l := NewLoudFaults(dlog.NewTestLogger())

	// Act
	l.Opened(loudLine("f-1", "error TS2322"))

	// Assert
	latest, _ := l.Topic().Latest()
	if len(latest.GetFaults()) != 1 {
		t.Fatalf("standing = %v, want the one fault", latest)
	}
	got := latest.GetFaults()[0]
	if got.GetFaultId() != "f-1" || got.GetLine() != "deploy failed: build webapp: error TS2322" ||
		got.GetOpenedAtMs() != fixedNow.UnixMilli() {
		t.Fatalf("standing fault = %v, want its id, its topbar line and its instant", got)
	}
	if got.GetFault().GetDeployFailed().GetBuild().GetStep() != "webapp" {
		t.Fatalf("typed fault = %v, want the deploy_failed build arm", got.GetFault())
	}
}

func TestLoudFaultsStandOldestFirst(t *testing.T) {
	// Arrange
	l := NewLoudFaults(dlog.NewTestLogger())
	l.Opened(loudLine("f-1", "first"))

	// Act
	l.Opened(loudLine("f-2", "second"))

	// Assert
	if got, _ := standingIDs(l); len(got) != 2 || got[0] != "f-1" || got[1] != "f-2" {
		t.Fatalf("standing = %v, want [f-1 f-2]", got)
	}
}

func TestReopeningALoudFaultRestatesItInPlace(t *testing.T) {
	// Arrange
	l := NewLoudFaults(dlog.NewTestLogger())
	l.Opened(loudLine("f-1", "first"))

	// Act
	l.Opened(loudLine("f-1", "second"))

	// Assert
	latest, _ := l.Topic().Latest()
	if len(latest.GetFaults()) != 1 || latest.GetFaults()[0].GetLine() != "deploy failed: build webapp: second" {
		t.Fatalf("standing = %v, want the one fault restated", latest)
	}
}

func TestALateSubscriberIsToldWhatStands(t *testing.T) {
	// Arrange
	ctx, cancel := context.WithCancel(context.Background())
	defer cancel()
	l := NewLoudFaults(dlog.NewTestLogger())
	l.Opened(loudLine("f-1", "error"))

	// Act
	got := <-l.Topic().Subscribe(ctx)

	// Assert
	if len(got.GetFaults()) != 1 || got.GetFaults()[0].GetFaultId() != "f-1" {
		t.Fatalf("first delivery = %v, want the standing fault", got)
	}
}

func TestAClosedLoudFaultLeavesTheSet(t *testing.T) {
	// Arrange
	l := NewLoudFaults(dlog.NewTestLogger())
	l.Opened(loudLine("f-1", "error"))

	// Act
	l.Closed("f-1")

	// Assert
	if got, ok := standingIDs(l); !ok || len(got) != 0 {
		t.Fatalf("standing = (%v, %v), want an empty set published", got, ok)
	}
}

func TestClosingAFaultThatIsNotStandingPublishesNothing(t *testing.T) {
	// Arrange
	l := NewLoudFaults(dlog.NewTestLogger())

	// Act
	l.Closed("f-9")

	// Assert
	if got, ok := standingIDs(l); ok {
		t.Fatalf("standing = %v, want nothing published", got)
	}
}

func TestAFaultWithNoTopbarLineIsRefusedLoudly(t *testing.T) {
	// Arrange
	log := dlog.NewTestLogger()
	l := NewLoudFaults(log)

	// Act
	l.Opened(FaultLine{ID: "f-1", Kind: KindPromptsDirMissing})

	// Assert
	if got, ok := standingIDs(l); ok {
		t.Fatalf("standing = %v, want nothing published", got)
	}
	for _, r := range log.Records() {
		if r.Level == "error" && r.Operation == opLoudFaults && r.Context["fault"] == "f-1" {
			return
		}
	}
	t.Fatalf("records = %+v, want the refusal at ERROR naming the fault", log.Records())
}

func TestAnOpenedLoudFaultIsRecordedAtInfo(t *testing.T) {
	// Arrange
	log := dlog.NewTestLogger()
	l := NewLoudFaults(log)

	// Act
	l.Opened(loudLine("f-1", "error"))

	// Assert
	for _, r := range log.Records() {
		if r.Level == "info" && r.Operation == opLoudFaults && r.Context["fault"] == "f-1" &&
			r.Context["kind"] == KindDeployFailed && r.Context["standing"] == 1 {
			return
		}
	}
	t.Fatalf("records = %+v, want the open at INFO naming the fault, its kind and the count", log.Records())
}

// The typed fault is DaemonHealth's own rendering, so the two wires cannot
// disagree about one fault.
func TestALoudFaultCarriesDaemonHealthsRendering(t *testing.T) {
	// Arrange
	l := NewLoudFaults(dlog.NewTestLogger())
	line := loudLine("f-1", "error")

	// Act
	l.Opened(line)

	// Assert
	latest, _ := l.Topic().Latest()
	if got, want := latest.GetFaults()[0].GetFault(), DaemonFaultOf(line.Record); !proto.Equal(got, want) {
		t.Fatalf("typed fault = %v, want %v", got, want)
	}
}
