package restartannounce

import (
	"errors"
	"strings"
	"testing"
	"time"
)

func fixedNow() Now {
	at := time.UnixMilli(1_700_000_000_000)
	return func() time.Time { return at }
}

func validAnnouncement() Announcement {
	return Announcement{
		Cause:          "deploy-all rebuilt the daemon",
		StopShims:      true,
		ExpectedOutage: DefaultExpectedOutage,
		AtMs:           1_700_000_000_000,
	}
}

type recordingSink struct {
	label string
	got   []Announcement
	err   error
}

func (r *recordingSink) Name() string { return r.label }

func (r *recordingSink) AnnounceRestart(a Announcement) error {
	r.got = append(r.got, a)
	return r.err
}

func TestAnnounceDeliversToEverySink(t *testing.T) {
	// Arrange.
	a, b := &recordingSink{label: "gui"}, &recordingSink{label: "host"}
	announcer, err := New(func(string, ...any) {}, a, b)
	if err != nil {
		t.Fatalf("New: %v", err)
	}

	// Act.
	if err := announcer.Announce(validAnnouncement()); err != nil {
		t.Fatalf("Announce: %v", err)
	}

	// Assert.
	if len(a.got) != 1 || len(b.got) != 1 {
		t.Fatalf("want one announcement per sink, got gui=%d host=%d", len(a.got), len(b.got))
	}
}

func TestAnnounceAttemptsRemainingSinksAfterAFailure(t *testing.T) {
	// Arrange.
	broken := &recordingSink{label: "gui", err: errors.New("socket closed")}
	healthy := &recordingSink{label: "host"}
	announcer, err := New(func(string, ...any) {}, broken, healthy)
	if err != nil {
		t.Fatalf("New: %v", err)
	}

	// Act.
	announceErr := announcer.Announce(validAnnouncement())

	// Assert.
	if len(healthy.got) != 1 {
		t.Fatalf("the healthy sink must still be attempted, got %d deliveries", len(healthy.got))
	}
	if announceErr == nil || !strings.Contains(announceErr.Error(), "socket closed") {
		t.Fatalf("the failure must be reported, got %v", announceErr)
	}
}

func TestAnnounceWithNoSinksIsALoudFailure(t *testing.T) {
	// Arrange.
	announcer, err := New(func(string, ...any) {})
	if err != nil {
		t.Fatalf("New: %v", err)
	}

	// Act.
	announceErr := announcer.Announce(validAnnouncement())

	// Assert.
	if !errors.Is(announceErr, ErrNoSinks) {
		t.Fatalf("want ErrNoSinks, got %v", announceErr)
	}
}

func TestAnnounceLogsTheUnannouncedBounce(t *testing.T) {
	// Arrange.
	var lines []string
	announcer, err := New(func(format string, args ...any) { lines = append(lines, format) })
	if err != nil {
		t.Fatalf("New: %v", err)
	}

	// Act.
	_ = announcer.Announce(validAnnouncement())

	// Assert.
	if len(lines) != 1 || !strings.Contains(lines[0], "NOT ANNOUNCED") {
		t.Fatalf("want one NOT ANNOUNCED record, got %v", lines)
	}
}

func TestAnnounceRefusesABlankCause(t *testing.T) {
	// Arrange.
	sink := &recordingSink{label: "gui"}
	announcer, err := New(func(string, ...any) {}, sink)
	if err != nil {
		t.Fatalf("New: %v", err)
	}
	ann := validAnnouncement()
	ann.Cause = ""

	// Act.
	announceErr := announcer.Announce(ann)

	// Assert.
	if announceErr == nil || len(sink.got) != 0 {
		t.Fatalf("a causeless announcement must be refused undelivered, err=%v deliveries=%d", announceErr, len(sink.got))
	}
}

func TestAnnounceRefusesAnOutageBeyondTheCap(t *testing.T) {
	// Arrange.
	ann := validAnnouncement()
	ann.ExpectedOutage = MaxExpectedOutage + time.Second

	// Act.
	err := ann.Validate()

	// Assert.
	if err == nil {
		t.Fatal("an outage hint past the cap must be refused")
	}
}

func TestAnnounceRefusesANonPositiveOutage(t *testing.T) {
	// Arrange.
	ann := validAnnouncement()
	ann.ExpectedOutage = 0

	// Act.
	err := ann.Validate()

	// Assert.
	if err == nil {
		t.Fatal("a zero outage hint must be refused")
	}
}

func TestNewRefusesANilSink(t *testing.T) {
	// Arrange / Act.
	_, err := New(func(string, ...any) {}, nil)

	// Assert.
	if err == nil {
		t.Fatal("a nil sink must be refused at construction")
	}
}

func TestNewRefusesAMissingLogger(t *testing.T) {
	// Arrange / Act.
	_, err := New(nil)

	// Assert.
	if err == nil {
		t.Fatal("an announcer without a logger must be refused")
	}
}

func TestComposeDefaultsTheOutageHint(t *testing.T) {
	// Arrange / Act.
	ann, err := Compose(fixedNow(), "SIGTERM", false, 0)

	// Assert.
	if err != nil || ann.ExpectedOutage != DefaultExpectedOutage {
		t.Fatalf("want the default hint, got %s err=%v", ann.ExpectedOutage, err)
	}
}

func TestComposeStampsTheMintTimeFromTheInjectedClock(t *testing.T) {
	// Arrange / Act.
	ann, err := Compose(fixedNow(), "SIGTERM", false, time.Second)

	// Assert.
	if err != nil || ann.AtMs != 1_700_000_000_000 {
		t.Fatalf("want the injected clock's mint time, got %d err=%v", ann.AtMs, err)
	}
}

func TestComposeCarriesTheStopShimsDecision(t *testing.T) {
	// Arrange / Act.
	ann, err := Compose(fixedNow(), "deploy", true, time.Second)

	// Assert.
	if err != nil || !ann.StopShims {
		t.Fatalf("want stop_shims carried, got %v err=%v", ann.StopShims, err)
	}
}

func TestComposeRefusesAMissingClock(t *testing.T) {
	// Arrange / Act.
	_, err := Compose(nil, "SIGTERM", false, time.Second)

	// Assert.
	if err == nil {
		t.Fatal("compose without a clock must be refused")
	}
}

func TestSinkFuncWithoutADeliveryFunctionFails(t *testing.T) {
	// Arrange.
	sink := SinkFunc{Label: "gui"}

	// Act.
	err := sink.AnnounceRestart(validAnnouncement())

	// Assert.
	if err == nil {
		t.Fatal("a sink with no delivery function must fail loudly")
	}
}
