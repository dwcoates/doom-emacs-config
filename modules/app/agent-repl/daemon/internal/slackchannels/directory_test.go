package slackchannels

import (
	"context"
	"errors"
	"sync"
	"testing"
	"time"

	"google.golang.org/protobuf/proto"

	agentreplv1 "agentrepl/proto/agentrepl/v1"

	"agent-repld/internal/dlog"
)

// awaitBound bounds a test's wait for a read it started to be recorded. The
// fake read answers at once, so a healthy wait is well under a millisecond;
// the bound is generous only against a loaded CI host's scheduler.
const awaitBound = 5 * time.Second

// fakeReads answers each read from its queue in order, counting them, and
// holds a read until the test releases it when gate is set.
type fakeReads struct {
	mu      sync.Mutex
	answers []readAnswer
	calls   int
	gate    chan struct{}
}

type readAnswer struct {
	channels []*agentreplv1.SlackChannel
	err      error
}

func (f *fakeReads) read(ctx context.Context) ([]*agentreplv1.SlackChannel, error) {
	f.mu.Lock()
	f.calls++
	answer := f.answers[0]
	f.answers = f.answers[1:]
	gate := f.gate
	f.mu.Unlock()
	if gate != nil {
		<-gate
	}
	return answer.channels, answer.err
}

func (f *fakeReads) count() int {
	f.mu.Lock()
	defer f.mu.Unlock()
	return f.calls
}

// fakeClock is a settable clock.
type fakeClock struct {
	mu  sync.Mutex
	now time.Time
}

func (c *fakeClock) Now() time.Time {
	c.mu.Lock()
	defer c.mu.Unlock()
	return c.now
}

func (c *fakeClock) advance(d time.Duration) {
	c.mu.Lock()
	defer c.mu.Unlock()
	c.now = c.now.Add(d)
}

func channel(id, name string) *agentreplv1.SlackChannel {
	return &agentreplv1.SlackChannel{Id: id, Name: name, Visibility: &agentreplv1.SlackChannel_PublicChannel{
		PublicChannel: &agentreplv1.SlackChannelPublic{},
	}}
}

type fixture struct {
	dir   *Directory
	reads *fakeReads
	clock *fakeClock
	log   *dlog.TestLogger
}

func newFixture(t *testing.T, answers ...readAnswer) *fixture {
	t.Helper()
	f := &fixture{
		reads: &fakeReads{answers: answers},
		clock: &fakeClock{now: time.Date(2026, 11, 19, 9, 0, 0, 0, time.UTC)},
		log:   dlog.NewTestLogger(),
	}
	dir, err := New(Deps{Read: f.reads.read, Now: f.clock.Now, Life: context.Background(), Log: f.log})
	if err != nil {
		t.Fatalf("New: %v", err)
	}
	f.dir = dir
	return f
}

// awaitReads waits until n reads have been recorded.
func (f *fixture) awaitReads(t *testing.T, n int) []dlog.Record {
	t.Helper()
	ctx, cancel := context.WithTimeout(context.Background(), awaitBound)
	defer cancel()
	records, err := f.log.AwaitCount(ctx, dlog.RecordQuery{Operation: opRead}, n)
	if err != nil {
		t.Fatalf("awaiting %d reads: %v (records %v)", n, err, f.log.Records())
	}
	return records
}

func TestNewRefusesMissingDeps(t *testing.T) {
	full := Deps{
		Read: func(context.Context) ([]*agentreplv1.SlackChannel, error) { return nil, nil },
		Now:  time.Now,
		Life: context.Background(),
		Log:  dlog.NewTestLogger(),
	}
	cases := []struct {
		name  string
		strip func(*Deps)
	}{
		{"reader", func(d *Deps) { d.Read = nil }},
		{"clock", func(d *Deps) { d.Now = nil }},
		{"lifetime", func(d *Deps) { d.Life = nil }},
		{"logger", func(d *Deps) { d.Log = nil }},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange.
			deps := full
			tc.strip(&deps)

			// Act.
			_, err := New(deps)

			// Assert.
			if err == nil {
				t.Fatalf("New without a %s succeeded", tc.name)
			}
		})
	}
}

func TestFirstAskIsPendingAndStartsARead(t *testing.T) {
	// Arrange.
	f := newFixture(t, readAnswer{channels: []*agentreplv1.SlackChannel{channel("C1", "general")}})
	f.reads.gate = make(chan struct{})

	// Act.
	resp := f.dir.Response()

	// Assert.
	if resp.GetSuccess().GetPending() == nil {
		t.Fatalf("first answer = %v, want success.pending", resp.GetResult())
	}
	close(f.reads.gate)
	f.awaitReads(t, 1)
}

func TestAskAfterAReadAnswersTheChannels(t *testing.T) {
	// Arrange.
	want := []*agentreplv1.SlackChannel{channel("C1", "general"), channel("C2", "random")}
	f := newFixture(t, readAnswer{channels: want})
	f.dir.Response()
	records := f.awaitReads(t, 1)

	// Act.
	resp := f.dir.Response()

	// Assert.
	got := resp.GetSuccess().GetListed().GetChannels()
	if len(got) != len(want) || !proto.Equal(got[0], want[0]) || !proto.Equal(got[1], want[1]) {
		t.Fatalf("listed = %v, want %v", got, want)
	}
	if records[0].Level != "info" || records[0].Context["count"] != 2 {
		t.Fatalf("read record = %+v, want an info record counting 2 channels", records[0])
	}
	if f.reads.count() != 1 {
		t.Fatalf("reads = %d, want 1: a fresh list is not read again", f.reads.count())
	}
}

func TestOnlyOneReadRunsAtATime(t *testing.T) {
	// Arrange.
	f := newFixture(t, readAnswer{channels: []*agentreplv1.SlackChannel{channel("C1", "general")}})
	f.reads.gate = make(chan struct{})

	// Act.
	for range 5 {
		f.dir.Response()
	}
	close(f.reads.gate)
	f.awaitReads(t, 1)

	// Assert.
	if f.reads.count() != 1 {
		t.Fatalf("reads = %d, want 1 while one was in flight", f.reads.count())
	}
}

func TestAListMaxAgeOldIsReadAgain(t *testing.T) {
	// Arrange.
	f := newFixture(t,
		readAnswer{channels: []*agentreplv1.SlackChannel{channel("C1", "general")}},
		readAnswer{channels: []*agentreplv1.SlackChannel{channel("C1", "general"), channel("C3", "new")}})
	f.dir.Response()
	f.awaitReads(t, 1)
	f.clock.advance(MaxAge - time.Second)
	f.dir.Response()
	if f.reads.count() != 1 {
		t.Fatalf("reads = %d before MaxAge, want 1", f.reads.count())
	}
	f.clock.advance(time.Second)

	// Act.
	stale := f.dir.Response()
	f.awaitReads(t, 2)
	fresh := f.dir.Response()

	// Assert.
	if n := len(stale.GetSuccess().GetListed().GetChannels()); n != 1 {
		t.Fatalf("the ask that started the refresh answered %d channels, want the standing 1", n)
	}
	if n := len(fresh.GetSuccess().GetListed().GetChannels()); n != 2 {
		t.Fatalf("after the refresh %d channels, want 2", n)
	}
}

func TestAFailedFirstReadIsTheErrorArmAndLogged(t *testing.T) {
	// Arrange.
	f := newFixture(t, readAnswer{err: errors.New("gns exited 1: HTTP 401: Invalid or expired JWT")}, readAnswer{})
	f.reads.gate = make(chan struct{})
	f.dir.Response()
	f.reads.gate <- struct{}{}
	records := f.awaitReads(t, 1)

	// Act.
	resp := f.dir.Response()
	close(f.reads.gate)
	f.awaitReads(t, 2)

	// Assert.
	if got := resp.GetError().GetReadFailed().GetDetail(); got != "gns exited 1: HTTP 401: Invalid or expired JWT" {
		t.Fatalf("answer = %v, want error.read_failed naming the failure", resp.GetResult())
	}
	rec := records[0]
	if rec.Level != "error" || rec.Context["error"] != "gns exited 1: HTTP 401: Invalid or expired JWT" || rec.Context["had_list"] != false {
		t.Fatalf("failure record = %+v, want an error record with the cause and had_list=false", rec)
	}
	if f.reads.count() != 2 {
		t.Fatalf("reads = %d, want the ask after a failure with no list to start another", f.reads.count())
	}
}

func TestAFailedRefreshKeepsTheList(t *testing.T) {
	// Arrange.
	f := newFixture(t,
		readAnswer{channels: []*agentreplv1.SlackChannel{channel("C1", "general")}},
		readAnswer{err: errors.New("gns ran past its 40s bound")})
	f.dir.Response()
	f.awaitReads(t, 1)
	f.clock.advance(MaxAge)
	f.dir.Response()
	records := f.awaitReads(t, 2)

	// Act.
	resp := f.dir.Response()

	// Assert.
	if n := len(resp.GetSuccess().GetListed().GetChannels()); n != 1 {
		t.Fatalf("answer = %v, want the earlier list kept", resp.GetResult())
	}
	if rec := records[1]; rec.Level != "error" || rec.Context["had_list"] != true || rec.Context["kept_count"] != 1 {
		t.Fatalf("failure record = %+v, want an error record keeping 1 channel", rec)
	}
	if f.reads.count() != 2 {
		t.Fatalf("reads = %d, want a failed refresh to wait MaxAge before the next", f.reads.count())
	}
}
