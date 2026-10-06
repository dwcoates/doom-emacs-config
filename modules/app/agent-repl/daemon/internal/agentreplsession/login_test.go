package agentreplsession

import (
	"context"
	"errors"
	"strings"
	"testing"
	"time"

	"claude-repld/internal/account"
	"claude-repld/internal/dlog"
)

// fakeLogins records every session a completed login began.
type fakeLogins struct {
	began []time.Time
}

func (l *fakeLogins) LoginCompleted(_ context.Context, at time.Time) { l.began = append(l.began, at) }

// scriptedRecords answers each read of the root from a queue.
type scriptedRecords struct {
	answers []account.LoginRecord
	errs    []error
}

func (s *scriptedRecords) read(string) (account.LoginRecord, error) {
	record, err := s.answers[0], s.errs[0]
	s.answers, s.errs = s.answers[1:], s.errs[1:]
	return record, err
}

// watchOver builds a watch whose clock reads opened at the opening and ended
// at the ending.
func watchOver(t *testing.T, records *scriptedRecords, opened, ended time.Time) (*LoginWatch, *fakeLogins, *dlog.TestLogger) {
	t.Helper()
	logins := &fakeLogins{}
	log := dlog.NewTestLogger()
	clock := []time.Time{opened, ended}
	w, err := NewLoginWatch(records.read, logins, func() time.Time {
		now := clock[0]
		if len(clock) > 1 {
			clock = clock[1:]
		}
		return now
	}, log)
	if err != nil {
		t.Fatalf("NewLoginWatch: %v", err)
	}
	return w, logins, log
}

const root = "/Users/dev/.claude"

func TestALoginFlowBeginsASessionOnlyWhenItMadeALogin(t *testing.T) {
	opened, ended := t0, t0.Add(5*time.Minute)
	stamp := t0.Add(2 * time.Minute)
	before := account.LoginRecord{Block: `{"emailAddress":"a@b.c","profileFetchedAt":1}`, ProfileFetchedAt: t0.Add(-time.Hour)}
	cases := []struct {
		name  string
		after account.LoginRecord
		want  []time.Time
	}{
		{
			name:  "a rewritten record begins the session at the vendor's profile stamp",
			after: account.LoginRecord{Block: `{"emailAddress":"a@b.c","profileFetchedAt":2}`, ProfileFetchedAt: stamp},
			want:  []time.Time{stamp},
		},
		{
			name:  "a rewritten record whose stamp predates the flow begins it when seen",
			after: account.LoginRecord{Block: `{"emailAddress":"x@y.z"}`, ProfileFetchedAt: t0.Add(-time.Minute)},
			want:  []time.Time{ended},
		},
		{
			name:  "an unchanged record made no login",
			after: before,
		},
		{
			name:  "a root naming no account made no login",
			after: account.LoginRecord{},
		},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange.
			records := &scriptedRecords{answers: []account.LoginRecord{before, tc.after}, errs: []error{nil, nil}}
			w, logins, _ := watchOver(t, records, opened, ended)
			w.LoginOpened(root)

			// Act.
			w.LoginEnded(root)

			// Assert.
			if len(logins.began) != len(tc.want) {
				t.Fatalf("began %v, want %v", logins.began, tc.want)
			}
			for i := range tc.want {
				if !logins.began[i].Equal(tc.want[i]) {
					t.Fatalf("began %v, want %v", logins.began, tc.want)
				}
			}
		})
	}
}

func TestALoginToAnEmptyRootBeginsASession(t *testing.T) {
	// Arrange: the root named no account before the flow.
	stamp := t0.Add(time.Minute)
	records := &scriptedRecords{
		answers: []account.LoginRecord{{}, {Block: `{"emailAddress":"a@b.c"}`, ProfileFetchedAt: stamp}},
		errs:    []error{nil, nil},
	}
	w, logins, _ := watchOver(t, records, t0, t0.Add(time.Hour))
	w.LoginOpened(root)

	// Act.
	w.LoginEnded(root)

	// Assert.
	if len(logins.began) != 1 || !logins.began[0].Equal(stamp) {
		t.Fatalf("began %v, want one session at %s", logins.began, stamp)
	}
}

func TestAnOpeningThatCannotBeReadIsRecordedAndDecidesNothing(t *testing.T) {
	// Arrange.
	records := &scriptedRecords{answers: []account.LoginRecord{{}}, errs: []error{errors.New("malformed")}}
	w, logins, log := watchOver(t, records, t0, t0)

	// Act.
	w.LoginOpened(root)
	w.LoginEnded(root)

	// Assert.
	if !logged(log, "error", "daemon.agentreplsession.login_opened") {
		t.Fatalf("the failed read was not recorded at ERROR: %v", log.Records())
	}
	if len(logins.began) != 0 {
		t.Fatalf("began %v, want nothing", logins.began)
	}
}

func TestAnEndingThatCannotBeReadIsRecordedAndBeginsNothing(t *testing.T) {
	// Arrange.
	records := &scriptedRecords{answers: []account.LoginRecord{{}, {}}, errs: []error{nil, errors.New("malformed")}}
	w, logins, log := watchOver(t, records, t0, t0)
	w.LoginOpened(root)

	// Act.
	w.LoginEnded(root)

	// Assert.
	for _, r := range log.Records() {
		if r.Level == "error" && r.Operation == "daemon.agentreplsession.login_ended" && r.Context["cause"] == "malformed" {
			if len(logins.began) != 0 {
				t.Fatalf("began %v, want nothing", logins.began)
			}
			return
		}
	}
	t.Fatalf("the failed read was not recorded at ERROR with its cause: %v", log.Records())
}

func TestNewLoginWatchRefusesAMissingCollaborator(t *testing.T) {
	read := func(string) (account.LoginRecord, error) { return account.LoginRecord{}, nil }
	cases := []struct {
		name     string
		read     func(string) (account.LoginRecord, error)
		sessions LoginSessions
		now      func() time.Time
		log      dlog.Logger
		want     string
	}{
		{"reader", nil, &fakeLogins{}, time.Now, dlog.NewTestLogger(), "reader is required"},
		{"session", read, nil, time.Now, dlog.NewTestLogger(), "session to begin is required"},
		{"clock", read, &fakeLogins{}, nil, dlog.NewTestLogger(), "clock is required"},
		{"logger", read, &fakeLogins{}, time.Now, nil, "logger is required"},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			// Act.
			_, err := NewLoginWatch(tc.read, tc.sessions, tc.now, tc.log)

			// Assert.
			if err == nil || !strings.Contains(err.Error(), tc.want) {
				t.Fatalf("NewLoginWatch error = %v, want %q", err, tc.want)
			}
		})
	}
}

func TestAnEndingWithNoOpeningDecidesNothing(t *testing.T) {
	// Arrange: a flow whose opening this watch never heard.
	records := &scriptedRecords{}
	w, logins, _ := watchOver(t, records, t0, t0)

	// Act.
	w.LoginEnded(root)

	// Assert.
	if len(logins.began) != 0 {
		t.Fatalf("began %v, want nothing", logins.began)
	}
}
