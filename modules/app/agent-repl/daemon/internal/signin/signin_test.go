package signin

import (
	"context"
	"errors"
	"slices"
	"strings"
	"testing"

	"agent-repld/internal/dlog"
	"agent-repld/internal/ids"
	"agent-repld/internal/wsm"
)

// recorder records every workspace a surface was told of.
type recorder struct {
	told []ids.WorkspaceID
	err  error
}

func (r *recorder) OnSignedIn(ws ids.WorkspaceID) error {
	r.told = append(r.told, ws)
	return r.err
}

// surface adapts a recorder to the footer's and roster's error-free half.
type surface struct{ r *recorder }

func (s surface) OnSignedIn(ws ids.WorkspaceID) { s.r.told = append(s.r.told, ws) }

// rootByPrefix routes a directory under /a to root A and anything else to B.
func rootByPrefix(dir string) string {
	if strings.HasPrefix(dir, "/a/") {
		return "A"
	}
	return "B"
}

func newFanout(t *testing.T, list func(context.Context) ([]wsm.Workspace, error), feed *recorder) (*Fanout, *recorder, *recorder, *dlog.TestLogger) {
	t.Helper()
	footer, roster, log := &recorder{}, &recorder{}, dlog.NewTestLogger()
	f, err := New(Deps{
		Workspaces: list, ConfigDirFor: rootByPrefix,
		Feed: feed, Footer: surface{footer}, Roster: surface{roster}, Log: log,
	})
	if err != nil {
		t.Fatalf("New: %v", err)
	}
	return f, footer, roster, log
}

func TestSignedInTellsEveryWorkspaceOnTheRootAndNoOther(t *testing.T) {
	// Arrange
	list := func(context.Context) ([]wsm.Workspace, error) {
		return []wsm.Workspace{{ID: "w1", Dir: "/a/one"}, {ID: "w2", Dir: "/b/two"}, {ID: "w3", Dir: "/a/three"}}, nil
	}
	feed := &recorder{}
	f, footer, roster, _ := newFanout(t, list, feed)

	// Act
	f.SignedIn(context.Background(), "A")

	// Assert
	want := []ids.WorkspaceID{"w1", "w3"}
	for name, got := range map[string][]ids.WorkspaceID{"feed": feed.told, "footer": footer.told, "roster": roster.told} {
		if !slices.Equal(got, want) {
			t.Fatalf("%s told %v, want %v", name, got, want)
		}
	}
}

func TestSignedInStillLiftsTheStatusWhenTheFeedCouldNotRecord(t *testing.T) {
	// Arrange: the feed retired the rows but could not record them (its own
	// ERROR and topbar warning are the feed's to raise).
	list := func(context.Context) ([]wsm.Workspace, error) { return []wsm.Workspace{{ID: "w1", Dir: "/a/one"}}, nil }
	f, footer, roster, _ := newFanout(t, list, &recorder{err: errors.New("disk full")})

	// Act
	f.SignedIn(context.Background(), "A")

	// Assert
	if len(footer.told) != 1 || len(roster.told) != 1 {
		t.Fatalf("footer told %v, roster told %v, want w1 each", footer.told, roster.told)
	}
}

func TestSignedInWithAnUnreadableWorkspaceListTellsNothingAndLogsTheCause(t *testing.T) {
	// Arrange
	list := func(context.Context) ([]wsm.Workspace, error) { return nil, errors.New("db locked") }
	feed := &recorder{}
	f, footer, roster, log := newFanout(t, list, feed)

	// Act
	f.SignedIn(context.Background(), "A")

	// Assert
	if len(feed.told)+len(footer.told)+len(roster.told) != 0 {
		t.Fatalf("told feed %v footer %v roster %v, want nothing", feed.told, footer.told, roster.told)
	}
	for _, r := range log.Records() {
		if r.Level == "error" && r.Operation == opSignedIn && r.Context["cause"] == "db locked" && r.Context["config_dir"] == "A" {
			return
		}
	}
	t.Fatalf("no ERROR record with the cause and root: %v", log.Records())
}

func TestNewRefusesAMissingCollaborator(t *testing.T) {
	list := func(context.Context) ([]wsm.Workspace, error) { return nil, nil }
	full := func() Deps {
		return Deps{Workspaces: list, ConfigDirFor: rootByPrefix, Feed: &recorder{}, Footer: surface{&recorder{}}, Roster: surface{&recorder{}}, Log: dlog.NewTestLogger()}
	}
	cases := []struct {
		name   string
		mutate func(*Deps)
		want   string
	}{
		{"lister", func(d *Deps) { d.Workspaces = nil }, "workspace lister"},
		{"router", func(d *Deps) { d.ConfigDirFor = nil }, "account-root router"},
		{"feed", func(d *Deps) { d.Feed = nil }, "feed, the footer and the roster"},
		{"footer", func(d *Deps) { d.Footer = nil }, "feed, the footer and the roster"},
		{"roster", func(d *Deps) { d.Roster = nil }, "feed, the footer and the roster"},
		{"logger", func(d *Deps) { d.Log = nil }, "logger"},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange
			deps := full()
			tc.mutate(&deps)

			// Act
			_, err := New(deps)

			// Assert
			if err == nil || !strings.Contains(err.Error(), tc.want) {
				t.Fatalf("New error = %v, want %q", err, tc.want)
			}
		})
	}
}
