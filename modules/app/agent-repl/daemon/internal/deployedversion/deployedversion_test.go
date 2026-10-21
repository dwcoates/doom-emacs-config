package deployedversion

import (
	"context"
	"errors"
	"strings"
	"sync"
	"testing"

	"agent-repld/internal/dlog"
	"agent-repld/internal/semver"
)

// fakeGit answers a scripted HEAD and tag sets, signalling each resolution.
type fakeGit struct {
	mu       sync.Mutex
	head     string
	at       []string
	merged   []string
	headErr  error
	resolved chan struct{}
}

func (g *fakeGit) ResolveRef(_ context.Context, _, ref string) (string, error) {
	g.mu.Lock()
	defer g.mu.Unlock()
	if g.resolved != nil {
		g.resolved <- struct{}{}
	}
	if ref != "HEAD" {
		return "", errors.New("unexpected ref " + ref)
	}
	return g.head, g.headErr
}

func (g *fakeGit) TagsAt(context.Context, string, string) ([]string, error) {
	g.mu.Lock()
	defer g.mu.Unlock()
	return g.at, nil
}

func (g *fakeGit) TagsMerged(context.Context, string, string) ([]string, error) {
	g.mu.Lock()
	defer g.mu.Unlock()
	return g.merged, nil
}

func (g *fakeGit) set(head string, at, merged []string, headErr error) {
	g.mu.Lock()
	defer g.mu.Unlock()
	g.head, g.at, g.merged, g.headErr = head, at, merged, headErr
}

// recordingSink keeps every version it was told, nil for a withdrawal.
type recordingSink struct {
	mu   sync.Mutex
	told []*Version
}

func (s *recordingSink) DeployedVersion(v *Version) {
	s.mu.Lock()
	defer s.mu.Unlock()
	s.told = append(s.told, v)
}

func (s *recordingSink) strings() []string {
	s.mu.Lock()
	defer s.mu.Unlock()
	out := make([]string, len(s.told))
	for i, v := range s.told {
		if v == nil {
			out[i] = "<withdrawn>"
			continue
		}
		out[i] = v.String()
	}
	return out
}

const headSHA = "0123456789abcdef0123456789abcdef01234567"

func TestResolve(t *testing.T) {
	tests := []struct {
		name    string
		at      []string
		merged  []string
		headErr error
		want    Version
		wantErr string
	}{
		{name: "a landing carries its own tag", at: []string{"agent-repl-test-checkpoint-1", "0.129.0"}, merged: []string{"0.128.0", "0.129.0"}, want: Version{Release: semver.Version{Minor: 129}}},
		{name: "a build past the landing is the last landing past by HEAD", merged: []string{"0.9.0", "0.128.0", "0.10.0"}, want: Version{Release: semver.Version{Minor: 128}, Past: "0123456789ab"}},
		{name: "no version tag at all is an error", merged: []string{"agent-repl-test-checkpoint-1"}, wantErr: "no version tag is reachable"},
		{name: "an unresolvable HEAD is an error", headErr: errors.New("not a repository"), wantErr: "resolve HEAD of /checkout: not a repository"},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			git := &fakeGit{head: headSHA, at: tt.at, merged: tt.merged, headErr: tt.headErr}

			// Act
			got, err := Resolve(context.Background(), git, "/checkout")

			// Assert
			if tt.wantErr != "" {
				if err == nil || !strings.Contains(err.Error(), tt.wantErr) {
					t.Fatalf("err = %v, want it to contain %q", err, tt.wantErr)
				}
				return
			}
			if err != nil || got != tt.want {
				t.Fatalf("Resolve = (%+v, %v), want %+v", got, err, tt.want)
			}
		})
	}
}

func TestVersionString(t *testing.T) {
	tests := []struct {
		name    string
		version Version
		want    string
	}{
		{name: "a landing is the bare release", version: Version{Release: semver.Version{Minor: 129}}, want: "0.129.0"},
		{name: "a build past a landing carries its commit as build metadata", version: Version{Release: semver.Version{Minor: 129}, Past: "0123456789ab"}, want: "0.129.0+0123456789ab"},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Act
			got := tt.version.String()

			// Assert
			if got != tt.want {
				t.Fatalf("String = %q, want %q", got, tt.want)
			}
		})
	}
}

// resolveSynchronously drives one resolution on the test goroutine, as Prime
// and each Kick do, so assertions need no waiting.
func resolveSynchronously(l *Loop) { l.Prime(context.Background()) }

func TestTheLoopTellsEverySinkWhatItResolved(t *testing.T) {
	// Arrange
	log := dlog.NewTestLogger()
	first, second := &recordingSink{}, &recordingSink{}
	loop := NewLoop(&fakeGit{head: headSHA, at: []string{"0.129.0"}}, "/checkout", log, first, second)

	// Act
	resolveSynchronously(loop)

	// Assert
	for i, sink := range []*recordingSink{first, second} {
		if got := sink.strings(); len(got) != 1 || got[0] != "0.129.0" {
			t.Fatalf("sink %d was told %v, want [0.129.0]", i, got)
		}
	}
	var infos int
	for _, r := range log.Records() {
		if r.Operation == Operation && r.Message == "the deployed version is resolved" && r.Context["version"] == "0.129.0" {
			infos++
		}
	}
	if infos != 1 {
		t.Fatalf("resolution records = %d, want 1", infos)
	}
}

func TestAnUnchangedVersionIsNotRetold(t *testing.T) {
	// Arrange
	log := dlog.NewTestLogger()
	sink := &recordingSink{}
	loop := NewLoop(&fakeGit{head: headSHA, at: []string{"0.129.0"}}, "/checkout", log, sink)

	// Act
	resolveSynchronously(loop)
	resolveSynchronously(loop)

	// Assert
	if got := sink.strings(); len(got) != 1 {
		t.Fatalf("sink was told %v, want the version once", got)
	}
}

func TestAFailedResolutionWithdrawsTheVersionAndLogsOncePerCause(t *testing.T) {
	// Arrange
	log := dlog.NewTestLogger()
	sink := &recordingSink{}
	git := &fakeGit{head: headSHA, at: []string{"0.129.0"}}
	loop := NewLoop(git, "/checkout", log, sink)
	resolveSynchronously(loop)
	git.set("", nil, nil, errors.New("index.lock exists"))

	// Act
	resolveSynchronously(loop)
	resolveSynchronously(loop)

	// Assert
	if got := sink.strings(); len(got) != 2 || got[1] != "<withdrawn>" {
		t.Fatalf("sink was told %v, want the version then one withdrawal", got)
	}
	var errs []dlog.Record
	for _, r := range log.Records() {
		if r.Level == "error" {
			errs = append(errs, r)
		}
	}
	if len(errs) != 1 || errs[0].Operation != Operation || errs[0].Context["checkout"] != "/checkout" ||
		!strings.Contains(errs[0].Context["error"].(string), "index.lock exists") {
		t.Fatalf("error records = %+v, want one naming the checkout and the cause", errs)
	}
}

func TestAFirstFailureTellsNoSink(t *testing.T) {
	// Arrange
	sink := &recordingSink{}
	loop := NewLoop(&fakeGit{headErr: errors.New("not a repository")}, "/checkout", dlog.NewTestLogger(), sink)

	// Act
	resolveSynchronously(loop)

	// Assert: a sink starts out stating none, so there is nothing to withdraw.
	if got := sink.strings(); len(got) != 0 {
		t.Fatalf("sink was told %v, want nothing", got)
	}
}

func TestAKickReresolvesAfterADeploy(t *testing.T) {
	// Arrange
	sink := &recordingSink{}
	git := &fakeGit{head: headSHA, at: []string{"0.129.0"}, resolved: make(chan struct{}, 2)}
	loop := NewLoop(git, "/checkout", dlog.NewTestLogger(), sink)
	loop.Prime(context.Background())
	<-git.resolved // the prime's resolution
	git.set(headSHA, []string{"0.130.0"}, nil, nil)
	ctx, cancel := context.WithCancel(context.Background())
	done := make(chan error, 1)
	go func() { done <- loop.Run(ctx) }()

	// Act
	loop.Kick()
	<-git.resolved // the kick's resolution began
	cancel()       // Run returns only once that resolution finished
	if err := <-done; err != nil {
		t.Fatalf("Run: %v", err)
	}

	// Assert
	if got := sink.strings(); len(got) != 2 || got[1] != "0.130.0" {
		t.Fatalf("sink was told %v, want the version after the deploy last", got)
	}
}

func TestKicksCollapseWithoutBlocking(t *testing.T) {
	// Arrange
	loop := NewLoop(&fakeGit{}, "/checkout", dlog.NewTestLogger())

	// Act: many kicks with nothing draining them.
	for range 5 {
		loop.Kick()
	}

	// Assert
	if len(loop.kick) != 1 {
		t.Fatalf("pending kicks = %d, want them collapsed into one", len(loop.kick))
	}
}

func TestRunResolvesOnlyOnAKick(t *testing.T) {
	// Arrange
	sink := &recordingSink{}
	git := &fakeGit{head: headSHA, at: []string{"0.129.0"}}
	loop := NewLoop(git, "/checkout", dlog.NewTestLogger(), sink)
	ctx, cancel := context.WithCancel(context.Background())
	done := make(chan error, 1)

	// Act: Run starts and stops with no kick.
	go func() { done <- loop.Run(ctx) }()
	cancel()
	if err := <-done; err != nil {
		t.Fatalf("Run: %v", err)
	}

	// Assert
	if got := sink.strings(); len(got) != 0 {
		t.Fatalf("sink was told %v, want nothing without a prime or a kick", got)
	}
}
