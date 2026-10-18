package runtimemetrics

import (
	"context"
	"errors"
	"strings"
	"sync"
	"testing"

	"agent-repld/internal/dlog"
)

// fakeVersionGit answers a scripted HEAD and tag sets, counting calls.
type fakeVersionGit struct {
	mu       sync.Mutex
	head     string
	at       []string
	merged   []string
	headErr  error
	calls    int
	resolved chan struct{}
}

func (g *fakeVersionGit) ResolveRef(_ context.Context, _, ref string) (string, error) {
	g.mu.Lock()
	defer g.mu.Unlock()
	g.calls++
	if g.resolved != nil {
		g.resolved <- struct{}{}
	}
	if ref != "HEAD" {
		return "", errors.New("unexpected ref " + ref)
	}
	return g.head, g.headErr
}

func (g *fakeVersionGit) TagsAt(context.Context, string, string) ([]string, error) {
	g.mu.Lock()
	defer g.mu.Unlock()
	return g.at, nil
}

func (g *fakeVersionGit) TagsMerged(context.Context, string, string) ([]string, error) {
	g.mu.Lock()
	defer g.mu.Unlock()
	return g.merged, nil
}

func (g *fakeVersionGit) set(head string, at, merged []string, headErr error) {
	g.mu.Lock()
	defer g.mu.Unlock()
	g.head, g.at, g.merged, g.headErr = head, at, merged, headErr
}

const headSHA = "0123456789abcdef0123456789abcdef01234567"

func TestResolveDeployedVersion(t *testing.T) {
	tests := []struct {
		name    string
		at      []string
		merged  []string
		headErr error
		want    string
		wantErr string
	}{
		{name: "a landing carries its own tag", at: []string{"agent-repl-test-checkpoint-1", "0.129.0"}, merged: []string{"0.128.0", "0.129.0"}, want: "0.129.0"},
		{name: "a build past the landing carries HEAD as build metadata", merged: []string{"0.9.0", "0.128.0", "0.10.0"}, want: "0.128.0+0123456789ab"},
		{name: "no version tag at all is an error", merged: []string{"agent-repl-test-checkpoint-1"}, wantErr: "no version tag is reachable"},
		{name: "an unresolvable HEAD is an error", headErr: errors.New("not a repository"), wantErr: "resolve HEAD of /checkout: not a repository"},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			git := &fakeVersionGit{head: headSHA, at: tt.at, merged: tt.merged, headErr: tt.headErr}

			// Act
			got, err := ResolveDeployedVersion(context.Background(), git, "/checkout")

			// Assert
			if tt.wantErr != "" {
				if err == nil || !strings.Contains(err.Error(), tt.wantErr) {
					t.Fatalf("err = %v, want it to contain %q", err, tt.wantErr)
				}
				return
			}
			if err != nil || got != tt.want {
				t.Fatalf("ResolveDeployedVersion = (%q, %v), want %q", got, err, tt.want)
			}
		})
	}
}

// runLoop starts the loop and answers a stop that waits for it to leave.
func runLoop(t *testing.T, l *VersionLoop) func() {
	t.Helper()
	ctx, cancel := context.WithCancel(context.Background())
	done := make(chan error, 1)
	go func() { done <- l.Run(ctx) }()
	return func() {
		cancel()
		if err := <-done; err != nil {
			t.Fatalf("Run: %v", err)
		}
	}
}

// resolveSynchronously drives one resolution on the test goroutine, the way
// Run does on its own, so assertions need no waiting.
func resolveSynchronously(l *VersionLoop) { l.resolve(context.Background()) }

func TestTheVersionLoopServesWhatItResolved(t *testing.T) {
	// Arrange
	log := dlog.NewTestLogger()
	s, err := Open(log, t.TempDir())
	if err != nil {
		t.Fatalf("Open: %v", err)
	}
	t.Cleanup(s.Close)
	git := &fakeVersionGit{head: headSHA, at: []string{"0.129.0"}}
	loop := s.DeployedVersionLoop(git, "/checkout")

	// Act
	resolveSynchronously(loop)
	body := scrape(t, s)

	// Assert
	if !strings.Contains(body, `agentrepl_deployed_version_info{version="0.129.0"} 1`) {
		t.Fatalf("scrape lacks the resolved version:\n%s", body)
	}
	var infos int
	for _, r := range log.Records() {
		if r.Message == "the deployed version is resolved" && r.Context["version"] == "0.129.0" {
			infos++
		}
	}
	if infos != 1 {
		t.Fatalf("resolution records = %d, want 1", infos)
	}
}

func TestAFailedResolutionWithdrawsTheVersionAndLogsOncePerCause(t *testing.T) {
	// Arrange
	log := dlog.NewTestLogger()
	s, err := Open(log, t.TempDir())
	if err != nil {
		t.Fatalf("Open: %v", err)
	}
	t.Cleanup(s.Close)
	git := &fakeVersionGit{head: headSHA, at: []string{"0.129.0"}}
	loop := s.DeployedVersionLoop(git, "/checkout")
	resolveSynchronously(loop)
	git.set("", nil, nil, errors.New("index.lock exists"))

	// Act
	resolveSynchronously(loop)
	resolveSynchronously(loop)
	body := scrape(t, s)

	// Assert
	if strings.Contains(body, DeployedVersionName+"{") {
		t.Fatalf("a failed resolution left a stale version served:\n%s", body)
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

func TestAKickReresolvesAfterADeploy(t *testing.T) {
	// Arrange
	log := dlog.NewTestLogger()
	s, err := Open(log, t.TempDir())
	if err != nil {
		t.Fatalf("Open: %v", err)
	}
	t.Cleanup(s.Close)
	git := &fakeVersionGit{head: headSHA, at: []string{"0.129.0"}, resolved: make(chan struct{}, 2)}
	loop := s.DeployedVersionLoop(git, "/checkout")
	stop := runLoop(t, loop)
	<-git.resolved // the loop's own first resolution began
	git.set(headSHA, []string{"0.130.0"}, nil, nil)

	// Act
	loop.Kick()
	<-git.resolved // the kick's resolution began
	stop()         // Run returns only once that resolution finished

	// Assert
	if !strings.Contains(scrape(t, s), `version="0.130.0"`) {
		t.Fatalf("the version after the deploy is not served")
	}
}

func TestKicksCollapseWithoutBlocking(t *testing.T) {
	// Arrange
	loop := (*Surface)(nil).DeployedVersionLoop(&fakeVersionGit{}, "/checkout")

	// Act: many kicks with nothing draining them.
	for range 5 {
		loop.Kick()
	}

	// Assert
	if len(loop.kick) != 1 {
		t.Fatalf("pending kicks = %d, want them collapsed into one", len(loop.kick))
	}
}

func TestTheOffSurfacesLoopResolvesNothing(t *testing.T) {
	// Arrange
	git := &fakeVersionGit{head: headSHA, at: []string{"0.129.0"}}
	loop := (*Surface)(nil).DeployedVersionLoop(git, "/checkout")

	// Act
	stop := runLoop(t, loop)
	loop.Kick()
	stop()

	// Assert
	if git.calls != 0 {
		t.Fatalf("the off surface's loop ran git %d times", git.calls)
	}
}
