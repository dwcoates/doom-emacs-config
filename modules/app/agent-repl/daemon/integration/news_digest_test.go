//go:build integration

package integration

import (
	"context"
	"fmt"
	"net/http"
	"net/http/httptest"
	"os"
	"path/filepath"
	"testing"
	"time"

	"connectrpc.com/connect"

	agentreplv1 "agentrepl/proto/agentrepl/v1"
	frontendv1 "agentrepl/proto/frontend/v1"

	"claude-repld/integration/harness"
)

// newsDigestAnswer is the condensing call's answer the fake claude gives: one
// backend item linking the fixture feed's entry, marked as a regression risk.
const newsDigestAnswer = `{\"sections\":[{\"kind\":\"backend\",\"items\":[{\"title\":\"SDK drops subscription billing\",\"summary\":\"The SDK now needs an API key.\",\"effective\":\"2026-11-01\",\"risk\":\"Subscription sign-in in the shim stops working.\",\"links\":[{\"label\":\"Release v9\",\"url\":\"https://fixture.test/releases/v9\"}]}]}]}`

// newsDigestEnv serves one Atom source from a loopback fixture server and
// names a fake claude that answers the condensing call with newsDigestAnswer.
// The schedule is held off (a day's start delay), so only RefreshNewsDigest
// runs a digest.
func newsDigestEnv(t *testing.T) []string {
	t.Helper()
	feed := fmt.Sprintf(`<feed xmlns="http://www.w3.org/2005/Atom"><entry><id>v9</id><title>v9</title>`+
		`<updated>%s</updated><link href="https://fixture.test/releases/v9"/>`+
		`<content type="html">&lt;p&gt;Subscription billing is removed.&lt;/p&gt;</content></entry></feed>`,
		time.Now().Add(-time.Hour).UTC().Format(time.RFC3339))
	srv := httptest.NewServer(http.HandlerFunc(func(w http.ResponseWriter, _ *http.Request) {
		_, _ = w.Write([]byte(feed))
	}))
	t.Cleanup(srv.Close)
	dir := t.TempDir()
	sources := filepath.Join(dir, "sources.json")
	writeTestFile(t, sources, fmt.Sprintf(
		`[{"key":"sdk","name":"Agent SDK releases","url":%q,"home":"https://fixture.test/releases","format":"atom"}]`, srv.URL))
	script := "#!/bin/sh\ncat > /dev/null\nprintf '%s' '{\"type\":\"result\",\"subtype\":\"success\",\"is_error\":false,\"result\":\"" + newsDigestAnswer + "\"}'\n"
	return []string{
		"AGENT_REPL_NEWS_DIGEST_SOURCES=" + sources,
		"AGENT_REPL_NEWS_DIGEST_START_DELAY=24h",
		scriptedClaudeEnv(t, script),
	}
}

// writeTestFile writes content at path.
func writeTestFile(t *testing.T, path, content string) {
	t.Helper()
	if err := os.WriteFile(path, []byte(content), 0o644); err != nil {
		t.Fatalf("write %s: %v", path, err)
	}
}

// refreshDigest runs RefreshNewsDigest and answers the digest that now stands
// on stream.
func refreshDigest(t *testing.T, d *harness.Daemon, stream *harness.Stream[*agentreplv1.WatchDaemonResponse]) *frontendv1.NewsDigestOverlay {
	t.Helper()
	ctx, cancel := context.WithTimeout(d.Ctx(), harness.DefaultTimeout)
	defer cancel()
	resp, err := d.Client().RefreshNewsDigest(ctx, connect.NewRequest(&agentreplv1.RefreshNewsDigestRequest{}))
	if err != nil {
		t.Fatalf("RefreshNewsDigest: %v", err)
	}
	if resp.Msg.GetSuccess().GetShown().GetItems() != 1 {
		t.Fatalf("RefreshNewsDigest = %v, want a digest of one item shown", resp.Msg)
	}
	return awaitShown(t, d, stream)
}

// awaitShown waits for a shown digest on stream.
func awaitShown(t *testing.T, d *harness.Daemon, stream *harness.Stream[*agentreplv1.WatchDaemonResponse]) *frontendv1.NewsDigestOverlay {
	t.Helper()
	return harness.AwaitView(t, d.Ctx(), stream, "the shown news digest", func(r *agentreplv1.WatchDaemonResponse) bool {
		return r.GetNewsDigest().GetShown() != nil
	}).GetNewsDigest().GetShown()
}

func TestARefreshedNewsDigestStandsInEveryWebview(t *testing.T) {
	t.Parallel()
	// Arrange.
	d := newDaemon(t, harness.Opts{ExtraEnv: newsDigestEnv(t)})
	first, second := d.WatchWebviewDaemonStream(), d.WatchWebviewDaemonStream()

	// Act.
	shown := refreshDigest(t, d, first)

	// Assert.
	other := awaitShown(t, d, second)
	if other.GetId().GetValue() != shown.GetId().GetValue() {
		t.Fatalf("the webviews drew different digests: %q and %q", shown.GetId().GetValue(), other.GetId().GetValue())
	}
	item := shown.GetSections()[0].GetItems()[0]
	if shown.GetSections()[0].GetKind().GetBackend() == nil || item.GetLinks()[0].GetUrl() != "https://fixture.test/releases/v9" {
		t.Fatalf("digest = %v, want the backend item linking its release", shown)
	}
	if row := shown.GetSources().GetSources()[0]; row.GetRead().GetNewEntries() != 1 || row.GetUrl() != "https://fixture.test/releases" {
		t.Fatalf("source row = %v, want the feed read with one new entry", row)
	}
}

func TestARefreshedNewsDigestCarriesItsRiskSinceLastWeek(t *testing.T) {
	t.Parallel()
	// Arrange.
	d := newDaemon(t, harness.Opts{ExtraEnv: newsDigestEnv(t)})

	// Act.
	shown := refreshDigest(t, d, d.WatchWebviewDaemonStream())

	// Assert.
	week := shown.GetWeek()
	items := week.GetRisks().GetItems()
	if week.GetHeading().GetText() != "Since last week" || len(items) != 1 ||
		items[0].GetReason().GetText() != "Subscription sign-in in the shim stops working." ||
		items[0].GetItem().GetEffective().GetText() != "2026-11-01" {
		t.Fatalf("week = %v, want the marked item with its reason and date", week)
	}
}

func TestANewsDigestBeforeAnySessionStartsSaysTheSDKVersionIsUnknown(t *testing.T) {
	t.Parallel()
	// Arrange.
	d := newDaemon(t, harness.Opts{ExtraEnv: newsDigestEnv(t)})

	// Act.
	shown := refreshDigest(t, d, d.WatchWebviewDaemonStream())

	// Assert.
	if shown.GetHeader().GetSdkVersion().GetUnknown() == nil {
		t.Fatalf("sdk version = %v, want unknown: no session has started", shown.GetHeader().GetSdkVersion())
	}
}

func TestADismissInOneWebviewTakesTheDigestDownInEvery(t *testing.T) {
	t.Parallel()
	// Arrange.
	d := newDaemon(t, harness.Opts{ExtraEnv: newsDigestEnv(t)})
	first, second := d.WatchWebviewDaemonStream(), d.WatchWebviewDaemonStream()
	shown := refreshDigest(t, d, first)
	awaitShown(t, d, second)

	// Act.
	ctx, cancel := context.WithTimeout(d.Ctx(), harness.DefaultTimeout)
	defer cancel()
	resp, err := d.Client().DismissNewsDigest(ctx, connect.NewRequest(&agentreplv1.DismissNewsDigestRequest{Id: shown.GetId()}))

	// Assert.
	if err != nil || resp.Msg.GetSuccess() == nil {
		t.Fatalf("DismissNewsDigest = (%v, %v), want success", resp, err)
	}
	for _, s := range []*harness.Stream[*agentreplv1.WatchDaemonResponse]{first, second} {
		harness.AwaitView(t, d.Ctx(), s, "the dismissed news digest", func(r *agentreplv1.WatchDaemonResponse) bool {
			return r.GetNewsDigest().GetNone() != nil
		})
	}
}

func TestDismissingAnUnknownNewsDigestIsRefused(t *testing.T) {
	t.Parallel()
	// Arrange.
	d := newDaemon(t, harness.Opts{ExtraEnv: newsDigestEnv(t)})
	refreshDigest(t, d, d.WatchWebviewDaemonStream())

	// Act.
	ctx, cancel := context.WithTimeout(d.Ctx(), harness.DefaultTimeout)
	defer cancel()
	resp, err := d.Client().DismissNewsDigest(ctx, connect.NewRequest(&agentreplv1.DismissNewsDigestRequest{
		Id: &frontendv1.NewsDigestId{Value: "not-a-digest"},
	}))

	// Assert.
	if err != nil || resp.Msg.GetError().GetUnknownDigest() == nil {
		t.Fatalf("DismissNewsDigest = (%v, %v), want unknown_digest", resp, err)
	}
}

func TestTheStandingNewsDigestSurvivesARestart(t *testing.T) {
	t.Parallel()
	// Arrange.
	env := newsDigestEnv(t)
	d1 := newDaemon(t, harness.Opts{ExtraEnv: env})
	shown := refreshDigest(t, d1, d1.WatchWebviewDaemonStream())
	d1.Stop()

	// Act.
	d2 := harness.StartDaemon(t, harness.Opts{StateDir: d1.StateDir, ExtraEnv: env})

	// Assert.
	again := awaitShown(t, d2, d2.WatchWebviewDaemonStream())
	if again.GetId().GetValue() != shown.GetId().GetValue() {
		t.Fatalf("after the restart the digest %q stands, want %q", again.GetId().GetValue(), shown.GetId().GetValue())
	}
}

func TestASecondRefreshWithNothingNewStandsNothingNew(t *testing.T) {
	t.Parallel()
	// Arrange.
	d := newDaemon(t, harness.Opts{ExtraEnv: newsDigestEnv(t)})
	refreshDigest(t, d, d.WatchWebviewDaemonStream())

	// Act.
	ctx, cancel := context.WithTimeout(d.Ctx(), harness.DefaultTimeout)
	defer cancel()
	resp, err := d.Client().RefreshNewsDigest(ctx, connect.NewRequest(&agentreplv1.RefreshNewsDigestRequest{}))

	// Assert.
	if err != nil || resp.Msg.GetSuccess().GetNothingNew() == nil {
		t.Fatalf("RefreshNewsDigest = (%v, %v), want nothing_new", resp, err)
	}
}
