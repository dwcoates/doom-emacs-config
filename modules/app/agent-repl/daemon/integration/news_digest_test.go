//go:build integration

package integration

import (
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

	"agent-repld/integration/grantedclock"
	"agent-repld/integration/harness"
)

// newsDigestAnswer is the condensing call's answer the fake claude gives: one
// backend item linking the fixture feed's entry, marked as a regression risk.
const newsDigestAnswer = `{\"sections\":[{\"kind\":\"backend\",\"items\":[{\"title\":\"SDK drops subscription billing\",\"summary\":\"The SDK now needs an API key.\",\"effective\":\"2026-11-01\",\"risk\":\"Subscription sign-in in the shim stops working.\",\"links\":[{\"label\":\"Release v9\",\"url\":\"https://fixture.test/releases/v9\"}]}]}]}`

// newsDigestIpoAnswer is the IPO outlook call's answer the fake claude gives.
const newsDigestIpoAnswer = `{\"anthropic\":{\"estimate\":\"Q2 2027\",\"basis\":\"Banks were hired.\",\"links\":[{\"label\":\"Report\",\"url\":\"https://press.test/anthropic\"}]},` +
	`\"openai\":{\"estimate\":\"late 2027\",\"basis\":\"The restructuring closed.\",\"links\":[{\"label\":\"Report\",\"url\":\"https://press.test/openai\"}]}}`

// newsDigestEnv serves one Atom source from a loopback fixture server and
// names a fake claude that answers the condensing call with newsDigestAnswer
// and the IPO outlook call (the one run allowed WebSearch) with
// newsDigestIpoAnswer.
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
	envelope := func(answer string) string {
		return "printf '%s' '{\"type\":\"result\",\"subtype\":\"success\",\"is_error\":false,\"result\":\"" + answer + "\"}'"
	}
	script := "#!/bin/sh\ncat > /dev/null\ncase \"$*\" in\n*WebSearch*) " + envelope(newsDigestIpoAnswer) + " ;;\n*) " + envelope(newsDigestAnswer) + " ;;\nesac\n"
	return []string{
		"AGENT_REPL_NEWS_DIGEST_SOURCES=" + sources,
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
	ctx, cancel := grantedclock.WithBound(d.Ctx(), harness.DefaultTimeout)
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

func TestARefreshedNewsDigestLeadsWithTheIpoOutlook(t *testing.T) {
	t.Parallel()
	// Arrange.
	d := newDaemon(t, harness.Opts{ExtraEnv: newsDigestEnv(t)})

	// Act.
	shown := refreshDigest(t, d, d.WatchWebviewDaemonStream())

	// Assert.
	ipo := shown.GetIpo()
	if ipo.GetHeading().GetText() != "IPO outlook" ||
		ipo.GetAnthropic().GetCompany().GetText() != "Anthropic" || ipo.GetAnthropic().GetEstimate().GetText() != "Q2 2027" ||
		ipo.GetOpenai().GetCompany().GetText() != "OpenAI" || ipo.GetOpenai().GetLinks()[0].GetUrl() != "https://press.test/openai" {
		t.Fatalf("ipo = %v, want each company's guess from the outlook call", ipo)
	}
}

func TestTheStandingNewsDigestSurvivesARestart(t *testing.T) {
	t.Parallel()
	// Arrange.
	env := newsDigestEnv(t)
	d1 := newDaemon(t, harness.Opts{DurableState: true, ExtraEnv: env})
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
