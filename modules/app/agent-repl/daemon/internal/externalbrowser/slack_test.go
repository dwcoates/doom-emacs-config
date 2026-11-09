package externalbrowser_test

import (
	"reflect"
	"strings"
	"testing"

	"agent-repld/internal/externalbrowser"
	"agent-repld/internal/usersetup"
)

func TestSlackTeamsFromEnv(t *testing.T) {
	tests := []struct {
		name    string
		raw     string
		want    externalbrowser.SlackTeams
		wantErr string
	}{
		{name: "unset is no teams", raw: "", want: externalbrowser.SlackTeams{}},
		{name: "one team", raw: "acme=T0123ABCD", want: externalbrowser.SlackTeams{"acme": "T0123ABCD"}},
		{
			name: "several teams, padded",
			raw:  " acme = T0123ABCD , big-co=E0999ZZZ ",
			want: externalbrowser.SlackTeams{"acme": "T0123ABCD", "big-co": "E0999ZZZ"},
		},
		{name: "a pair with no team", raw: "acme", wantErr: "is not `<subdomain>=<team-id>`"},
		{name: "an uppercase subdomain", raw: "Acme=T0123ABCD", wantErr: "which is not a lowercase Slack subdomain"},
		{name: "a subdomain with a dot", raw: "acme.slack.com=T0123ABCD", wantErr: "which is not a lowercase Slack subdomain"},
		{name: "a team ID of the wrong shape", raw: "acme=C0123ABCD", wantErr: "which is not a Slack team ID"},
		{name: "an empty team ID", raw: "acme=", wantErr: "which is not a Slack team ID"},
		{name: "a subdomain named twice", raw: "acme=T0123ABCD,acme=T0456EFGH", wantErr: `names the subdomain "acme" twice`},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange.
			getenv := func(k string) string {
				if k != externalbrowser.EnvSlackTeams {
					t.Fatalf("read %s, want only %s", k, externalbrowser.EnvSlackTeams)
				}
				return tc.raw
			}

			// Act.
			got, err := externalbrowser.SlackTeamsFromEnv(getenv)

			// Assert.
			if tc.wantErr != "" {
				if err == nil || !strings.Contains(err.Error(), tc.wantErr) || !strings.Contains(err.Error(), usersetup.Doc) {
					t.Fatalf("SlackTeamsFromEnv(%q) = %v, want an error containing %q pointing at %s", tc.raw, err, tc.wantErr, usersetup.Doc)
				}
				return
			}
			if err != nil {
				t.Fatalf("SlackTeamsFromEnv(%q) = %v, want nil", tc.raw, err)
			}
			if !reflect.DeepEqual(got, tc.want) {
				t.Fatalf("SlackTeamsFromEnv(%q) = %v, want %v", tc.raw, got, tc.want)
			}
		})
	}
}

func TestSlackDeepLink(t *testing.T) {
	teams := externalbrowser.SlackTeams{"acme": "T0123ABCD"}
	tests := []struct {
		name     string
		url      string
		wantDeep string
		wantOK   bool
		wantWhy  string
	}{
		{
			name:     "a channel permalink",
			url:      "https://acme.slack.com/archives/C04FPU3L4JG",
			wantDeep: "slack://channel?team=T0123ABCD&id=C04FPU3L4JG",
			wantOK:   true,
		},
		{
			name:     "a message permalink",
			url:      "https://acme.slack.com/archives/C04FPU3L4JG/p1792730535549649",
			wantDeep: "slack://channel?team=T0123ABCD&id=C04FPU3L4JG&message=1792730535.549649",
			wantOK:   true,
		},
		{
			name:     "a thread reply permalink keeps its thread",
			url:      "https://acme.slack.com/archives/C04FPU3L4JG/p1793039029766939?thread_ts=1792730535.549649&cid=C04FPU3L4JG",
			wantDeep: "slack://channel?team=T0123ABCD&id=C04FPU3L4JG&message=1793039029.766939&thread_ts=1792730535.549649",
			wantOK:   true,
		},
		{
			name:     "a direct message permalink",
			url:      "https://acme.slack.com/archives/D0AB12CD34/p1792730535549649",
			wantDeep: "slack://channel?team=T0123ABCD&id=D0AB12CD34&message=1792730535.549649",
			wantOK:   true,
		},
		{
			name:     "an uppercase host and a trailing slash",
			url:      "http://ACME.Slack.com/archives/C04FPU3L4JG/",
			wantDeep: "slack://channel?team=T0123ABCD&id=C04FPU3L4JG",
			wantOK:   true,
		},
		{name: "a link that is not Slack's", url: "https://example.com/archives/C04FPU3L4JG"},
		{name: "a host that only ends in slack.com", url: "https://notslack.com/archives/C04FPU3L4JG"},
		{name: "a deeper slack.com host", url: "https://acme.enterprise.slack.com/archives/C04FPU3L4JG"},
		{name: "a subdomain no team names", url: "https://other.slack.com/archives/C04FPU3L4JG", wantWhy: "subdomain-unconfigured"},
		{name: "a slack.com page that is not a permalink", url: "https://acme.slack.com/files/U012/F034/x.png", wantWhy: "not-a-permalink"},
		{name: "a malformed channel", url: "https://acme.slack.com/archives/general", wantWhy: "not-a-permalink"},
		{name: "a malformed message segment", url: "https://acme.slack.com/archives/C04FPU3L4JG/p123", wantWhy: "not-a-permalink"},
		{name: "too many segments", url: "https://acme.slack.com/archives/C04FPU3L4JG/p1792730535549649/x", wantWhy: "not-a-permalink"},
		{name: "a malformed thread", url: "https://acme.slack.com/archives/C04FPU3L4JG/p1792730535549649?thread_ts=x", wantWhy: "not-a-permalink"},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Act.
			deep, ok, why := externalbrowser.SlackDeepLink(tc.url, teams)

			// Assert.
			if deep != tc.wantDeep || ok != tc.wantOK || why != tc.wantWhy {
				t.Fatalf("SlackDeepLink(%q) = (%q, %v, %q), want (%q, %v, %q)", tc.url, deep, ok, why, tc.wantDeep, tc.wantOK, tc.wantWhy)
			}
		})
	}
}
