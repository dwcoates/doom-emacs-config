package slacklink

import (
	"strings"
	"testing"
)

func TestParseThread(t *testing.T) {
	tests := []struct {
		name string
		link string
		want Thread
	}{
		{
			"a root message is its own thread",
			"https://chesscom.slack.com/archives/C0123ABC/p1730000000123456",
			Thread{Channel: "C0123ABC", TS: "1730000000.123456"},
		},
		{
			"a reply posts into its root's thread",
			"https://chesscom.slack.com/archives/C0123ABC/p1730000099000001?thread_ts=1730000000.123456&cid=C0123ABC",
			Thread{Channel: "C0123ABC", TS: "1730000000.123456"},
		},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Act.
			got, err := ParseThread(tc.link)

			// Assert.
			tc.want.URL = tc.link
			if err != nil || got != tc.want {
				t.Fatalf("ParseThread(%q) = (%+v, %v), want %+v", tc.link, got, err, tc.want)
			}
		})
	}
}

func TestParseThreadRefuses(t *testing.T) {
	tests := []struct {
		name     string
		link     string
		fragment string
	}{
		{"a plain http link", "http://chesscom.slack.com/archives/C0123ABC/p1730000000123456", "not an https link"},
		{"another host", "https://example.com/archives/C0123ABC/p1730000000123456", "not an https link"},
		{"a channel link", "https://chesscom.slack.com/archives/C0123ABC", "does not link a message"},
		{"not a url", "://", "is not a url"},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Act.
			_, err := ParseThread(tc.link)

			// Assert.
			if err == nil || !strings.Contains(err.Error(), tc.fragment) {
				t.Fatalf("ParseThread(%q) = %v, want a refusal naming %q", tc.link, err, tc.fragment)
			}
		})
	}
}
