package envc

import (
	"strings"
	"testing"
)

func TestRequireTestRun(t *testing.T) {
	tests := []struct {
		name        string
		forbid      bool
		consequence string
		want        string
	}{
		{name: "a test run honors the seam", forbid: true, consequence: "a live daemon does not", want: ""},
		{name: "a live daemon refuses it, saying what it does instead", consequence: "a live daemon does not",
			want: "AGENT_REPL_X=1 is a test-run seam and is honored only with " + EnvForbidVendorCalls + " set; a live daemon does not"},
		{name: "a live daemon refuses it with no consequence named",
			want: "AGENT_REPL_X=1 is a test-run seam and is honored only with " + EnvForbidVendorCalls + " set"},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange
			c := Contracts{forbidVendorCalls: tc.forbid}

			// Act
			err := c.RequireTestRun("AGENT_REPL_X=1", tc.consequence)

			// Assert
			got := ""
			if err != nil {
				got = err.Error()
			}
			if got != tc.want {
				t.Fatalf("RequireTestRun = %q, want %q", got, tc.want)
			}
		})
	}
}

func TestRequireTestMetricsRoot(t *testing.T) {
	tests := []struct {
		name      string
		testRun   bool
		root      string
		wantError bool
	}{
		{name: "a live daemon keeps any root", root: "/Users/someone/.cache/agent-repl/metrics"},
		{name: "a test run with no root", testRun: true},
		{name: "a test run's root inside its temporary directory", testRun: true, root: "/tmp/run/T/metrics"},
		{name: "a test run's root outside it", testRun: true, root: "/Users/someone/.cache/agent-repl/metrics", wantError: true},
		{name: "a sibling that shares the prefix", testRun: true, root: "/tmp/run/Tother/metrics", wantError: true},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			c := Contracts{forbidVendorCalls: tt.testRun}

			// Act
			err := c.RequireTestMetricsRoot(tt.root, "/tmp/run/T")

			// Assert
			if (err != nil) != tt.wantError {
				t.Fatalf("RequireTestMetricsRoot = %v, want error %v", err, tt.wantError)
			}
			if err != nil && !strings.Contains(err.Error(), "never writes the live metrics store") {
				t.Fatalf("err = %v, want it to say why", err)
			}
		})
	}
}
