package wsm

import (
	"context"
	"errors"
	"testing"

	"agent-repld/internal/envc"
)

func TestInMemoryFromEnv(t *testing.T) {
	tests := []struct {
		name    string
		forbid  string
		value   string
		want    bool
		wantErr bool
	}{
		{name: "a live daemon with no flag keeps its file", forbid: "", value: "", want: false},
		{name: "a live daemon handed the flag refuses to boot", forbid: "", value: "1", wantErr: true},
		{name: "a test-run daemon honors the flag", forbid: "1", value: "1", want: true},
		{name: "a test-run daemon with no flag keeps its file", forbid: "1", value: "", want: false},
		{name: "a malformed flag is refused even under the vendor guard", forbid: "1", value: "yes", wantErr: true},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange
			t.Setenv(envc.EnvForbidVendorCalls, tc.forbid)
			getenv := func(key string) string {
				if key == EnvTestStateInMemory {
					return tc.value
				}
				return ""
			}

			// Act
			got, err := InMemoryFromEnv(envc.Load(), getenv)

			// Assert
			if (err != nil) != tc.wantErr {
				t.Fatalf("InMemoryFromEnv error = %v, wantErr %v", err, tc.wantErr)
			}
			if got.Enabled() != tc.want {
				t.Fatalf("InMemoryFromEnv = %v, want %v", got.Enabled(), tc.want)
			}
		})
	}
}

func TestOpenForTestRunRefusesAnUnprovedProcess(t *testing.T) {
	// Act
	_, err := OpenForTestRun(context.Background(), InMemoryTestRun{})

	// Assert
	if !errors.Is(err, errNotProved) {
		t.Fatalf("OpenForTestRun = %v, want the unproved refusal", err)
	}
}

func TestOpenForTestRunOpensAWorkingDatabaseWhenProved(t *testing.T) {
	// Arrange
	ctx := context.Background()
	proof := InMemoryTestRun{enabled: true}

	// Act
	db, err := OpenForTestRun(ctx, proof)

	// Assert
	if err != nil {
		t.Fatalf("OpenForTestRun = %v, want a database", err)
	}
	t.Cleanup(func() { _ = db.Close() })
	if _, err := db.ListWorkspaces(ctx); err != nil {
		t.Fatalf("ListWorkspaces on the in-memory database = %v, want the schema in place", err)
	}
}
