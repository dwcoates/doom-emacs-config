package testtimes

import (
	"context"
	"io"
	"net/http"
	"net/http/httptest"
	"os"
	"path/filepath"
	"slices"
	"strings"
	"testing"
	"time"
)

func TestTakeTarget(t *testing.T) {
	tests := []struct {
		name    string
		env     map[string]string
		want    Target
		wantErr string
	}{
		{name: "neither set is no push", env: map[string]string{}},
		{name: "both set name the push", env: map[string]string{EnvRoot: "/m", EnvTrigger: "merge_queue"}, want: Target{Root: "/m", Trigger: "merge_queue"}},
		{name: "a root without a trigger is refused", env: map[string]string{EnvRoot: "/m"}, wantErr: "names both or neither"},
		{name: "a trigger without a root is refused", env: map[string]string{EnvTrigger: "merge_queue"}, wantErr: "names both or neither"},
		{name: "an empty root is refused", env: map[string]string{EnvRoot: "", EnvTrigger: "merge_queue"}, wantErr: "names both or neither"},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			t.Setenv(EnvRoot, "")
			t.Setenv(EnvTrigger, "")
			os.Unsetenv(EnvRoot)
			os.Unsetenv(EnvTrigger)
			for k, v := range tt.env {
				t.Setenv(k, v)
			}

			// Act
			got, err := TakeTarget()

			// Assert
			if tt.wantErr != "" {
				if err == nil || !strings.Contains(err.Error(), tt.wantErr) {
					t.Fatalf("TakeTarget = %v, %v; want an error saying %q", got, err, tt.wantErr)
				}
			} else if err != nil || got != tt.want {
				t.Fatalf("TakeTarget = %+v, %v; want %+v", got, err, tt.want)
			}
			_, root := os.LookupEnv(EnvRoot)
			_, trigger := os.LookupEnv(EnvTrigger)
			if root || trigger {
				t.Fatalf("the environment still names the push, so a unit would inherit it")
			}
		})
	}
}

func TestParseGoReadsEveryTestAndSubtest(t *testing.T) {
	// Arrange
	out := []byte(strings.Join([]string{
		"=== RUN   TestA",
		"=== RUN   TestA/one",
		"    --- PASS: TestA/one (0.25s)",
		"    --- SKIP: TestA/two (0.00s)",
		"--- FAIL: TestA (0.30s)",
		"--- PASS: TestB (1.5s)",
		"ok  	x	1.2s",
	}, "\n"))

	// Act
	got, err := ParseGo("daemon", "agent-repld/internal/x", out)

	// Assert
	want := []Test{
		{Suite: "daemon", Name: "agent-repld/internal/x.TestA/one", Outcome: Pass, Seconds: 0.25},
		{Suite: "daemon", Name: "agent-repld/internal/x.TestA/two", Outcome: Skip, Seconds: 0},
		{Suite: "daemon", Name: "agent-repld/internal/x.TestA", Outcome: Fail, Seconds: 0.3},
		{Suite: "daemon", Name: "agent-repld/internal/x.TestB", Outcome: Pass, Seconds: 1.5},
	}
	if err != nil || !slices.Equal(got, want) {
		t.Fatalf("ParseGo = %+v, %v; want %+v", got, err, want)
	}
}

func TestParseERTReadsEveryResultLine(t *testing.T) {
	// Arrange
	out := []byte(strings.Join([]string{
		"Running 3 tests (2026-10-23 17:00:54-0400, selector `t')",
		"   passed  1/3  agent-repl-test-a (0.000231 sec)",
		"   FAILED  2/3  agent-repl-test-b (1.500000 sec) at lisp/test-x.el:12",
		"  skipped  3/3  agent-repl-test-c (0.000010 sec)",
		"Ran 3 tests, 1 results as expected",
	}, "\n"))

	// Act
	got, err := ParseERT("elisp", out)

	// Assert
	want := []Test{
		{Suite: "elisp", Name: "agent-repl-test-a", Outcome: Pass, Seconds: 0.000231},
		{Suite: "elisp", Name: "agent-repl-test-b", Outcome: Fail, Seconds: 1.5},
		{Suite: "elisp", Name: "agent-repl-test-c", Outcome: Skip, Seconds: 0.00001},
	}
	if err != nil || !slices.Equal(got, want) {
		t.Fatalf("ParseERT = %+v, %v; want %+v", got, err, want)
	}
}

func writeTests(t *testing.T, content string) string {
	t.Helper()
	path := filepath.Join(t.TempDir(), "tests.json")
	if err := os.WriteFile(path, []byte(content), 0o644); err != nil {
		t.Fatal(err)
	}
	return path
}

func TestParseVitestNamesEachTestByItsFileAndSuites(t *testing.T) {
	// Arrange
	path := writeTests(t, `[{"file":"/pkg/test/a.test.ts","name":"suite > case","state":"pass","seconds":0.012},
		{"file":"/pkg/test/a.test.ts","name":"other","state":"todo","seconds":0}]`)

	// Act
	got, err := ParseVitest("webapp", path, "/pkg")

	// Assert
	want := []Test{
		{Suite: "webapp", Name: "test/a.test.ts > suite > case", Outcome: Pass, Seconds: 0.012},
		{Suite: "webapp", Name: "test/a.test.ts > other", Outcome: Skip, Seconds: 0},
	}
	if err != nil || !slices.Equal(got, want) {
		t.Fatalf("ParseVitest = %+v, %v; want %+v", got, err, want)
	}
}

func TestParseVitestRefusesABrokenTestsFile(t *testing.T) {
	tests := []struct {
		name    string
		content string
		missing bool
		want    string
	}{
		{name: "no file", missing: true, want: "read the chunk's tests file"},
		{name: "not JSON", content: `{`, want: "is not an items-reporter tests file"},
		{name: "an unknown state", content: `[{"file":"/p/a","name":"x","state":"run","seconds":1}]`, want: `in state "run"`},
		{name: "a negative time", content: `[{"file":"/p/a","name":"x","state":"pass","seconds":-1}]`, want: "taking -1 seconds"},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			path := filepath.Join(t.TempDir(), "absent.json")
			if !tt.missing {
				path = writeTests(t, tt.content)
			}

			// Act
			_, err := ParseVitest("webapp", path, "/p")

			// Assert
			if err == nil || !strings.Contains(err.Error(), tt.want) {
				t.Fatalf("ParseVitest = %v, want an error saying %q", err, tt.want)
			}
		})
	}
}

func TestOfItemsGivesEachItemTheUnitsOutcomeInNameOrder(t *testing.T) {
	// Act
	got := OfItems("harness", map[string]float64{"b": 2, "a": 1}, Fail)

	// Assert
	want := []Test{{Suite: "harness", Name: "a", Outcome: Fail, Seconds: 1}, {Suite: "harness", Name: "b", Outcome: Fail, Seconds: 2}}
	if !slices.Equal(got, want) {
		t.Fatalf("OfItems = %+v, want %+v", got, want)
	}
}

func TestEncodeStampsEverySampleWithTheTriggerAndTheRunsEnd(t *testing.T) {
	// Arrange
	at := time.UnixMilli(1_700_000_000_123)

	// Act
	got := string(Encode(
		[]Test{{Suite: "webapp", Name: `a > says "hi"\n`, Outcome: Pass, Seconds: 0.5}},
		[]Suite{{Name: "webapp", Outcome: Pass, Seconds: 12}},
		"merge_queue", at))

	// Assert
	want := `agentrepl_test_seconds{suite="webapp",test="a > says \"hi\"\\n",outcome="pass",trigger="merge_queue"} 0.5 1700000000123` + "\n" +
		`agentrepl_test_suite_seconds{suite="webapp",outcome="pass",trigger="merge_queue"} 12 1700000000123` + "\n"
	if got != want {
		t.Fatalf("Encode =\n%s\nwant\n%s", got, want)
	}
}

func TestPushSendsTheEncodedTimesToTheStore(t *testing.T) {
	// Arrange
	var body, labels string
	srv := httptest.NewServer(http.HandlerFunc(func(w http.ResponseWriter, r *http.Request) {
		raw, _ := io.ReadAll(r.Body)
		body, labels = string(raw), strings.Join(r.URL.Query()["extra_label"], ",")
		w.WriteHeader(http.StatusNoContent)
	}))
	t.Cleanup(srv.Close)
	root := t.TempDir()
	if err := os.WriteFile(filepath.Join(root, "store.json"), []byte(`{"import_url":"`+srv.URL+`/api/v1/import/prometheus","job":"agent-repl"}`), 0o644); err != nil {
		t.Fatal(err)
	}
	tests := []Test{{Suite: "elisp", Name: "t", Outcome: Pass, Seconds: 1}}

	// Act
	err := Push(context.Background(), srv.Client(), Target{Root: root, Trigger: "merge_queue"}, tests, nil, time.UnixMilli(5))

	// Assert
	if err != nil || body != string(Encode(tests, nil, "merge_queue", time.UnixMilli(5))) || labels != "job=agent-repl,instance=testrun" {
		t.Fatalf("Push = %v; store got %q labeled %q", err, body, labels)
	}
}

func TestPushAnswersAStoreThatCannotBeReached(t *testing.T) {
	// Arrange
	root := t.TempDir()

	// Act
	err := Push(context.Background(), http.DefaultClient, Target{Root: root, Trigger: "merge_queue"}, []Test{{Name: "t"}}, nil, time.Now())

	// Assert
	if err == nil || !strings.Contains(err.Error(), "1 test and 0 suite times were not pushed") {
		t.Fatalf("Push = %v, want the unpushed times named", err)
	}
}
