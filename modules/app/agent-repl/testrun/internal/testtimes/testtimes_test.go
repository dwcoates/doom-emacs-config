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

func TestParseGoGivesEachTestItsLatestStart(t *testing.T) {
	// Arrange: TestA/one ran, paused, and its clock restarted at its CONT.
	run, cont, b := at(1), at(2.5), at(3)
	out := []byte(strings.Join([]string{
		StartLine(at(0), "TestA"),
		"=== RUN   TestA",
		StartLine(run, "TestA/one"),
		"=== RUN   TestA/one",
		"=== PAUSE TestA/one",
		StartLine(cont, "TestA/one"),
		"=== CONT  TestA/one",
		"    --- PASS: TestA/one (0.25s)",
		"--- PASS: TestA (0.30s)",
		StartLine(b, "TestB"),
		"=== RUN   TestB",
		"--- PASS: TestB (1.5s)",
	}, "\n"))

	// Act
	got, err := ParseGo("daemon", "x", out)

	// Assert
	want := map[string]time.Time{"x.TestA/one": cont, "x.TestA": at(0), "x.TestB": b}
	if err != nil || len(got) != len(want) {
		t.Fatalf("ParseGo = %+v, %v", got, err)
	}
	for _, tt := range got {
		if !tt.Start.Equal(want[tt.Name]) {
			t.Fatalf("%s started %v; want %v", tt.Name, tt.Start, want[tt.Name])
		}
	}
}

func TestParseGoRefusesAnUnreadableStart(t *testing.T) {
	// Arrange
	out := []byte(StartMarker + " 99999999999999999999.5 TestA\n--- PASS: TestA (0.01s)\n")

	// Act
	_, err := ParseGo("daemon", "x", out)

	// Assert
	if err == nil || !strings.Contains(err.Error(), "unreadable start") {
		t.Fatalf("ParseGo = %v; want the unreadable start named", err)
	}
}

func TestStartLineRoundTripsItsInstant(t *testing.T) {
	// Arrange
	want := time.Unix(1_800_000_000, 123_456_789)

	// Act
	name, got, ok, err := startOf(StartLine(want, "TestX/y"))

	// Assert
	if err != nil || !ok || name != "TestX/y" || !got.Equal(want) {
		t.Fatalf("startOf(StartLine) = %q, %v, %v, %v; want TestX/y at %v", name, got, ok, err, want)
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

func TestParseERTGivesEachTestTheStartTheDriverWrote(t *testing.T) {
	// Arrange: the driver writes six decimals.
	out := []byte(strings.Join([]string{
		StartMarker + " 1800000000.250000 agent-repl-test-a",
		"   passed  1/1  agent-repl-test-a (0.000231 sec)",
	}, "\n"))

	// Act
	got, err := ParseERT("elisp", out)

	// Assert
	if err != nil || len(got) != 1 || !got[0].Start.Equal(at(0.25)) {
		t.Fatalf("ParseERT = %+v, %v; want agent-repl-test-a started at %v", got, err, at(0.25))
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

func TestParseVitestGivesEachRunTestItsStart(t *testing.T) {
	// Arrange
	path := writeTests(t, `[{"file":"/pkg/a.test.ts","name":"ran","state":"pass","seconds":0.5,"start":1800000000.5},
		{"file":"/pkg/a.test.ts","name":"skipped","state":"skip","seconds":0}]`)

	// Act
	got, err := ParseVitest("webapp", path, "/pkg")

	// Assert
	if err != nil || len(got) != 2 {
		t.Fatalf("ParseVitest = %+v, %v", got, err)
	}
	if !got[0].Start.Equal(at(0.5)) || !got[1].Start.IsZero() {
		t.Fatalf("starts = %v, %v; want %v and none", got[0].Start, got[1].Start, at(0.5))
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
	got := string(Encode(Report{
		Tests: []Test{
			{Suite: "webapp", Name: `a > says "hi"\n`, Outcome: Pass, Seconds: 0.5},
			{Suite: "webapp", Name: "b", Outcome: Pass, Seconds: 2, CPU: 0.75, HasCPU: true},
		},
		Suites:     []Suite{{Name: "webapp", Outcome: Pass, Seconds: 12}},
		Units:      []Unit{{Suite: "webapp", ID: "webapp#00", Outcome: Fail, Wall: 3, CPU: 2.5, MaxRSSBytes: 1048576, BlockReads: 7, BlockWrites: 9}},
		Run:        &Run{Seconds: 95.5, PredictedSeconds: 80, Outcome: Fail},
		SuiteUnits: []SuiteUnits{{Suite: "webapp", Run: 15, Reused: 2}},
		Batching:   "max48-min12",
	}, "merge_queue", at))

	// Assert
	unit := `{suite="webapp",unit="webapp#00",outcome="fail",trigger="merge_queue"}`
	run := `{outcome="fail",trigger="merge_queue",batching="max48-min12"}`
	want := `agentrepl_test_seconds{suite="webapp",test="a > says \"hi\"\\n",outcome="pass",trigger="merge_queue"} 0.5 1700000000123` + "\n" +
		`agentrepl_test_seconds{suite="webapp",test="b",outcome="pass",trigger="merge_queue"} 2 1700000000123` + "\n" +
		`agentrepl_test_cpu_seconds{suite="webapp",test="b",outcome="pass",trigger="merge_queue"} 0.75 1700000000123` + "\n" +
		`agentrepl_test_suite_seconds{suite="webapp",outcome="pass",trigger="merge_queue",batching="max48-min12"} 12 1700000000123` + "\n" +
		`agentrepl_test_run_seconds` + run + ` 95.5 1700000000123` + "\n" +
		`agentrepl_test_run_predicted_seconds` + run + ` 80 1700000000123` + "\n" +
		`agentrepl_test_suite_units{suite="webapp",state="run",trigger="merge_queue",batching="max48-min12"} 15 1700000000123` + "\n" +
		`agentrepl_test_suite_units{suite="webapp",state="reused",trigger="merge_queue",batching="max48-min12"} 2 1700000000123` + "\n" +
		`agentrepl_test_unit_wall_seconds` + unit + ` 3 1700000000123` + "\n" +
		`agentrepl_test_unit_cpu_seconds` + unit + ` 2.5 1700000000123` + "\n" +
		`agentrepl_test_unit_max_rss_bytes` + unit + ` 1048576 1700000000123` + "\n" +
		`agentrepl_test_unit_block_reads` + unit + ` 7 1700000000123` + "\n" +
		`agentrepl_test_unit_block_writes` + unit + ` 9 1700000000123` + "\n"
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
	report := Report{Tests: []Test{{Suite: "elisp", Name: "t", Outcome: Pass, Seconds: 1}}}

	// Act
	err := Push(context.Background(), srv.Client(), Target{Root: root, Trigger: "merge_queue"}, report, time.UnixMilli(5))

	// Assert
	if err != nil || body != string(Encode(report, "merge_queue", time.UnixMilli(5))) || labels != "job=agent-repl,instance=testrun" {
		t.Fatalf("Push = %v; store got %q labeled %q", err, body, labels)
	}
}

func TestPushAnswersAStoreThatCannotBeReached(t *testing.T) {
	// Arrange
	root := t.TempDir()

	// Act
	err := Push(context.Background(), http.DefaultClient, Target{Root: root, Trigger: "merge_queue"}, Report{Tests: []Test{{Name: "t"}}}, time.Now())

	// Assert
	if err == nil || !strings.Contains(err.Error(), "1 test, 0 suite and 0 unit records were not pushed") {
		t.Fatalf("Push = %v, want the unpushed times named", err)
	}
}
