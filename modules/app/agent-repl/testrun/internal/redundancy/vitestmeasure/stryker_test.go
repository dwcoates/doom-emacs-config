package vitestmeasure

import (
	"reflect"
	"strings"
	"testing"
)

const report = `{
  "schemaVersion": "2",
  "files": {
    "src/a.ts": {"mutants": [
      {"id": "1", "mutatorName": "EqualityOperator", "replacement": "<=", "status": "Killed", "killedBy": ["t2", "t1"], "coveredBy": ["t1", "t2"], "location": {"start": {"line": 3, "column": 9}}},
      {"id": "2", "mutatorName": "BooleanLiteral", "replacement": "false", "status": "Survived", "coveredBy": ["t1"], "location": {"start": {"line": 4, "column": 1}}},
      {"id": "3", "mutatorName": "BlockStatement", "replacement": "{}", "status": "NoCoverage", "location": {"start": {"line": 5, "column": 1}}},
      {"id": "4", "mutatorName": "StringLiteral", "replacement": "\"\"", "status": "CompileError", "location": {"start": {"line": 6, "column": 1}}},
      {"id": "5", "mutatorName": "UpdateOperator", "replacement": "i--", "status": "Timeout", "coveredBy": ["t2"], "location": {"start": {"line": 7, "column": 1}}}
    ]}
  },
  "testFiles": {
    "test/a.test.ts": {"tests": [{"id": "t1", "name": "a > adds"}, {"id": "t2", "name": "a > compares"}]}
  }
}`

func TestParseStryker(t *testing.T) {
	// Act
	got, err := ParseStryker([]byte(report))

	// Assert
	if err != nil {
		t.Fatal(err)
	}
	if !reflect.DeepEqual(got.Cases, []string{"test/a.test.ts:a > adds", "test/a.test.ts:a > compares"}) {
		t.Fatalf("cases = %v", got.Cases)
	}
	status := map[string]string{}
	for _, m := range got.Mutants {
		status[m.Key] = m.Status
	}
	want := map[string]string{
		"src/a.ts:3:9:EqualityOperator:<=":  "killed",
		"src/a.ts:4:1:BooleanLiteral:false": "survived",
		"src/a.ts:5:1:BlockStatement:{}":    "unreached",
		`src/a.ts:6:1:StringLiteral:""`:     "dropped",
		"src/a.ts:7:1:UpdateOperator:i--":   "unattributed",
	}
	if !reflect.DeepEqual(status, want) {
		t.Fatalf("statuses = %v", status)
	}
	if !reflect.DeepEqual(got.Mutants[0].KilledBy, []string{"test/a.test.ts:a > adds", "test/a.test.ts:a > compares"}) {
		t.Fatalf("killed by %v", got.Mutants[0].KilledBy)
	}
	if !reflect.DeepEqual(got.Unattributed, []string{"src/a.ts:7:1:UpdateOperator:i--"}) {
		t.Fatalf("unattributed %v", got.Unattributed)
	}
}

func TestParseStrykerRefuses(t *testing.T) {
	tests := []struct {
		name, body, want string
	}{
		{name: "not JSON", body: `{`, want: "decode the Stryker report"},
		{name: "no file", body: `{"files": {}}`, want: "names no mutated file"},
		{name: "an unknown status", body: `{"files": {"a.ts": {"mutants": [{"id": "1", "status": "Pending"}]}}}`, want: `has status "Pending"`},
		{name: "a kill with no named killer", body: `{"files": {"a.ts": {"mutants": [{"id": "1", "status": "Killed"}]}}}`, want: "coverageAnalysis perTest"},
		{name: "a test id no test file holds", body: `{"files": {"a.ts": {"mutants": [{"id": "1", "status": "Killed", "killedBy": ["x"]}]}}}`, want: "test id x"},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Act
			_, err := ParseStryker([]byte(tt.body))

			// Assert
			if err == nil || !strings.Contains(err.Error(), tt.want) {
				t.Fatalf("err = %v, want %q", err, tt.want)
			}
		})
	}
}
