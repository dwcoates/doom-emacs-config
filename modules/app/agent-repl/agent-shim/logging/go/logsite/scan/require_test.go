package scan

import (
	"fmt"
	"reflect"
	"strings"
	"testing"
)

func TestCallsAnswersEachSelectorsShapeAndMsg(t *testing.T) {
	// Act
	got := Calls(3, 1, 2, "Info", "Error")

	// Assert
	want := []Call{
		{Selector: "Info", Args: 3, Template: 1, Context: 2},
		{Selector: "Error", Args: 3, Template: 1, Context: 2},
		MsgCall,
	}
	if !reflect.DeepEqual(got, want) {
		t.Fatalf("Calls = %+v, want %+v", got, want)
	}
}

// fakeTB records what Require reports.
type fakeTB struct {
	fatals, errors []string
}

func (f *fakeTB) Helper() {}

func (f *fakeTB) Fatalf(format string, args ...any) {
	f.fatals = append(f.fatals, fmt.Sprintf(format, args...))
}

func (f *fakeTB) Errorf(format string, args ...any) {
	f.errors = append(f.errors, fmt.Sprintf(format, args...))
}

func TestRequireFailsOnEveryKindOfFault(t *testing.T) {
	tests := []struct {
		name       string
		spec       Spec
		wantFatal  string
		wantErrors int
	}{
		{
			name:       "every fault Check finds is reported",
			spec:       Spec{Runtime: "daemon", Module: "example.com/mod", Root: "testdata/mod", Calls: Calls(3, 1, 2, "Error")},
			wantErrors: len(Check(fixtureSites(t))),
		},
		{
			name:      "a scan that cannot run",
			spec:      Spec{Runtime: "daemon", Module: "example.com/mod", Root: "testdata/absent", Calls: Calls(3, 1, 2, "Error")},
			wantFatal: "scan the example.com/mod module's source",
		},
		{
			name:      "a scan that finds no log call",
			spec:      Spec{Runtime: "daemon", Module: "example.com/mod", Root: "testdata/mod", Calls: Calls(3, 1, 2, "Nothing")},
			wantFatal: "found no log call",
		},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			tb := &fakeTB{}

			// Act
			Require(tb, tt.spec)

			// Assert
			if tt.wantFatal != "" {
				if len(tb.fatals) != 1 || !strings.Contains(tb.fatals[0], tt.wantFatal) || len(tb.errors) != 0 {
					t.Fatalf("fatals %v, errors %v; want one fatal naming %q", tb.fatals, tb.errors, tt.wantFatal)
				}
				return
			}
			if len(tb.fatals) != 0 || len(tb.errors) != tt.wantErrors || tt.wantErrors == 0 {
				t.Fatalf("fatals %v, errors %v; want %d faults reported", tb.fatals, tb.errors, tt.wantErrors)
			}
		})
	}
}
