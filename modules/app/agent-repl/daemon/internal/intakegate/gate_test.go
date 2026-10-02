package intakegate

import (
	"reflect"
	"testing"

	"claude-repld/internal/dlog"
)

func TestAdmitsAnswersWhetherThisDaemonServes(t *testing.T) {
	tests := []struct {
		name   string
		serves bool
	}{
		{name: "a serving daemon takes the intake", serves: true},
		{name: "a daemon that does not serve leaves it", serves: false},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange
			g := New(func() bool { return tc.serves }, dlog.NewTestLogger(), "daemon.test.gate")

			// Act
			got := g.Admits()

			// Assert
			if got != tc.serves {
				t.Fatalf("Admits = %v, want %v", got, tc.serves)
			}
		})
	}
}

func TestAdmitsRecordsTheFirstAnswerAndEveryChangeOnce(t *testing.T) {
	tests := []struct {
		name    string
		answers []bool
		want    []string
	}{
		{name: "an unchanged answer is recorded once", answers: []bool{true, true, true}, want: []string{"serves"}},
		{name: "a stop is recorded", answers: []bool{true, false, false}, want: []string{"serves", "leaves"}},
		{name: "a stop and a resumption are both recorded", answers: []bool{false, true, false}, want: []string{"leaves", "serves", "leaves"}},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange
			log := dlog.NewTestLogger()
			i := 0
			g := New(func() bool { return tc.answers[i] }, log, "daemon.test.gate")

			// Act
			for i = range tc.answers {
				g.Admits()
			}

			// Assert
			var got []string
			for _, r := range log.Records() {
				if r.Level != "info" || r.Operation != "daemon.test.gate" {
					t.Fatalf("record %+v, want every record at INFO under the gate's operation", r)
				}
				if r.Message == "this daemon serves, so it takes the intake" {
					got = append(got, "serves")
				} else {
					got = append(got, "leaves")
				}
			}
			if !reflect.DeepEqual(got, tc.want) {
				t.Fatalf("recorded = %v, want %v", got, tc.want)
			}
		})
	}
}

func TestNewForNamesItsDutyInTheRecords(t *testing.T) {
	tests := []struct {
		name   string
		serves bool
		want   string
	}{
		{name: "serving", serves: true, want: "this daemon serves, so it takes the digest"},
		{name: "not serving", serves: false, want: "this daemon does not serve (a successor still joining, or a handover in flight), so it leaves the digest for the daemon that does"},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange
			log := dlog.NewTestLogger()
			g := NewFor(func() bool { return tc.serves }, log, "daemon.test.gate", "the digest")

			// Act
			g.Admits()

			// Assert
			records := log.Records()
			if len(records) != 1 || records[0].Message != tc.want {
				t.Fatalf("records = %+v, want one saying %q", records, tc.want)
			}
		})
	}
}
