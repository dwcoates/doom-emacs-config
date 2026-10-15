package cli

import (
	"errors"
	"reflect"
	"testing"

	"agentrepl/testrun/internal/suites"
	"agentrepl/testrun/roster"
)

func TestProgramUnitsAreReportedUnderTheirOwnersOrTheFirstConsumer(t *testing.T) {
	tests := []struct {
		name    string
		running []string
		want    map[string]string
	}{
		{name: "an owner in the run takes its program", running: []string{"daemon", "store", "e2e"},
			want: map[string]string{"program:shim-store": "store", "program:agent-repld": "daemon"}},
		{name: "an owner outside the run hands it to the first consumer", running: []string{"e2e"},
			want: map[string]string{"program:shim-store": "e2e", "program:agent-repld": "e2e"}},
		{name: "an owner the run reuses hands it to the first consumer", running: []string{"store", "e2e"},
			want: map[string]string{"program:shim-store": "store", "program:agent-repld": "e2e"}},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange: e2e runs both programs; store runs its own; daemon runs
			// none in this fixture, so only its ownership is at stake.
			units := map[string]suites.Units{
				"daemon": {},
				"store":  {Programs: []string{"shim-store"}},
				"e2e":    {Programs: []string{"agent-repld", "shim-store"}},
			}
			var selected []roster.Suite
			var built []suites.Units
			for _, name := range tt.running {
				selected = append(selected, roster.Suite{Name: name})
				built = append(built, units[name])
			}

			// Act.
			u, err := programUnits(suites.Layout{Module: "/m", Work: "/w"}, selected, built, make([]error, len(selected)))

			// Assert.
			if err != nil {
				t.Fatal(err)
			}
			got := map[string]string{}
			for _, s := range u.Atomic {
				got[s.ID] = s.Suite
			}
			if !reflect.DeepEqual(got, tt.want) {
				t.Fatalf("owners = %v, want %v", got, tt.want)
			}
		})
	}
}

func TestProgramUnitsSkipASuiteThatCouldNotBePlanned(t *testing.T) {
	// Arrange.
	selected := []roster.Suite{{Name: "store"}}
	built := []suites.Units{{Programs: []string{"shim-store"}}}

	// Act.
	u, err := programUnits(suites.Layout{}, selected, built, []error{errors.New("no plan")})

	// Assert.
	if err != nil || len(u.Atomic) != 0 {
		t.Fatalf("units = %v, %v; want none for an unplanned suite", u.Atomic, err)
	}
}
