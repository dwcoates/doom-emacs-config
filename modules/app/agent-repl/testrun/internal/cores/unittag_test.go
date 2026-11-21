package cores

import "testing"

func TestUnitTag(t *testing.T) {
	cases := []struct {
		name string
		got  string
		want string
	}{
		{name: "a unit's tag is its run's pid and its id", got: UnitTag(42, "daemon:integration#00"), want: "42/daemon:integration#00"},
		{name: "a run's tag prefix is its tag with no id", got: UnitTag(42, ""), want: "42/"},
		{name: "a unit's environment entry sets UnitEnv to its tag", got: UnitEnvEntry(42, "e2e#01"), want: UnitEnv + "=42/e2e#01"},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			if tc.got != tc.want {
				t.Fatalf("got %q, want %q", tc.got, tc.want)
			}
		})
	}
}
