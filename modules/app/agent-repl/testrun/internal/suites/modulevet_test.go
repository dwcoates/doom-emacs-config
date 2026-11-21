package suites

import (
	"reflect"
	"testing"

	"agentrepl/testrun/internal/govet"
	"agentrepl/testrun/internal/run"
	"agentrepl/testrun/roster"
)

func TestModuleVetID(t *testing.T) {
	tests := []struct {
		name, tags, want string
	}{
		{"the ordinary pass", "", "m:vet"},
		{"a tagged pass", "integration", "m:vet[integration]"},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Act
			got := ModuleVetID("m", tt.tags)

			// Assert
			if got != tt.want {
				t.Fatalf("ModuleVetID = %q, want %q", got, tt.want)
			}
		})
	}
}

func TestGoVetSpecVetsTheNamedPackages(t *testing.T) {
	// Arrange
	v := moduleVet(Layout{Work: "/w", Self: "/testrun"}, "m", "/mod", "integration")

	// Act
	sp := v.Spec([]string{"a", "b"})

	// Assert
	want := []string{"/testrun", "vet-module", "-module", "/mod", "-resultdir", "/w/vet/m-integration", "-tags", "integration", "--", "a", "b"}
	if sp.ID != "m:vet[integration]" || sp.Suite != "m" || sp.Dir != "/mod" || !reflect.DeepEqual(sp.Argv, want) {
		t.Fatalf("Spec = %+v, want argv %v", sp, want)
	}
}

func TestGoVetCheckWaitsForTheVetAndReadsItsPackagesVerdict(t *testing.T) {
	// Arrange
	v := moduleVet(Layout{Work: "/w", Self: "/testrun"}, "m", "/mod", "")

	// Act
	sp := v.check("m:a/b", "a/b")

	// Assert
	want := []string{"/testrun", "vet-check", govet.ResultPath("/w/vet/m", "a/b")}
	if sp.ID != "m:a/b:vet" || !reflect.DeepEqual(sp.Argv, want) || !reflect.DeepEqual(sp.Deps, []string{"m:vet"}) {
		t.Fatalf("check = %+v, want argv %v after m:vet", sp, want)
	}
	if v.Needs["m:a/b:vet"] != "a/b" {
		t.Fatalf("Needs = %v, want the check to need a/b", v.Needs)
	}
}

func TestGoVetCheckRefusesOneCheckForTwoPackages(t *testing.T) {
	// Arrange
	v := moduleVet(Layout{Work: "/w"}, "m", "/mod", "")
	v.check("m:a", "a")

	// Act / Assert
	defer func() {
		if recover() == nil {
			t.Fatal("one check ID for two packages did not panic")
		}
	}()
	v.check("m:a", "b")
}

func TestAModulePassVetsEachPackageByItsOwnCheck(t *testing.T) {
	// Arrange: p has tests; q, untested, is only vetted.
	module := goPackageFixture(t, "func TestA(t *testing.T) {}\n")
	l := Layout{Module: module, Work: t.TempDir(), Self: "/testrun"}

	// Act
	u, err := goModuleUnitsForPackages(l, roster.Suite{Name: "m"}, module, []string{"p"}, "q")

	// Assert
	if err != nil {
		t.Fatal(err)
	}
	byID := map[string]run.Spec{}
	for _, a := range u.Atomic {
		byID[a.ID] = a
	}
	for _, id := range []string{"m:p:vet", "m:q:vet"} {
		if _, ok := byID[id]; !ok {
			t.Errorf("no vet check %s among %v", id, keys(byID))
		}
	}
	want := moduleVet(l, "m", module, "").Spec([]string{"p", "q"})
	if got := byID["m:vet"]; !reflect.DeepEqual(got.Argv, want.Argv) {
		t.Fatalf("module vet = %v, want %v", got.Argv, want.Argv)
	}
}

func TestFinishVetsOnlyThePackagesWhoseChecksRemain(t *testing.T) {
	tests := []struct {
		name   string
		checks []string
		want   [][]string
	}{
		{"every check remains", []string{"m:a:vet", "m:b:vet"}, [][]string{{"a", "b"}}},
		{"one check was reused", []string{"m:b:vet"}, [][]string{{"b"}}},
		{"no check remains, so no vet is placed", nil, nil},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			v := moduleVet(Layout{Work: "/w", Self: "/testrun"}, "m", "/mod", "")
			v.check("m:a", "a")
			v.check("m:b", "b")
			u := Units{GoVets: []GoVet{v}, Atomic: []run.Spec{v.Spec([]string{"a", "b"})}}
			for _, id := range tt.checks {
				u.Atomic = append(u.Atomic, unit(id))
			}

			// Act
			got := u.Finish()

			// Assert
			var vets [][]string
			for _, a := range got.Atomic {
				if a.ID == v.ID {
					o, err := govet.ParseArgs(a.Argv[2:])
					if err != nil {
						t.Fatal(err)
					}
					vets = append(vets, o.Packages)
				}
			}
			if !reflect.DeepEqual(vets, tt.want) {
				t.Fatalf("vets = %v, want %v", vets, tt.want)
			}
		})
	}
}

func keys(m map[string]run.Spec) []string {
	var out []string
	for k := range m {
		out = append(out, k)
	}
	return out
}
