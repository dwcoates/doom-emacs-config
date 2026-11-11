package reuse

import (
	"errors"
	"os"
	"path/filepath"
	"slices"
	"strings"
	"testing"

	"agentrepl/testrun/roster"
)

// keyedSubject is the fixture's one keyed subject: the file prompts/p.md.
func keyedSubject(t *testing.T, f fixture) (*InputSet, Facts) {
	t.Helper()
	set := NewInputSet()
	if err := set.AddTree(fixtureModule + "/prompts"); err != nil {
		t.Fatal(err)
	}
	return set, Facts{Suite: "subject", Toolchain: map[string]string{"go": "go1.24"}}
}

func TestDecideKeyed(t *testing.T) {
	tests := []struct {
		name    string
		mode    Mode
		before  func(t *testing.T, f fixture, set *InputSet, facts Facts)
		reused  bool
		wantWhy string
	}{
		{"no record", Reusing, nil, false, "no record"},
		{"a record of the same key", Reusing, recordSubject, true, ""},
		{"a record of an earlier key", Reusing, func(t *testing.T, f fixture, set *InputSet, facts Facts) {
			recordSubject(t, f, set, facts)
			write(t, f.d.Repo, fixtureModule+"/prompts/p.md", "changed")
		}, false, "changed since its newest green record: " + fixtureModule + "/prompts/p.md"},
		{"--no-reuse", NoReuse, recordSubject, false, "--no-reuse"},
		{"a targeted run", Targeted, recordSubject, false, "a targeted run neither reuses nor records"},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			f := newFixture(t)
			set, facts := keyedSubject(t, f)
			if tt.before != nil {
				tt.before(t, f, set, facts)
			}

			// Act
			dec, err := decideKeyed(f.d, "subject", set, facts, tt.mode)

			// Assert
			if err != nil {
				t.Fatal(err)
			}
			if (dec.Reused != nil) != tt.reused || dec.Why != tt.wantWhy {
				t.Fatalf("decision = reused %v why %q, want reused %v why %q", dec.Reused != nil, dec.Why, tt.reused, tt.wantWhy)
			}
			if dec.Profile != filepath.Join(f.d.ProfileDir, "subject.sb") {
				t.Fatalf("profile = %s, want the subject's own file in the profile directory", dec.Profile)
			}
			if _, err := os.Stat(dec.Profile); err != nil {
				t.Fatalf("the profile was not written: %v", err)
			}
		})
	}
}

// recordSubject records a pass of the keyed subject on its current inputs.
func recordSubject(t *testing.T, f fixture, set *InputSet, facts Facts) {
	t.Helper()
	dec, err := decideKeyed(f.d, "subject", set, facts, NoReuse)
	if err != nil {
		t.Fatal(err)
	}
	r := recorder{d: f.d}
	if ok, err := r.record("subject", "the subject", set, facts, dec.Key, 1); err != nil || !ok {
		t.Fatalf("record = %v, %v", ok, err)
	}
}

func TestDecideKeyedFails(t *testing.T) {
	tests := []struct {
		name  string
		setup func(t *testing.T, f *fixture, set *InputSet)
		subj  string
		want  string
	}{
		{"an input that is not there", func(t *testing.T, f *fixture, set *InputSet) {
			if err := set.AddFile("missing.md"); err != nil {
				t.Fatal(err)
			}
		}, "subject", "reuse: key subject: "},
		{"a profile directory that cannot be written", func(t *testing.T, f *fixture, _ *InputSet) {
			f.d.ProfileDir = filepath.Join(f.d.ProfileDir, "absent")
		}, "subject", "reuse: write subject's profile: "},
		{"a name the store refuses", func(*testing.T, *fixture, *InputSet) {}, "not/a name", "is not a suite name"},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			f := newFixture(t)
			set, facts := keyedSubject(t, f)
			tt.setup(t, &f, set)
			if strings.Contains(tt.subj, "/") {
				// The profile file must still be writable for the store's
				// refusal to be reached.
				if err := os.MkdirAll(filepath.Join(f.d.ProfileDir, "not"), 0o755); err != nil {
					t.Fatal(err)
				}
			}

			// Act
			_, err := decideKeyed(f.d, tt.subj, set, facts, Reusing)

			// Assert
			if err == nil || !strings.Contains(err.Error(), tt.want) {
				t.Fatalf("decideKeyed = %v, want %q", err, tt.want)
			}
		})
	}
}

func TestTheRecorderReadsHeadOnceForEveryRecord(t *testing.T) {
	// Arrange
	f := newFixture(t)
	set, facts := keyedSubject(t, f)
	heads := 0
	head := f.d.Head
	f.d.Head = func() (string, string, bool, error) { heads++; return head() }
	dec, err := decideKeyed(f.d, "subject", set, facts, NoReuse)
	if err != nil {
		t.Fatal(err)
	}
	r := recorder{d: f.d}

	// Act
	for _, name := range []string{"subject", "subject@a", "subject@b"} {
		if ok, err := r.record(name, name, set, facts, dec.Key, 2); err != nil || !ok {
			t.Fatalf("record(%s) = %v, %v", name, ok, err)
		}
	}

	// Assert
	if heads != 1 {
		t.Fatalf("HEAD read %d times, want once", heads)
	}
	rec, ok, err := f.d.Store.Lookup("subject@b", dec.Key.Digest)
	if err != nil || !ok || rec.Branch != "feature" || rec.Commit != "c0ffee" || rec.Units != 2 {
		t.Fatalf("record = %+v, %v, %v; want feature c0ffee with 2 units", rec, ok, err)
	}
}

func TestTheRecorderRecordsNothingForInputsThatMoved(t *testing.T) {
	// Arrange
	f := newFixture(t)
	set, facts := keyedSubject(t, f)
	dec, err := decideKeyed(f.d, "subject", set, facts, NoReuse)
	if err != nil {
		t.Fatal(err)
	}
	write(t, f.d.Repo, fixtureModule+"/prompts/p.md", "edited mid-run")
	r := recorder{d: f.d}

	// Act
	ok, err := r.record("subject", "the subject", set, facts, dec.Key, 1)

	// Assert
	if err != nil || ok {
		t.Fatalf("record = %v, %v; want nothing recorded and no error", ok, err)
	}
	if want := "the subject: not recorded, its inputs changed while it ran: " + fixtureModule + "/prompts/p.md"; !slices.Contains(f.log.errs, want) {
		t.Fatalf("errors = %q, want %q", f.log.errs, want)
	}
	if _, found, _ := f.d.Store.Newest("subject"); found {
		t.Fatal("a record was stored for inputs that moved")
	}
}

func TestTheRecorderFails(t *testing.T) {
	tests := []struct {
		name  string
		setup func(t *testing.T, f *fixture)
		want  string
	}{
		{"an unreadable HEAD", func(t *testing.T, f *fixture) {
			f.d.Head = func() (string, string, bool, error) { return "", "", false, errors.New("no git") }
		}, "reuse: read the checkout's HEAD: no git"},
		{"a store that cannot be written", func(t *testing.T, f *fixture) {
			f.d.Store.Dir = filepath.Join(f.d.Repo, fixtureModule, "prompts", "p.md")
		}, "reuse: create "},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			f := newFixture(t)
			set, facts := keyedSubject(t, f)
			dec, err := decideKeyed(f.d, "subject", set, facts, NoReuse)
			if err != nil {
				t.Fatal(err)
			}
			tt.setup(t, &f)
			r := recorder{d: f.d}

			// Act
			ok, err := r.record("subject", "the subject", set, facts, dec.Key, 1)

			// Assert
			if ok || err == nil || !strings.Contains(err.Error(), tt.want) {
				t.Fatalf("record = %v, %v; want %q", ok, err, tt.want)
			}
		})
	}
}

// A suite and its packages are decided and recorded through the one shape
// (decideKeyed, recorder): a site that keyed, looked up or recorded on its own
// would answer differently here.
func TestSuitesAndPackagesAreDecidedAndRecordedAlike(t *testing.T) {
	// Arrange
	f := newPkgFixture(t)
	f.suite.Reuse = &roster.Reuse{Inputs: []string{"prompts/"}}

	// Act
	suite := f.decide(t, Reusing, f.suite)[0]
	pv := f.decidePackages(t, Reusing)
	f.pass(t, Reusing, []Verdict{suite})
	f.passAll(t, pv)
	write(t, f.root, fixtureModule+"/prompts/p.md", "changed")
	suiteAfter := f.decide(t, Reusing, f.suite)[0]
	var cAfter PackageVerdict
	for _, v := range f.decidePackages(t, Reusing) {
		if v.Package == "c" {
			cAfter = v
		}
	}

	// Assert
	if suite.Why != "no record" || pv[2].Why != "no record" {
		t.Fatalf("first reasons %q and %q, want both no record", suite.Why, pv[2].Why)
	}
	if filepath.Dir(suite.Profile) != filepath.Dir(pv[2].Profile) || filepath.Base(pv[2].Profile) != pv[2].Name()+".sb" {
		t.Fatalf("profiles %s and %s, want both <name>.sb in the profile directory", suite.Profile, pv[2].Profile)
	}
	want := "changed since its newest green record: " + fixtureModule + "/prompts/p.md"
	if suiteAfter.Why != want || cAfter.Why != want {
		t.Fatalf("reasons after the change %q and %q, want both %q", suiteAfter.Why, cAfter.Why, want)
	}
	s, _, _ := f.d.Store.Newest("daemon")
	p, _, _ := f.d.Store.Newest(pv[2].Name())
	if s.Branch != p.Branch || s.Commit != p.Commit || s.Checkout != p.Checkout || !s.PassedAt.Equal(p.PassedAt) {
		t.Fatalf("records %+v and %+v name different runs", s, p)
	}
}
