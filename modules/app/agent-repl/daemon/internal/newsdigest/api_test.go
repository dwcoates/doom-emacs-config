package newsdigest

import (
	"testing"
)

func TestNewRefusesAMissingCollaboratorOrWindow(t *testing.T) {
	tests := []struct {
		name   string
		mutate func(*Deps)
	}{
		{name: "no sources", mutate: func(d *Deps) { d.Sources = nil }},
		{name: "no fetcher", mutate: func(d *Deps) { d.Fetcher = nil }},
		{name: "no headless runner", mutate: func(d *Deps) { d.Headless = nil }},
		{name: "no prompts dir", mutate: func(d *Deps) { d.PromptsDir = "" }},
		{name: "no store", mutate: func(d *Deps) { d.Store = nil }},
		{name: "no clock", mutate: func(d *Deps) { d.Clock = nil }},
		{name: "no lock path", mutate: func(d *Deps) { d.LockPath = "" }},
		{name: "no serving answer", mutate: func(d *Deps) { d.Serves = nil }},
		{name: "no id minter", mutate: func(d *Deps) { d.MintID = nil }},
		{name: "no logger", mutate: func(d *Deps) { d.Log = nil }},
		{name: "a zero cadence", mutate: func(d *Deps) { d.Every = 0 }},
		{name: "a zero start delay", mutate: func(d *Deps) { d.StartDelay = 0 }},
		{name: "a zero recheck", mutate: func(d *Deps) { d.Recheck = 0 }},
		{name: "a source with no key", mutate: func(d *Deps) { d.Sources = []Source{{Name: "n", URL: "u", Home: "h", Format: FormatPage}} }},
		{name: "a source with no home", mutate: func(d *Deps) { d.Sources = []Source{{Key: "k", Name: "n", URL: "u", Format: FormatPage}} }},
		{name: "a source with no format", mutate: func(d *Deps) { d.Sources = []Source{{Key: "k", Name: "n", URL: "u", Home: "h"}} }},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			w := newWorld(t)
			deps := Deps{
				Sources: w.sources, Fetcher: w.fetcher, Headless: w.runner, PromptsDir: repoPromptsDir,
				Store: w.store, Clock: w.clock, LockPath: w.lock, Serves: func() bool { return true },
				MintID: func() string { return "x" },
				Every:  DefaultEvery, StartDelay: DefaultStartDelay, Recheck: DefaultRecheck, Log: w.log,
			}
			tt.mutate(&deps)

			// Act
			_, err := New(deps)

			// Assert
			if err == nil {
				t.Fatal("New = nil, want a refusal")
			}
		})
	}
}

func TestNewDefaultsTheModelTimeout(t *testing.T) {
	// Arrange
	w := newWorld(t)

	// Act
	d := w.digester()

	// Assert
	if d.condenser.timeout != DefaultModelTimeout {
		t.Fatalf("model timeout = %v, want %v", d.condenser.timeout, DefaultModelTimeout)
	}
}
