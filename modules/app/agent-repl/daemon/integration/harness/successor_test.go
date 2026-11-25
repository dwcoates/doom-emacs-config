package harness

import (
	"slices"
	"testing"
)

func TestSuccessorOptsCarriesThePredecessorsMachine(t *testing.T) {
	// Arrange
	d := &Daemon{StateDir: "/s/state", ProfileDir: "/s/profiles", DefaultConfigDir: "/s/config-default"}

	// Act
	got := d.SuccessorOpts()

	// Assert
	if got.StateDir != d.StateDir || got.ProfileDir != d.ProfileDir {
		t.Fatalf("SuccessorOpts() = %+v, want the predecessor's state root and profiles", got)
	}
	if want := []string{"--default-config-dir", d.DefaultConfigDir}; !slices.Equal(got.ExtraArgs, want) {
		t.Fatalf("SuccessorOpts().ExtraArgs = %q, want %q", got.ExtraArgs, want)
	}
}

func TestSuccessorOptsCarriesThePredecessorsSocketHolder(t *testing.T) {
	// Arrange
	holder := &SocketHolder{Control: "/h/control.sock", Address: "127.0.0.1:41000"}
	d := &Daemon{StateDir: "/s/state", Holder: holder}

	// Act
	got := d.SuccessorOpts()

	// Assert: one shared address outlives every daemon.
	if got.Holder != holder {
		t.Fatalf("SuccessorOpts().Holder = %+v, want the predecessor's", got.Holder)
	}
}

func TestSuccessorOptsSharesThePredecessorsLocks(t *testing.T) {
	// Arrange
	d := &Daemon{StateDir: "/s/state", LockDir: LockDirFor("/s/state")}

	// Act
	got := LockDirFor(d.SuccessorOpts().StateDir)

	// Assert
	if got != d.LockDir {
		t.Fatalf("the successor's lock directory = %q, want the predecessor's %q", got, d.LockDir)
	}
}

func TestSuccessorOptsAreFreshPerCall(t *testing.T) {
	// Arrange
	d := &Daemon{DefaultConfigDir: "/s/config-default"}
	first := d.SuccessorOpts()

	// Act: one caller extends its own successor's arguments.
	first.ExtraArgs = append(first.ExtraArgs, "--multi-repo-config-dir", "/s/multi")
	second := d.SuccessorOpts()

	// Assert
	if len(second.ExtraArgs) != 2 {
		t.Fatalf("a later SuccessorOpts().ExtraArgs = %q, want it untouched by an earlier caller's extension", second.ExtraArgs)
	}
}

func TestAdoptConfigDirArgsKeepsTheStructWhereTheArgvPoints(t *testing.T) {
	tests := []struct {
		name                   string
		extra                  []string
		wantDefault, wantMulti string
	}{
		{name: "no override keeps the minted roots", extra: nil, wantDefault: "/minted/default", wantMulti: "/minted/multi"},
		{name: "a default override replaces the default root", extra: []string{"--default-config-dir", "/carried"}, wantDefault: "/carried", wantMulti: "/minted/multi"},
		{name: "a multi-repo override replaces the multi-repo root", extra: []string{"--multi-repo-config-dir", "/carried-m"}, wantDefault: "/minted/default", wantMulti: "/carried-m"},
		{name: "the last of two overrides wins, as at the daemon", extra: []string{"--default-config-dir", "/a", "--default-config-dir", "/b"}, wantDefault: "/b", wantMulti: "/minted/multi"},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			d := &Daemon{DefaultConfigDir: "/minted/default", MultiRepoConfigDir: "/minted/multi"}

			// Act
			d.adoptConfigDirArgs(tt.extra)

			// Assert
			if d.DefaultConfigDir != tt.wantDefault || d.MultiRepoConfigDir != tt.wantMulti {
				t.Fatalf("roots after %q = (%q, %q), want (%q, %q)", tt.extra, d.DefaultConfigDir, d.MultiRepoConfigDir, tt.wantDefault, tt.wantMulti)
			}
		})
	}
}
