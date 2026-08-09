package externalbrowser

import (
	"errors"
	"reflect"
	"strings"
	"testing"
)

// call is one recorded Runner invocation.
type call struct {
	name string
	args []string
}

// recorder returns a Runner that records every invocation and hands out errs
// one per call, in order (a call past the end of errs succeeds).
func recorder(calls *[]call, errs ...error) Runner {
	return func(name string, args ...string) error {
		*calls = append(*calls, call{name: name, args: args})
		if len(errs) == 0 {
			return nil
		}
		err := errs[0]
		errs = errs[1:]
		return err
	}
}

func TestValidate(t *testing.T) {
	tests := []struct {
		name    string
		url     string
		wantErr bool
	}{
		{"https is accepted", "https://example.com/x", false},
		{"http is accepted", "http://example.com/x", false},
		{"empty is refused", "", true},
		{"file scheme is refused", "file:///etc/passwd", true},
		{"javascript scheme is refused", "javascript:alert(1)", true},
		{"scheme-less is refused", "example.com", true},
		{"embedded whitespace is refused", "https://example.com/a b", true},
		{"embedded newline is refused", "https://example.com/a\nb", true},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Act.
			err := Validate(tt.url)

			// Assert.
			if gotErr := err != nil; gotErr != tt.wantErr {
				t.Fatalf("Validate(%q) error = %v, wantErr = %v", tt.url, err, tt.wantErr)
			}
		})
	}
}

func TestLaunchArgvPinsTheProfile(t *testing.T) {
	// Act.
	got := LaunchArgv("https://example.com/x")

	// Assert.
	want := []string{"--profile-directory=Profile 6", "https://example.com/x"}
	if !reflect.DeepEqual(got, want) {
		t.Errorf("LaunchArgv() = %q, want %q", got, want)
	}
}

func TestActivateArgvNamesTheApp(t *testing.T) {
	// Act.
	got := ActivateArgv()

	// Assert.
	want := []string{"-e", `tell application "Google Chrome" to activate`}
	if !reflect.DeepEqual(got, want) {
		t.Errorf("ActivateArgv() = %q, want %q", got, want)
	}
}

func TestOpenHandsTheURLToChromeWithTheProfile(t *testing.T) {
	// Arrange.
	var calls []call

	// Act.
	if err := Open("https://example.com/x", recorder(&calls)); err != nil {
		t.Fatalf("Open() error = %v, want nil", err)
	}

	// Assert.
	if len(calls) != 2 {
		t.Fatalf("Open() ran %d commands, want 2", len(calls))
	}
	want := call{name: Binary, args: []string{"--profile-directory=Profile 6", "https://example.com/x"}}
	if !reflect.DeepEqual(calls[1], want) {
		t.Errorf("Open() second call = %+v, want %+v", calls[1], want)
	}
}

// TestOpenRaisesTheBrowserBeforeHandingTheURLOver pins the ORDER, which is what
// puts focus on the right window: Chrome raises the profile window it puts the
// tab in but never fronts itself, so an activation that came second would
// restore whichever window was frontmost before — routinely the other profile's.
func TestOpenRaisesTheBrowserBeforeHandingTheURLOver(t *testing.T) {
	// Arrange.
	var calls []call

	// Act.
	if err := Open("https://example.com/x", recorder(&calls)); err != nil {
		t.Fatalf("Open() error = %v, want nil", err)
	}

	// Assert.
	if len(calls) == 0 {
		t.Fatal("Open() ran no command")
	}
	want := call{name: "osascript", args: ActivateArgv()}
	if !reflect.DeepEqual(calls[0], want) {
		t.Errorf("Open() first call = %+v, want %+v", calls[0], want)
	}
}

func TestOpenRefusesANonHTTPURL(t *testing.T) {
	// Arrange.
	var calls []call

	// Act.
	err := Open("file:///etc/passwd", recorder(&calls))

	// Assert.
	if err == nil {
		t.Fatal("Open() error = nil, want a refusal")
	}
	if len(calls) != 0 {
		t.Errorf("Open() ran %d commands for a refused url, want 0", len(calls))
	}
}

func TestOpenSurfacesAFailedHandOff(t *testing.T) {
	// Arrange.
	var calls []call
	boom := errors.New("chrome exploded")

	// Act.
	err := Open("https://example.com/x", recorder(&calls, nil, boom))

	// Assert.
	if !errors.Is(err, boom) {
		t.Fatalf("Open() error = %v, want it to wrap %v", err, boom)
	}
	if !strings.Contains(err.Error(), "Profile 6") {
		t.Errorf("Open() error = %q, want it to name the profile it tried", err)
	}
}

func TestOpenSurfacesAFailedRaise(t *testing.T) {
	// Arrange.
	var calls []call
	boom := errors.New("osascript exploded")

	// Act.
	err := Open("https://example.com/x", recorder(&calls, boom))

	// Assert.
	if !errors.Is(err, boom) {
		t.Fatalf("Open() error = %v, want it to wrap %v", err, boom)
	}
}

func TestOpenDoesNotHandTheURLOverAfterAFailedRaise(t *testing.T) {
	// Arrange.
	var calls []call

	// Act.
	if err := Open("https://example.com/x", recorder(&calls, errors.New("boom"))); err == nil {
		t.Fatal("Open() error = nil, want a failure")
	}

	// Assert.
	if len(calls) != 1 {
		t.Errorf("Open() ran %d commands after a failed raise, want 1", len(calls))
	}
}
