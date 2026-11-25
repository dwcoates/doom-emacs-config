package daemonrules

import (
	"reflect"
	"regexp"
	"testing"

	"agent-repld/reposcans/internal/sourcescan"
)

// internalDesktopnotifyDir is the package these rules hold, under the daemon module's root.
const internalDesktopnotifyDir = "internal/desktopnotify"

// TestEveryProgramRunGoesThroughRunWithin pins that no backend, chime or
// speech bounds a run of its own: the one bounded run is runWithin, across
// every production source of the package.
func TestEveryProgramRunGoesThroughRunWithin(t *testing.T) {
	sourcescan.InPackage(t, internalDesktopnotifyDir)
	cases := []struct {
		substr string
		want   []string
	}{
		// launcher.go bounds one socket exchange with the launcher, which is no
		// program run: the launcher is the one resident program, already running.
		{substr: "context.WithTimeout(", want: []string{"launcher.go"}},
		{substr: "context.WithTimeoutCause(", want: []string{"backend.go"}},
		{substr: "runner.Run(", want: []string{"backend.go"}},
	}
	for _, tc := range cases {
		t.Run(tc.substr, func(t *testing.T) {
			// Act
			got := sourcescan.Sites(t, internalDesktopnotifyDir, tc.substr)

			// Assert
			if !reflect.DeepEqual(got, tc.want) {
				t.Fatalf("%q occurs in %v, want only runWithin's %v", tc.substr, got, tc.want)
			}
		})
	}
}

// TestSpeechHasOneChokePoint pins that every banner reaches the platform
// through the one Backend.Post call, and is read aloud from its made edge
// alone: no kind of banner can speak, or skip speaking, on its own.
func TestSpeechHasOneChokePoint(t *testing.T) {
	sourcescan.InPackage(t, internalDesktopnotifyDir)
	cases := []struct {
		substr string
		want   []string
	}{
		{substr: "Backend.Post(", want: []string{"notifier.go"}},
		{substr: "n.speak(", want: []string{"notifier.go"}},
		{substr: ".Speak(", want: []string{"notifier.go"}},
	}
	for _, tc := range cases {
		t.Run(tc.substr, func(t *testing.T) {
			// Act
			got := sourcescan.Sites(t, internalDesktopnotifyDir, tc.substr)

			// Assert
			if !reflect.DeepEqual(got, tc.want) {
				t.Fatalf("%q is called in %v, want only %v", tc.substr, got, tc.want)
			}
		})
	}
}

// TestEveryEventTitleIsComposedByEventTitle pins the shared shape: every
// banner title in the package is the workspace's name, either bare (an agent
// notification) or behind its mark through eventTitle.
func TestEveryEventTitleIsComposedByEventTitle(t *testing.T) {
	sourcescan.InPackage(t, internalDesktopnotifyDir)
	titled := regexp.MustCompile(`Title: (.+?), Body`)
	allowed := regexp.MustCompile(`^(name|eventTitle\(mark, name\)|TurnTitle\(end, name\)|MergeQueuedTitle\(name\)|DeployMediatedTitle\(name\)|CommentTitle\(name\))$`)
	// Arrange / Act
	seen := 0
	for _, file := range sourcescan.Production(t, internalDesktopnotifyDir) {
		for _, m := range titled.FindAllStringSubmatch(string(file.Source), -1) {
			seen++
			// Assert
			if !allowed.MatchString(m[1]) {
				t.Errorf("%s titles a banner %q: compose titles through eventTitle", file.Name, m[1])
			}
		}
	}
	if seen == 0 {
		t.Fatal("found no banner titles to check")
	}
}
