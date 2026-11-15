package harness

import (
	"context"
	"os"
	"path/filepath"
	"slices"
	"testing"
	"time"
)

// awaitChangeBound bounds a wait that must see a write this test makes at
// once; the write is the wait's only work, so it never fails a passing run.
const awaitChangeBound = 10 * time.Second

func TestFollowFilesFollowsWhatTheWaitMustSee(t *testing.T) {
	tests := []struct {
		name string
		// arrange lays out the tree under root and answers the file waited on
		// and the paths the wait must follow.
		arrange func(t *testing.T, root string) (string, []string)
	}{
		{
			name: "a file and its directory",
			arrange: func(t *testing.T, root string) (string, []string) {
				f := filepath.Join(root, "a.log")
				return f, []string{f, root}
			},
		},
		{
			name: "a file whose directories are not made yet, up to the first that is",
			arrange: func(t *testing.T, root string) (string, []string) {
				f := filepath.Join(root, "x", "y", "a.log")
				return f, []string{f, filepath.Join(root, "x", "y"), filepath.Join(root, "x"), root}
			},
		},
		{
			name: "a link whose target is not written yet, through its target's directory",
			arrange: func(t *testing.T, root string) (string, []string) {
				link := filepath.Join(root, "emacs", "webapp.log")
				target := filepath.Join(root, "logs", "webapp-1.log")
				mkdir(t, filepath.Dir(link), filepath.Dir(target))
				if err := os.Symlink(target, link); err != nil {
					t.Fatal(err)
				}
				return link, []string{link, filepath.Dir(link), target, filepath.Dir(target)}
			},
		},
		{
			name: "a relative link, resolved against the link's directory",
			arrange: func(t *testing.T, root string) (string, []string) {
				link := filepath.Join(root, "emacs", "webapp.log")
				mkdir(t, filepath.Dir(link), filepath.Join(root, "logs"))
				if err := os.Symlink("../logs/webapp-1.log", link); err != nil {
					t.Fatal(err)
				}
				target := filepath.Join(root, "emacs", "..", "logs", "webapp-1.log")
				return link, []string{link, filepath.Dir(link), filepath.Clean(target), filepath.Join(root, "logs")}
			},
		},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			f, want := tt.arrange(t, t.TempDir())

			// Act
			got := followFiles(f)()

			// Assert
			for _, p := range want {
				if !slices.Contains(got, filepath.Clean(p)) && !slices.Contains(got, p) {
					t.Fatalf("followFiles(%s) = %v, want it to follow %s", f, got, p)
				}
			}
		})
	}
}

// TestAWaitOnADanglingLinkSeesItsTargetWritten pins 2026-11-15 06:31: a wait
// taken on a workspace sink link before its target existed never saw the
// target written, and ran out its bound with the record standing.
func TestAWaitOnADanglingLinkSeesItsTargetWritten(t *testing.T) {
	// Arrange
	root := t.TempDir()
	link := filepath.Join(root, "emacs", "webapp.log")
	target := filepath.Join(root, "logs", "webapp-1.log")
	mkdir(t, filepath.Dir(link), filepath.Dir(target))
	if err := os.Symlink(target, link); err != nil {
		t.Fatal(err)
	}
	wait, cancel := context.WithTimeout(context.Background(), awaitChangeBound)
	defer cancel()
	looked := make(chan struct{}, 1)
	check := func() bool {
		select {
		case looked <- struct{}{}:
		default:
		}
		_, err := os.Stat(link)
		return err == nil
	}
	done := make(chan bool, 1)

	// Act
	go func() { done <- awaitChange(t, wait, check, followFiles(link)) }()
	<-looked
	if err := os.WriteFile(target, []byte("{}\n"), 0o600); err != nil {
		t.Fatal(err)
	}

	// Assert
	if !<-done {
		t.Fatal("the wait never saw the link's target written")
	}
}

// A LINK RENAMED INTO PLACE WHILE THE ROUND'S WATCH WAS TAKEN: its watch on
// the link found nothing, the directory's watch was taken after the link
// appeared, and the target's first write went unreported (2026-11-15 20:19).
// The first round's paths stand for that watch: the directory alone, with the
// link made before the watch is.
func TestAWaitSeesATargetLinkedWhileItsWatchWasTaken(t *testing.T) {
	// Arrange
	root := t.TempDir()
	link := filepath.Join(root, "emacs", "webapp.log")
	target := filepath.Join(root, "logs", "webapp-1.log")
	mkdir(t, filepath.Dir(link), filepath.Dir(target))
	if err := os.WriteFile(target, nil, 0o600); err != nil {
		t.Fatal(err)
	}
	follow := followFiles(link)
	linked := false
	paths := func() []string {
		if linked {
			return follow()
		}
		linked = true
		if err := os.Symlink(target, link); err != nil {
			t.Error(err)
		}
		return []string{filepath.Dir(link)}
	}
	wait, cancel := context.WithTimeout(context.Background(), awaitChangeBound)
	defer cancel()
	looked := make(chan struct{}, 1)
	check := func() bool {
		info, err := os.Stat(link)
		select {
		case looked <- struct{}{}:
		default:
		}
		return err == nil && info.Size() > 0
	}
	done := make(chan bool, 1)

	// Act
	go func() { done <- awaitChange(t, wait, check, paths) }()
	<-looked
	if err := os.WriteFile(target, []byte("{}\n"), 0o600); err != nil {
		t.Fatal(err)
	}

	// Assert
	if !<-done {
		t.Fatal("the wait never saw the target written after its link appeared mid-round")
	}
}

func mkdir(t *testing.T, dirs ...string) {
	t.Helper()
	for _, d := range dirs {
		if err := os.MkdirAll(d, 0o700); err != nil {
			t.Fatal(err)
		}
	}
}
