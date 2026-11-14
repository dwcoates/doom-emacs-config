package persistentwifi

import (
	"context"
	"net"
	"os"
	"path/filepath"
	"strings"
	"sync"
	"testing"
)

// lineServer serves a Unix socket in a short directory, writing each
// connection the lines given and then holding it open until hold is closed
// (or closing it at once when end is set). It answers the socket's path.
func lineServer(t *testing.T, lines string, end bool) (path string, hold chan struct{}) {
	t.Helper()
	dir, err := os.MkdirTemp("/tmp", "pwl")
	if err != nil {
		t.Fatalf("mkdir: %v", err)
	}
	t.Cleanup(func() { _ = os.RemoveAll(dir) })
	path = filepath.Join(dir, "lid")
	ln, err := net.Listen("unix", path)
	if err != nil {
		t.Fatalf("listen: %v", err)
	}
	hold = make(chan struct{})
	var held sync.WaitGroup
	t.Cleanup(func() {
		_ = ln.Close()
		select {
		case <-hold:
		default:
			close(hold)
		}
		held.Wait()
	})
	held.Add(1)
	go func() {
		defer held.Done()
		for {
			conn, err := ln.Accept()
			if err != nil {
				return
			}
			held.Add(1)
			go func() {
				defer held.Done()
				defer conn.Close()
				_, _ = conn.Write([]byte(lines))
				if !end {
					<-hold
				}
			}()
		}
	}()
	return path, hold
}

// shade is a heard fact of the tests' own, so the shared watch forms are
// tested apart from any one fact.
type shade int

const (
	shadeLight shade = iota + 1
	shadeDark
)

func (s shade) String() string { return map[shade]string{shadeLight: "light", shadeDark: "dark"}[s] }

func parseShade(line string) (shade, error) { return parseState(line, "shade", shadeLight, shadeDark) }

func TestParseState(t *testing.T) {
	cases := []struct {
		name, line string
		want       shade
		wantErr    string
	}{
		{name: "a named state", line: "dark", want: shadeDark},
		{name: "surrounding space is trimmed", line: "  light \t", want: shadeLight},
		{name: "an unnamed line is refused naming the fact", line: "dim", wantErr: `"dim" names no shade state`},
		{name: "an empty line is refused", line: "", wantErr: `"" names no shade state`},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			// Act
			got, err := parseShade(tc.line)

			// Assert
			if tc.wantErr != "" {
				if err == nil || err.Error() != tc.wantErr {
					t.Fatalf("parseState(%q) error = %v, want %q", tc.line, err, tc.wantErr)
				}
				return
			}
			if err != nil || got != tc.want {
				t.Fatalf("parseState(%q) = %v, %v, want %v", tc.line, got, err, tc.want)
			}
		})
	}
}

func TestWatchSocket(t *testing.T) {
	cases := []struct {
		name      string
		lines     string
		end       bool
		missing   bool
		wantHeard []shade
		wantErr   string // empty for nil
	}{
		{name: "each line is heard until an unnamed one is refused", lines: "light\ndark\nbogus\n",
			wantHeard: []shade{shadeLight, shadeDark}, wantErr: `"bogus" names no shade state`},
		{name: "a stream that ended is reported naming the fact", lines: "dark\n", end: true,
			wantHeard: []shade{shadeDark}, wantErr: "the shade socket ended"},
		{name: "a missing socket is refused naming the fact", missing: true,
			wantErr: "connect to the shade socket"},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange
			path := filepath.Join("/tmp", "pwl-absent-shade")
			if !tc.missing {
				path, _ = lineServer(t, tc.lines, tc.end)
			}
			var heard []shade

			// Act
			err := watchSocket(context.Background(), path, "shade", parseShade, func(s shade) { heard = append(heard, s) })

			// Assert
			if len(heard) != len(tc.wantHeard) {
				t.Fatalf("heard = %v, want %v", heard, tc.wantHeard)
			}
			for i := range heard {
				if heard[i] != tc.wantHeard[i] {
					t.Fatalf("heard = %v, want %v", heard, tc.wantHeard)
				}
			}
			if err == nil || !strings.Contains(err.Error(), tc.wantErr) {
				t.Fatalf("watchSocket() = %v, want one containing %q", err, tc.wantErr)
			}
		})
	}
}

func TestWatchSocketEndsQuietlyWithItsContext(t *testing.T) {
	// Arrange
	path, _ := lineServer(t, "light\n", false)
	ctx, cancel := context.WithCancel(context.Background())

	// Act: the first line cancels, which closes the connection under the
	// blocked read of the next.
	err := watchSocket(ctx, path, "shade", parseShade, func(shade) { cancel() })

	// Assert
	if err != nil {
		t.Fatalf("watchSocket() = %v, want nil once ctx ended", err)
	}
}

func TestWatchFixedIsHeardOnceUntilItsContextEnds(t *testing.T) {
	// Arrange
	ctx, cancel := context.WithCancel(context.Background())
	var heard []shade

	// Act
	err := watchFixed(ctx, shadeDark, func(s shade) { heard = append(heard, s); cancel() })

	// Assert
	if err != nil || len(heard) != 1 || heard[0] != shadeDark {
		t.Fatalf("watchFixed() = %v having heard %v, want dark once and nil", err, heard)
	}
}

func TestFakeFact(t *testing.T) {
	const env = "AGENT_REPL_TEST_SHADE"
	cases := []struct {
		name, value, dir string
		wantErr          string // empty for nil
	}{
		{name: "neither set is the real machine"},
		{name: "a state with fake tools", value: "dark", dir: "/fake/bin"},
		{name: "an absolute socket with fake tools", value: "/fake/shade", dir: "/fake/bin"},
		{name: "fake tools without the fake fact are refused", dir: "/fake/bin",
			wantErr: EnvToolsDir + " is set without " + env},
		{name: "a fake fact without fake tools is refused", value: "dark",
			wantErr: env + " is set without " + EnvToolsDir},
		{name: "a value naming neither a state nor an absolute path is refused", value: "dim", dir: "/fake/bin",
			wantErr: env + `="dim" is neither a state nor an absolute socket path`},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			// Act
			err := fakeFact(env, tc.value, tc.dir, parseShade)

			// Assert
			if tc.wantErr == "" && err != nil || tc.wantErr != "" && (err == nil || !strings.Contains(err.Error(), tc.wantErr)) {
				t.Fatalf("fakeFact() = %v, want %q", err, tc.wantErr)
			}
		})
	}
}

func TestPickWatcher(t *testing.T) {
	cases := []struct{ fake, want string }{
		{fake: "", want: "system"},
		{fake: "dark", want: "fixed dark"},
		{fake: "/fake/shade", want: "socket /fake/shade"},
	}
	for _, tc := range cases {
		t.Run(tc.want, func(t *testing.T) {
			// Act
			got := pickWatcher(tc.fake, parseShade,
				func() string { return "system" },
				func(s shade) string { return "fixed " + s.String() },
				func(path string) string { return "socket " + path })

			// Assert
			if got != tc.want {
				t.Fatalf("pickWatcher(%q) = %q, want %q", tc.fake, got, tc.want)
			}
		})
	}
}
