package procstat

import (
	"slices"
	"strings"
	"testing"
)

func TestParseStat(t *testing.T) {
	cases := []struct {
		name       string
		stat       string
		wantComm   string
		wantFields []string
		wantErr    string
	}{
		{name: "an ordinary line", stat: "42 (node) S 1 42", wantComm: "node", wantFields: []string{"S", "1", "42"}},
		{name: "a command name holding spaces and parentheses", stat: "42 (a (b) c) R 1", wantComm: "a (b) c", wantFields: []string{"R", "1"}},
		{name: "a line with no command name", stat: "42 node R", wantErr: "no command name"},
		{name: "a close before the open", stat: "42 )node( R", wantErr: "no command name"},
		{name: "a line with nothing after its command name", stat: "42 (node) ", wantErr: "no state after its command name"},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			// Act
			got, err := ParseStat(tc.stat)

			// Assert
			if tc.wantErr != "" {
				if err == nil || !strings.Contains(err.Error(), tc.wantErr) {
					t.Fatalf("ParseStat error = %v, want one containing %q", err, tc.wantErr)
				}
				return
			}
			if err != nil || got.Comm != tc.wantComm || !slices.Equal(got.Fields, tc.wantFields) {
				t.Fatalf("ParseStat = %+v, %v; want comm %q fields %v", got, err, tc.wantComm, tc.wantFields)
			}
		})
	}
}

func TestStatLineField(t *testing.T) {
	// Arrange
	line, err := ParseStat("42 (node) S 1 42")
	if err != nil {
		t.Fatal(err)
	}
	cases := []struct {
		name    string
		n       int
		want    string
		wantErr bool
	}{
		{name: "the state is field 3", n: 3, want: "S"},
		{name: "the ppid is field 4", n: 4, want: "1"},
		{name: "the last field present", n: 5, want: "42"},
		{name: "a field past the line's end is refused", n: 6, wantErr: true},
		{name: "a field before the state is not after the command", n: 2, wantErr: true},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			// Act
			got, err := line.Field(tc.n)

			// Assert
			if (err != nil) != tc.wantErr || got != tc.want {
				t.Fatalf("Field(%d) = (%q, %v), want %q (error %v)", tc.n, got, err, tc.want, tc.wantErr)
			}
		})
	}
}

func TestParseCmdline(t *testing.T) {
	cases := []struct {
		name string
		raw  string
		want []string
	}{
		{name: "argv split on its NULs", raw: "node\x00main.js\x00--listen\x00/s/ws.sock\x00", want: []string{"node", "main.js", "--listen", "/s/ws.sock"}},
		{name: "an empty argument inside argv is kept", raw: "a\x00\x00b\x00", want: []string{"a", "", "b"}},
		{name: "a rewritten title's NUL padding is dropped", raw: "node (vitest)\x00\x00\x00\x00", want: []string{"node (vitest)"}},
		{name: "a single argument stands alone", raw: "/bin/x\x00", want: []string{"/bin/x"}},
		{name: "a kernel thread has no argv", raw: "", want: []string{}},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			// Act
			got := ParseCmdline([]byte(tc.raw))

			// Assert
			if !slices.Equal(got, tc.want) {
				t.Fatalf("ParseCmdline = %q, want %q", got, tc.want)
			}
		})
	}
}

func TestHandParsed(t *testing.T) {
	cases := []struct {
		name string
		src  string
		want bool
	}{
		{name: "a stat line split at its command by hand", src: "i := strings.LastIndexByte(stat, ')')", want: true},
		{name: "a cmdline read without ParseCmdline", src: `os.ReadFile(dir + "/cmdline")`, want: true},
		{name: "a cmdline read decoded by ParseCmdline", src: "raw, _ := tree.Read(pid, \"cmdline\")\nprocstat.ParseCmdline(raw)", want: false},
		{name: "a pid joined onto /proc/ by hand", src: `os.ReadFile("/proc/" + pid + "/stat")`, want: true},
		{name: "pprof's cmdline endpoint is no /proc read", src: `mux.HandleFunc("/debug/pprof/cmdline", pprof.Cmdline)`, want: false},
		{name: "source that reads no /proc file", src: "package x\nfunc f() {}", want: false},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			// Act
			got := HandParsed(tc.src)

			// Assert
			if got != tc.want {
				t.Fatalf("HandParsed(%q) = %v, want %v", tc.src, got, tc.want)
			}
		})
	}
}
