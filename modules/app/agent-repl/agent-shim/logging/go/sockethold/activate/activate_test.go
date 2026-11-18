package activate

import (
	"os"
	"strings"
	"testing"
)

func TestFromSystemdEnv(t *testing.T) {
	cases := []struct {
		name      string
		env       map[string]string
		wantNames []string
		wantErr   string
	}{
		{name: "two named sockets", env: map[string]string{"LISTEN_PID": "42", "LISTEN_FDS": "2", "LISTEN_FDNAMES": "store:store-read"}, wantNames: []string{"store", "store-read"}},
		{name: "sockets passed to another process", env: map[string]string{"LISTEN_PID": "7", "LISTEN_FDS": "1", "LISTEN_FDNAMES": "store"}, wantErr: "not this process"},
		{name: "no count", env: map[string]string{"LISTEN_PID": "42"}, wantErr: "LISTEN_FDS"},
		{name: "names not matching the count", env: map[string]string{"LISTEN_PID": "42", "LISTEN_FDS": "2", "LISTEN_FDNAMES": "store"}, wantErr: "names 1 sockets"},
		{name: "an unnamed socket", env: map[string]string{"LISTEN_PID": "42", "LISTEN_FDS": "2", "LISTEN_FDNAMES": "store:"}, wantErr: "no FileDescriptorName"},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange
			var fds []uintptr
			newFile := func(fd uintptr, name string) *os.File { fds = append(fds, fd); return nil }

			// Act
			got, err := fromSystemdEnv(func(k string) string { return tc.env[k] }, 42, newFile)

			// Assert
			if tc.wantErr != "" {
				if err == nil || !strings.Contains(err.Error(), tc.wantErr) {
					t.Fatalf("err = %v, want one naming %q", err, tc.wantErr)
				}
				return
			}
			if err != nil {
				t.Fatal(err)
			}
			if len(got) != len(tc.wantNames) {
				t.Fatalf("sockets = %+v, want %v", got, tc.wantNames)
			}
			for i, s := range got {
				if s.Name != tc.wantNames[i] || fds[i] != uintptr(listenFDsStart+i) {
					t.Fatalf("socket %d = %q at fd %d, want %q at %d", i, s.Name, fds[i], tc.wantNames[i], listenFDsStart+i)
				}
			}
		})
	}
}
