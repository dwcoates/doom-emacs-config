package protofile

import (
	"os"
	"path/filepath"
	"strings"
	"testing"

	"google.golang.org/protobuf/encoding/protowire"
	"google.golang.org/protobuf/proto"

	testrunv1 "agentrepl/proto/testrun/v1"
)

// withUnknown appends an unknown varint field (number 99) to data.
func withUnknown(data []byte) []byte {
	data = protowire.AppendTag(data, 99, protowire.VarintType)
	return protowire.AppendVarint(data, 1)
}

func write(t *testing.T, data []byte) string {
	t.Helper()
	path := filepath.Join(t.TempDir(), "req.pb")
	if err := os.WriteFile(path, data, 0o644); err != nil {
		t.Fatal(err)
	}
	return path
}

func ertAll(file string) *testrunv1.TestSelection {
	return &testrunv1.TestSelection{Target: &testrunv1.TestSelection_ErtFile{ErtFile: &testrunv1.TestSelectionErtFile{
		File: file, Tests: &testrunv1.TestSelectionErtFile_All{All: &testrunv1.TestSelectionErtAllTests{}}}}}
}

func TestReadDecodesAKnownRequest(t *testing.T) {
	// Arrange
	want := &testrunv1.TestRunRequest{Selections: []*testrunv1.TestSelection{ertAll("lisp/test-x.el")}, Coverage: true}
	data, err := proto.Marshal(want)
	if err != nil {
		t.Fatal(err)
	}

	// Act
	got := &testrunv1.TestRunRequest{}
	err = Read(write(t, data), got)

	// Assert
	if err != nil || !proto.Equal(got, want) {
		t.Fatalf("Read = %v, %v; want %v", got, err, want)
	}
}

func TestReadRefusesUnknownFields(t *testing.T) {
	sel, err := proto.Marshal(ertAll("lisp/test-x.el"))
	if err != nil {
		t.Fatal(err)
	}
	cases := []struct {
		name  string
		data  []byte
		where string
	}{
		{name: "an unknown field at the top", data: withUnknown(nil), where: "testrun.v1.TestRunRequest"},
		{name: "an unknown field in a listed message", data: protowire.AppendBytes(protowire.AppendTag(nil, 1, protowire.BytesType), withUnknown(sel)), where: "testrun.v1.TestSelection"},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			// Act
			err := Read(write(t, tc.data), &testrunv1.TestRunRequest{})

			// Assert
			if err == nil || !strings.Contains(err.Error(), "in "+tc.where) || !strings.Contains(err.Error(), "rebase") {
				t.Fatalf("Read = %v, want a refusal naming %s", err, tc.where)
			}
		})
	}
}

func TestReadRefusesAnUnreadableFile(t *testing.T) {
	cases := []struct {
		name string
		path func(t *testing.T) string
		want string
	}{
		{name: "a missing file", path: func(t *testing.T) string { return filepath.Join(t.TempDir(), "none.pb") }, want: "read "},
		{name: "bytes that are no message", path: func(t *testing.T) string { return write(t, []byte{0xff, 0xff}) }, want: "decode "},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			// Act
			err := Read(tc.path(t), &testrunv1.TestRunRequest{})

			// Assert
			if err == nil || !strings.Contains(err.Error(), tc.want) {
				t.Fatalf("Read = %v, want an error containing %q", err, tc.want)
			}
		})
	}
}
