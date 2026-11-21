package testenv

import (
	"os"
	"path/filepath"
	"reflect"
	"strings"
	"testing"
)

func TestCountLogRecordsAppendsWhatReadLogRecordsSums(t *testing.T) {
	// Arrange
	path := filepath.Join(t.TempDir(), "records.jsonl")
	getenv := func(k string) string { return map[string]string{LogRecordsOut: path}[k] }

	// Act
	errs := []error{
		CountLogRecords(getenv, "TestA", 3),
		CountLogRecords(getenv, "TestB/case", 1),
		CountLogRecords(getenv, "TestA", 4),
	}
	got, err := ReadLogRecords(path)

	// Assert
	for _, e := range errs {
		if e != nil {
			t.Fatal(e)
		}
	}
	if want := map[string]int{"TestA": 7, "TestB/case": 1}; err != nil || !reflect.DeepEqual(got, want) {
		t.Fatalf("ReadLogRecords = %v, %v; want %v", got, err, want)
	}
}

func TestCountLogRecordsOutsideARunThatAskedDoesNothing(t *testing.T) {
	// Act
	err := CountLogRecords(func(string) string { return "" }, "TestA", 3)

	// Assert
	if err != nil {
		t.Fatalf("CountLogRecords = %v, want nothing done", err)
	}
}

func TestCountLogRecordsFailsWhereItCannotWrite(t *testing.T) {
	// Arrange
	path := filepath.Join(t.TempDir(), "gone", "records.jsonl")

	// Act
	err := CountLogRecords(func(string) string { return path }, "TestA", 3)

	// Assert
	if err == nil || !strings.Contains(err.Error(), "open the log record counts "+path) {
		t.Fatalf("err = %v, want the file named", err)
	}
}

func TestReadLogRecords(t *testing.T) {
	dir := t.TempDir()
	write := func(name, body string) string {
		path := filepath.Join(dir, name)
		if err := os.WriteFile(path, []byte(body), 0o644); err != nil {
			t.Fatal(err)
		}
		return path
	}
	tests := []struct {
		name    string
		path    string
		want    map[string]int
		wantErr string
	}{
		{name: "a file no harness wrote is no counts", path: filepath.Join(dir, "absent.jsonl")},
		{name: "a line that is not a count is refused", path: write("bad.jsonl", "{\"test\":\"TestA\",\"records\":1}\nnot json\n"), wantErr: "line 2 of"},
		{name: "a count naming no test is refused", path: write("unnamed.jsonl", "{\"records\":1}\n"), wantErr: "line 1 of"},
		{name: "an unreadable path fails", path: dir, wantErr: "read the log record counts"},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Act
			got, err := ReadLogRecords(tt.path)

			// Assert
			if tt.wantErr != "" {
				if err == nil || !strings.Contains(err.Error(), tt.wantErr) {
					t.Fatalf("err = %v, want one containing %q", err, tt.wantErr)
				}
				return
			}
			if err != nil || !reflect.DeepEqual(got, tt.want) {
				t.Fatalf("ReadLogRecords = %v, %v; want %v", got, err, tt.want)
			}
		})
	}
}
