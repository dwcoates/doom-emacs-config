package livelock

import (
	"strings"
	"testing"
)

func TestParseFlocks(t *testing.T) {
	cases := []struct {
		name  string
		table string
		want  map[fileKey]bool
	}{
		{
			name:  "an empty table holds nothing",
			table: "",
			want:  map[fileKey]bool{},
		},
		{
			name:  "a flock holder is read with a hex device and a decimal inode",
			table: "1: FLOCK  ADVISORY  WRITE 2280 fd:1a:1234 0 EOF\n",
			want:  map[fileKey]bool{{major: 0xfd, minor: 0x1a, inode: 1234}: true},
		},
		{
			name:  "a waiter is not a holder",
			table: "1: -> FLOCK  ADVISORY  WRITE 2281 00:1a:1234 0 EOF\n",
			want:  map[fileKey]bool{},
		},
		{
			name:  "a posix lock is not the shim's flock",
			table: "1: POSIX  ADVISORY  WRITE 2282 00:1a:1234 0 EOF\n",
			want:  map[fileKey]bool{},
		},
		{
			name:  "an open file description lock is not the shim's flock",
			table: "1: OFDLCK ADVISORY  READ -1 00:1a:1234 0 EOF\n",
			want:  map[fileKey]bool{},
		},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange.
			table := strings.NewReader(tc.table)

			// Act.
			got, err := parseFlocks(table)

			// Assert.
			if err != nil {
				t.Fatalf("parseFlocks failed: %v", err)
			}
			if len(got) != len(tc.want) {
				t.Fatalf("parseFlocks = %v, want %v", got, tc.want)
			}
			for key := range tc.want {
				if !got[key] {
					t.Fatalf("parseFlocks = %v, missing %v", got, key)
				}
			}
		})
	}
}

func TestParseFlocksRefuses(t *testing.T) {
	cases := []struct {
		name  string
		table string
	}{
		{name: "a truncated line", table: "1: FLOCK ADVISORY WRITE\n"},
		{name: "a file id with two parts", table: "1: FLOCK ADVISORY WRITE 1 00:1234 0 EOF\n"},
		{name: "a major that is not hex", table: "1: FLOCK ADVISORY WRITE 1 zz:1a:1234 0 EOF\n"},
		{name: "a minor that is not hex", table: "1: FLOCK ADVISORY WRITE 1 00:zz:1234 0 EOF\n"},
		{name: "an inode that is not decimal", table: "1: FLOCK ADVISORY WRITE 1 00:1a:abc 0 EOF\n"},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange.
			table := strings.NewReader(tc.table)

			// Act.
			got, err := parseFlocks(table)

			// Assert.
			if err == nil {
				t.Fatalf("parseFlocks accepted a malformed table and answered %v", got)
			}
		})
	}
}
