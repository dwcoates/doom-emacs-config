package main

import (
	"testing"

	"claude-repld/internal/server"
)

func TestParseAccounts(t *testing.T) {
	tests := []struct {
		name    string
		raw     string
		want    []server.Account
		wantErr bool
	}{
		{
			name: "empty flag is an unconfigured roster, not an error",
			raw:  "",
			want: nil,
		},
		{
			name: "one pair",
			raw:  "work=/home/u/.claude-chesscom",
			want: []server.Account{{Label: "work", ConfigDir: "/home/u/.claude-chesscom"}},
		},
		{
			name: "empty dir names the CLI default root",
			raw:  "personal=",
			want: []server.Account{{Label: "personal", ConfigDir: ""}},
		},
		{
			name: "two pairs keep roster order",
			raw:  "personal=,work=/home/u/.claude-chesscom",
			want: []server.Account{
				{Label: "personal", ConfigDir: ""},
				{Label: "work", ConfigDir: "/home/u/.claude-chesscom"},
			},
		},
		{
			name:    "pair without an equals sign is malformed",
			raw:     "personal",
			wantErr: true,
		},
		{
			name:    "empty label is malformed",
			raw:     "=/home/u/.claude",
			wantErr: true,
		},
		{
			name:    "duplicate label is rejected",
			raw:     "work=/a,work=/b",
			wantErr: true,
		},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Act
			got, err := parseAccounts(tt.raw)

			// Assert
			if tt.wantErr {
				if err == nil {
					t.Fatalf("parseAccounts(%q) = %v, want error", tt.raw, got)
				}
				return
			}
			if err != nil {
				t.Fatalf("parseAccounts(%q): %v", tt.raw, err)
			}
			if len(got) != len(tt.want) {
				t.Fatalf("parseAccounts(%q) = %v, want %v", tt.raw, got, tt.want)
			}
			for i := range got {
				if got[i] != tt.want[i] {
					t.Errorf("account[%d] = %v, want %v", i, got[i], tt.want[i])
				}
			}
		})
	}
}
