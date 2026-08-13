package server

import (
	"strings"
	"testing"

	protocolv1 "agentrepl/proto/protocol/v1"
)

func TestSessionModelCatalogsRejectsMalformedOptions(t *testing.T) {
	catalogs := NewSessionModelCatalogs()
	for _, tc := range []struct {
		name   string
		models []*protocolv1.ModelOption
		want   string
	}{
		{name: "nil option", models: []*protocolv1.ModelOption{nil}, want: "nil option"},
		{name: "empty option", models: []*protocolv1.ModelOption{{Value: ""}}, want: "empty or <synthetic>"},
		{name: "synthetic option", models: []*protocolv1.ModelOption{{Value: "<synthetic>"}}, want: "empty or <synthetic>"},
		{name: "duplicate option", models: []*protocolv1.ModelOption{{Value: "opus"}, {Value: "opus"}}, want: "duplicate"},
	} {
		t.Run(tc.name, func(t *testing.T) {
			err := catalogs.Set("s1", tc.models)
			if err == nil || !strings.Contains(err.Error(), tc.want) {
				t.Fatalf("Set() error = %v, want containing %q", err, tc.want)
			}
		})
	}
}

func TestSessionModelCatalogsNormalizesAndCopiesRealOptions(t *testing.T) {
	catalogs := NewSessionModelCatalogs()
	if err := catalogs.Set("s1", []*protocolv1.ModelOption{{Value: "opus", DisplayName: "Opus", Description: "capable"}}); err != nil {
		t.Fatalf("Set(): %v", err)
	}
	got := catalogs.Get("s1")
	if len(got) != 1 || got[0].GetValue() != "opus" {
		t.Fatalf("Get() = %#v, want opus", got)
	}
	got[0].Value = "mutated"
	if again := catalogs.Get("s1"); again[0].GetValue() != "opus" {
		t.Fatalf("Get() leaked a mutable stored option: %#v", again)
	}
}
