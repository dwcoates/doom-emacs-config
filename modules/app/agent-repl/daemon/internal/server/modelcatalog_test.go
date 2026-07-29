package server

import (
	"strings"
	"testing"

	corev1 "agentrepl/proto/agentshim/core/v1"
)

func TestSessionModelCatalogsRejectsMalformedOptions(t *testing.T) {
	catalogs := NewSessionModelCatalogs()
	for _, tc := range []struct {
		name   string
		models []*corev1.ModelOption
		want   string
	}{
		{name: "nil option", models: []*corev1.ModelOption{nil}, want: "nil option"},
		{name: "empty option", models: []*corev1.ModelOption{{Value: ""}}, want: "empty or <synthetic>"},
		{name: "synthetic option", models: []*corev1.ModelOption{{Value: "<synthetic>"}}, want: "empty or <synthetic>"},
		{name: "duplicate option", models: []*corev1.ModelOption{{Value: "opus"}, {Value: "opus"}}, want: "duplicate"},
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
	if err := catalogs.Set("s1", []*corev1.ModelOption{{Value: "opus", DisplayName: "Opus", Description: "capable"}}); err != nil {
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
