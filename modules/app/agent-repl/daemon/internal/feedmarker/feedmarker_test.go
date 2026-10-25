package feedmarker

import (
	"testing"

	frontendv1 "agentrepl/proto/frontend/v1"

	"google.golang.org/protobuf/proto"
)

func TestNeutral(t *testing.T) {
	tests := []struct {
		name   string
		label  string
		detail string
		want   *frontendv1.FeedOutcomeMarker
	}{
		{
			name: "a label alone carries no detail", label: VendorNoticeLabel,
			want: &frontendv1.FeedOutcomeMarker{
				Label:  &frontendv1.FeedOutcomeMarkerLabel{Text: "vendor notice"},
				Family: &frontendv1.FeedOutcomeMarker_Neutral{Neutral: &frontendv1.FeedOutcomeMarkerNeutral{}},
			},
		},
		{
			name: "a detail is carried", label: "interrupted", detail: "by you",
			want: &frontendv1.FeedOutcomeMarker{
				Label:  &frontendv1.FeedOutcomeMarkerLabel{Text: "interrupted"},
				Detail: &frontendv1.FeedOutcomeMarkerDetail{Text: "by you"},
				Family: &frontendv1.FeedOutcomeMarker_Neutral{Neutral: &frontendv1.FeedOutcomeMarkerNeutral{}},
			},
		},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Act
			got := Neutral(tt.label, tt.detail)

			// Assert
			if !proto.Equal(got, tt.want) {
				t.Fatalf("Neutral = %v, want %v", got, tt.want)
			}
		})
	}
}
