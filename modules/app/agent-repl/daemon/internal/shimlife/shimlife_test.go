package shimlife

import (
	"context"
	"testing"
)

func TestTheStartCauseRidesTheContext(t *testing.T) {
	tests := []struct {
		name   string
		ctx    context.Context
		want   StartCause
		wantOK bool
	}{
		{name: "a stated cause is read back", ctx: WithStart(context.Background(), StartPrompt), want: StartPrompt, wantOK: true},
		{name: "no cause stated is none", ctx: context.Background()},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Act
			got, ok := StartOf(tt.ctx)

			// Assert
			if got != tt.want || ok != tt.wantOK {
				t.Fatalf("StartOf = %q, %v; want %q, %v", got, ok, tt.want, tt.wantOK)
			}
		})
	}
}

func TestTheStandDownReasonRidesTheContext(t *testing.T) {
	tests := []struct {
		name   string
		ctx    context.Context
		want   StandDownReason
		wantOK bool
	}{
		{name: "a stated reason is read back", ctx: WithStandDown(context.Background(), StandDownHibernate), want: StandDownHibernate, wantOK: true},
		{name: "no reason stated is none", ctx: context.Background()},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Act
			got, ok := StandDownOf(tt.ctx)

			// Assert
			if got != tt.want || ok != tt.wantOK {
				t.Fatalf("StandDownOf = %q, %v; want %q, %v", got, ok, tt.want, tt.wantOK)
			}
		})
	}
}

func TestTheStandDownDetailRidesTheContext(t *testing.T) {
	cases := []struct {
		name   string
		ctx    context.Context
		want   string
		stated bool
	}{
		{name: "a stated detail", ctx: WithStandDownDetail(context.Background(), "parked after its idle grace"), want: "parked after its idle grace", stated: true},
		{name: "no detail", ctx: context.Background()},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			// Act
			got, stated := StandDownDetailOf(tc.ctx)

			// Assert
			if got != tc.want || stated != tc.stated {
				t.Fatalf("StandDownDetailOf = %q, %v; want %q, %v", got, stated, tc.want, tc.stated)
			}
		})
	}
}
