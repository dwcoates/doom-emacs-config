package server

import (
	"context"
	"testing"

	"connectrpc.com/connect"
	"google.golang.org/protobuf/proto"

	agentreplv1 "agentrepl/proto/agentrepl/v1"
)

// TestListSlackChannelsAnswersTheDirectorysResponse pins that the rpc answers
// exactly what the daemon's channel directory answers, in every arm.
func TestListSlackChannelsAnswersTheDirectorysResponse(t *testing.T) {
	cases := []struct {
		name     string
		response *agentreplv1.ListSlackChannelsResponse
	}{
		{"listed", &agentreplv1.ListSlackChannelsResponse{Result: &agentreplv1.ListSlackChannelsResponse_Success{
			Success: &agentreplv1.ListSlackChannelsSuccess{Listing: &agentreplv1.ListSlackChannelsSuccess_Listed{
				Listed: &agentreplv1.ListSlackChannelsListed{Channels: []*agentreplv1.SlackChannel{{Id: "C1", Name: "general"}}},
			}},
		}}},
		{"pending", &agentreplv1.ListSlackChannelsResponse{Result: &agentreplv1.ListSlackChannelsResponse_Success{
			Success: &agentreplv1.ListSlackChannelsSuccess{Listing: &agentreplv1.ListSlackChannelsSuccess_Pending{
				Pending: &agentreplv1.ListSlackChannelsPending{},
			}},
		}}},
		{"read_failed", &agentreplv1.ListSlackChannelsResponse{Result: &agentreplv1.ListSlackChannelsResponse_Error{
			Error: &agentreplv1.ListSlackChannelsError{Cause: &agentreplv1.ListSlackChannelsError_ReadFailed{
				ReadFailed: &agentreplv1.ListSlackChannelsReadFailed{Detail: "gns exited 1"},
			}},
		}}},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange.
			h := newHarness(t)
			h.SlackChannels.response = tc.response

			// Act.
			resp, err := h.Client.ListSlackChannels(context.Background(), connect.NewRequest(&agentreplv1.ListSlackChannelsRequest{}))

			// Assert.
			if err != nil {
				t.Fatalf("ListSlackChannels: %v", err)
			}
			if !proto.Equal(resp.Msg, tc.response) {
				t.Fatalf("response = %v, want %v", resp.Msg, tc.response)
			}
		})
	}
}
