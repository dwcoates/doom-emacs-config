package chessboard

import (
	"bytes"
	"context"
	"encoding/json"
	"errors"
	"fmt"
	"io"
	"net/http"

	"google.golang.org/protobuf/encoding/protowire"
)

// THE CEE CALLS, over Connect's unary protocol with its binary codec. The two
// requests are tiny and are encoded here with protowire against CEE's field
// numbers, because CEE's schema lives in another repository whose bindings
// this module does not import (docs/protobuf-design/chess-board-feed.md; the
// numbers are vetting item V1). The answers are relayed as bytes: the widget
// data and the square answer reach the widget exactly as cee-webapp sent them.

// The two procedures, as Connect routes them.
const (
	procGetCeeWebWidget = "/chesscom.cee_webapp.v1.CeeWebWidgetService/GetCeeWebWidget"
	procGetSquareEvents = "/chesscom.cee_webapp.v1.CeeWebWidgetService/GetSquareEvents"
)

// CEE's field numbers (chesscom.cee_webapp.v1).
const (
	// CeeSessionMetadata.session_id and .game_id.
	fieldMetadataSessionID protowire.Number = 1
	fieldMetadataGameID    protowire.Number = 2
	// GetCeeWebWidgetRequest.session and GetSquareEventsRequest.session.
	fieldRequestSession protowire.Number = 1
	// GetSquareEventsRequest.game_point (int64) and .square (enum).
	fieldRequestGamePoint protowire.Number = 2
	fieldRequestSquare    protowire.Number = 3
	// GetCeeWebWidgetResponse.widget.
	fieldResponseWidget protowire.Number = 1
)

// errSessionGone is CEE refusing a session that no longer holds the game: the
// session was swept, or another game was loaded into it (Connect
// failed_precondition, CEE's own code for both).
var errSessionGone = errors.New("chessboard: the CEE session no longer holds the game")

// errCall is a CEE call that got no answer, or an answer that is not one.
var errCall = errors.New("chessboard: the CEE call failed")

// sessionMetadata encodes a CeeSessionMetadata.
func sessionMetadata(s Session) []byte {
	var b []byte
	b = protowire.AppendTag(b, fieldMetadataSessionID, protowire.BytesType)
	b = protowire.AppendString(b, s.ID)
	b = protowire.AppendTag(b, fieldMetadataGameID, protowire.BytesType)
	b = protowire.AppendString(b, s.GameID)
	return b
}

// widgetRequest encodes a GetCeeWebWidgetRequest.
func widgetRequest(s Session) []byte {
	b := protowire.AppendTag(nil, fieldRequestSession, protowire.BytesType)
	return protowire.AppendBytes(b, sessionMetadata(s))
}

// squareRequest encodes a GetSquareEventsRequest.
func squareRequest(s Session, gamePoint int64, square uint32) []byte {
	b := protowire.AppendTag(nil, fieldRequestSession, protowire.BytesType)
	b = protowire.AppendBytes(b, sessionMetadata(s))
	b = protowire.AppendTag(b, fieldRequestGamePoint, protowire.VarintType)
	b = protowire.AppendVarint(b, uint64(gamePoint))
	b = protowire.AppendTag(b, fieldRequestSquare, protowire.VarintType)
	return protowire.AppendVarint(b, uint64(square))
}

// widgetOf reads GetCeeWebWidgetResponse.widget's bytes. An absent field is an
// empty widget message, which encodes as no bytes.
func widgetOf(response []byte) ([]byte, error) {
	widget := []byte{}
	for len(response) > 0 {
		num, typ, n := protowire.ConsumeTag(response)
		if n < 0 {
			return nil, fmt.Errorf("%w: the widget answer is not a protobuf message: %v", errCall, protowire.ParseError(n))
		}
		response = response[n:]
		if num == fieldResponseWidget && typ == protowire.BytesType {
			value, m := protowire.ConsumeBytes(response)
			if m < 0 {
				return nil, fmt.Errorf("%w: the widget field is malformed: %v", errCall, protowire.ParseError(m))
			}
			widget = append([]byte{}, value...)
			response = response[m:]
			continue
		}
		m := protowire.ConsumeFieldValue(num, typ, response)
		if m < 0 {
			return nil, fmt.Errorf("%w: field %d is malformed: %v", errCall, num, protowire.ParseError(m))
		}
		response = response[m:]
	}
	return widget, nil
}

// connectError is the JSON body a Connect error answer carries.
type connectError struct {
	Code    string `json:"code"`
	Message string `json:"message"`
}

// call posts one unary request and answers the response bytes. A Connect
// failed_precondition is errSessionGone; every other failure is errCall.
func call(ctx context.Context, client *http.Client, baseURL, procedure string, request []byte) ([]byte, error) {
	req, err := http.NewRequestWithContext(ctx, http.MethodPost, baseURL+procedure, bytes.NewReader(request))
	if err != nil {
		return nil, fmt.Errorf("%w: build the request: %v", errCall, err)
	}
	req.Header.Set("Content-Type", "application/proto")
	req.Header.Set("Connect-Protocol-Version", "1")
	resp, err := client.Do(req)
	if err != nil {
		return nil, fmt.Errorf("%w: %v", errCall, err)
	}
	defer resp.Body.Close()
	body, err := io.ReadAll(resp.Body)
	if err != nil {
		return nil, fmt.Errorf("%w: read the answer: %v", errCall, err)
	}
	if resp.StatusCode == http.StatusOK {
		return body, nil
	}
	var refusal connectError
	if err := json.Unmarshal(body, &refusal); err != nil {
		return nil, fmt.Errorf("%w: HTTP %d with an unreadable body", errCall, resp.StatusCode)
	}
	if refusal.Code == "failed_precondition" {
		return nil, fmt.Errorf("%w: %s", errSessionGone, refusal.Message)
	}
	return nil, fmt.Errorf("%w: %s: %s", errCall, refusal.Code, refusal.Message)
}

// getWidget asks cee-webapp for a session's widget data.
func getWidget(ctx context.Context, client *http.Client, baseURL string, s Session) ([]byte, error) {
	response, err := call(ctx, client, baseURL, procGetCeeWebWidget, widgetRequest(s))
	if err != nil {
		return nil, err
	}
	return widgetOf(response)
}

// getSquareEvents asks cee-webapp what the engine says about one square, and
// answers the GetSquareEventsResponse whole.
func getSquareEvents(ctx context.Context, client *http.Client, baseURL string, s Session, gamePoint int64, square uint32) ([]byte, error) {
	return call(ctx, client, baseURL, procGetSquareEvents, squareRequest(s, gamePoint, square))
}
