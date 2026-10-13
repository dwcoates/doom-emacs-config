// @vitest-environment jsdom
import { describe, expect, it } from "vitest";
import {
  SIGN_IN_ACTION,
  SIGN_IN_ACTION_ATTRIBUTE,
  answerLoginRequests,
  drawSignInAction,
  requestLogin,
} from "../../src/login/request.js";
import { captureLogRecords, forwardedRecord } from "../log-capture.js";

describe("requestLogin", () => {
  it("hands the clicked control to the page's login overlay", () => {
    // Arrange
    const opened: HTMLElement[] = [];
    const stop = answerLoginRequests((control) => opened.push(control));
    const control = document.createElement("span");
    // Act
    requestLogin(control);
    stop();
    // Assert
    expect(opened).toEqual([control]);
  });

  it("stops answering once the answer is withdrawn", () => {
    // Arrange
    const opened: HTMLElement[] = [];
    answerLoginRequests((control) => opened.push(control))();
    // Act
    requestLogin(document.createElement("span"));
    // Assert
    expect(opened).toEqual([]);
  });

  it("records a request no overlay answered at error", async () => {
    // Arrange
    const capture = captureLogRecords();
    // Act
    requestLogin(document.createElement("span"));
    // Assert
    const record = await forwardedRecord(capture, "login.request-unanswered");
    expect(record.level.case).toBe("error");
  });
});

describe("drawSignInAction", () => {
  it("is a control labelled sign in, marked as the sign-in action, in the surface's class", () => {
    // Act
    const action = drawSignInAction("some-surface-action", "test.sign-in");
    // Assert
    expect([action.localName, action.className, action.getAttribute(SIGN_IN_ACTION_ATTRIBUTE), action.textContent]).toEqual([
      "ar-button",
      "some-surface-action",
      SIGN_IN_ACTION,
      "sign in",
    ]);
  });

  it("opens the page's login overlay with itself as the control", () => {
    // Arrange
    const opened: HTMLElement[] = [];
    const stop = answerLoginRequests((control) => opened.push(control));
    const action = drawSignInAction("some-surface-action", "test.sign-in");
    // Act
    action.click();
    stop();
    // Assert
    expect(opened).toEqual([action]);
  });

  it("records the click under the surface's operation", async () => {
    // Arrange
    const capture = captureLogRecords();
    const stop = answerLoginRequests(() => undefined);
    const action = drawSignInAction("some-surface-action", "test.sign-in");
    // Act
    action.click();
    stop();
    // Assert
    const record = await forwardedRecord(capture, "test.sign-in");
    expect(record.level.case).toBe("info");
  });
});
