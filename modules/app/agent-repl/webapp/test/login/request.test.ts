// @vitest-environment jsdom
import { describe, expect, it } from "vitest";
import { answerLoginRequests, requestLogin } from "../../src/login/request.js";
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
