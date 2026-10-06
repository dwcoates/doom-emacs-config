/**
 * request — how a control OUTSIDE the topbar asks for the login flow.
 *
 * The page has ONE login overlay (`login.ts`, mounted in `main.ts`), opened by
 * the topbar's account cell through `agentrepl.v1.OpenLogin`. A feed row's
 * outcome marker offers the same "sign in" for an account cause (feed.proto
 * FeedOutcomeSignIn), and it must open THAT overlay rather than a second one:
 * so it raises this page-level request, and `main.ts` answers it with the
 * overlay's own `open`. One overlay, one request, whichever control asked.
 */
import { log } from "../log.js";

/** The page-level event a control raises to ask for the login flow. */
export const LOGIN_REQUESTED_EVENT = "login-requested";

/** The event's detail: the control the reader clicked, for any refusal. */
export interface LoginRequestedDetail {
  control: HTMLElement;
}

/**
 * Ask the page's login overlay to open, with CONTROL as the element any
 * refusal is drawn at. A page with no overlay listening is a mount defect,
 * recorded at error rather than ignored.
 */
export function requestLogin(control: HTMLElement): void {
  const event = new CustomEvent<LoginRequestedDetail>(LOGIN_REQUESTED_EVENT, {
    detail: { control },
    cancelable: true,
  });
  const unanswered = document.dispatchEvent(event);
  if (unanswered) {
    log.error("a control asked for the login flow and no login overlay answered", {
      operation: "login.request-unanswered",
    });
  }
}

/**
 * Answer every login request with OPEN until the returned function is called.
 * The answer cancels the event, which is how the requester knows it landed.
 */
export function answerLoginRequests(open: (control: HTMLElement) => void): () => void {
  const onRequest = (event: Event): void => {
    event.preventDefault();
    open((event as CustomEvent<LoginRequestedDetail>).detail.control);
  };
  document.addEventListener(LOGIN_REQUESTED_EVENT, onRequest);
  return () => document.removeEventListener(LOGIN_REQUESTED_EVENT, onRequest);
}
