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
import { createControl } from "../control.js";
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

/** The attribute and value every sign-in action carries, wherever it is drawn. */
export const SIGN_IN_ACTION_ATTRIBUTE = "data-action";
export const SIGN_IN_ACTION = "sign-in";

/**
 * A "sign in" action: the one control every feed surface offering the sign-in
 * draws (an outcome marker's expansion, a broken response bubble), so they
 * cannot drift apart. CLASSNAME is the surface's own class for it; OPERATION
 * is the record its click is logged under. Its click asks the page's one login
 * overlay to open (`requestLogin`) and goes no further: the bubble or row it
 * sits inside never takes the click as its own (a bubble's click selects it).
 */
export function drawSignInAction(className: string, operation: string): HTMLElement {
  const button = createControl();
  button.className = className;
  button.setAttribute(SIGN_IN_ACTION_ATTRIBUTE, SIGN_IN_ACTION);
  button.textContent = "sign in";
  button.addEventListener("click", (event) => {
    event.preventDefault();
    event.stopPropagation();
    log.info("the reader asked to sign in from the feed", { operation });
    requestLogin(button);
  });
  return button;
}
