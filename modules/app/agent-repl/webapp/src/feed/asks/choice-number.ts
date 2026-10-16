/**
 * choice-number — THE NUMERAL A CARD'S NUMBERED CHOICE IS DRAWN WITH.
 *
 * A standing question's options and a standing permission's buttons are both
 * numbered from 1 in drawn order, because a number typed into the editor's
 * empty composer answers the choice it names (agentrepl.v1.HostWorkspace.ask).
 * Both cards draw the numeral here, so the two never disagree about how a
 * number looks.
 */

/** The numeral prefix choice NUMBER is drawn with, `1.` for the first. */
export function choiceNumeral(number: number): string {
  return `${String(number)}.`;
}

/**
 * The numeral element for choice NUMBER: a span of CLASS NAME carrying the
 * number on ATTRIBUTE, so a test or a stylesheet reads the number without
 * parsing the drawn text.
 */
export function drawChoiceNumeral(number: number, className: string, attribute: string): HTMLElement {
  const numeral = document.createElement("span");
  numeral.className = className;
  numeral.setAttribute(attribute, String(number));
  numeral.textContent = choiceNumeral(number);
  return numeral;
}
