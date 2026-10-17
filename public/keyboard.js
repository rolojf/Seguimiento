(() => {
  const addButtons = {
    a: "pending-add",
    o: "opportunity-add",
    r: "process-add",
  };
  let heldShortcut = null;

  // A document listener also covers an unfocused page and cancels browser shortcuts.
  // Clicking the existing buttons keeps all actions and saving rules in Elm.
  document.addEventListener("keydown", (event) => {
    if (event.defaultPrevented || !event.ctrlKey || event.altKey || event.metaKey || event.shiftKey || event.isComposing) return;

    // Holding a captured shortcut must not reload the page after the form gains focus.
    if (event.repeat && event.code === heldShortcut) {
      event.preventDefault();
      return;
    }

    const target = event.target;
    if (!(target instanceof HTMLElement) || target.isContentEditable || target.closest("input, textarea, select, form, [role='dialog']")) return;

    const buttonId = addButtons[event.key.toLowerCase()];
    if (!buttonId) return;

    const button = document.getElementById(buttonId);
    if (!(button instanceof HTMLButtonElement)) return;

    event.preventDefault();
    heldShortcut = event.code;
    if (event.repeat || button.disabled || button.closest("[inert]")) return;

    button.click();
  });

  document.addEventListener("keyup", (event) => {
    if (event.code === heldShortcut || event.key === "Control") heldShortcut = null;
  });
  window.addEventListener("blur", () => { heldShortcut = null; });
})();
