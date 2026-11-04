// Translates the fixed English texts of the file upload widgets:
// the "Browse..." button and "no file selected" placeholder of
// shiny.semantic::file_input(), and the "Finishing upload" / "Upload complete"
// text that Shiny's upload code writes into the progress block (shiny.semantic
// shows it in the progress ".label").
// The server sends the translated texts with send_file_input_labels() on
// every language change.
(function () {
  var english = {
    browse: "Browse...",
    complete: "Upload complete",
    finishing: "Finishing upload",
    placeholder: "no file selected"
  };
  var labels = null;
  // Any text we may find on the page (English or an earlier language) -> key
  var known = {};
  Object.keys(english).forEach(function (key) { known[english[key]] = key; });
  var observedProgress = typeof WeakSet !== "undefined" ? new WeakSet() : null;

  function translateProgress(progress) {
    progress.querySelectorAll(".bar, .label").forEach(function (el) {
      var key = known[el.textContent];
      if ((key === "complete" || key === "finishing") && el.textContent !== labels[key]) {
        el.textContent = labels[key];
      }
    });
  }

  function apply() {
    if (!labels) return;
    document.querySelectorAll(".ui-ss-input label.btn-file").forEach(function (label) {
      label.childNodes.forEach(function (node) {
        if (node.nodeType === 3 && known[node.textContent.trim()] === "browse") {
          node.textContent = " " + labels.browse + " ";
        }
      });
    });
    document.querySelectorAll(".ui-ss-input input[type=text][readonly]").forEach(function (input) {
      input.placeholder = labels.placeholder;
    });
    document.querySelectorAll(".ui-ss-progress-file").forEach(function (progress) {
      translateProgress(progress);
      // The progress text is written after the upload finishes.
      if (observedProgress && !observedProgress.has(progress)) {
        observedProgress.add(progress);
        new MutationObserver(function () { translateProgress(progress); })
          .observe(progress, { childList: true, characterData: true, subtree: true });
      }
    });
  }

  function register() {
    Shiny.addCustomMessageHandler("file-input-labels", function (message) {
      labels = message;
      Object.keys(message).forEach(function (key) { known[message[key]] = key; });
      apply();
    });
    // File inputs re-rendered later (e.g. the life table upload page is a
    // renderUI) arrive in English again.
    $(document).on("shiny:bound", function () { setTimeout(apply, 0); });
  }

  if (window.Shiny && Shiny.addCustomMessageHandler) {
    register();
  } else {
    document.addEventListener("DOMContentLoaded", register);
  }
})();
