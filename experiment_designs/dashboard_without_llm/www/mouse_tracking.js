// Tracks mouse movement over the survey panel and forwards throttled
// samples to Shiny as `input.mouse_move`. Positions are relative to the
// survey container's own top-left corner (plus the container's current
// size), so they can be re-normalized to 0-1 on the R side later.

(function () {
  var CONTAINER_ID = "survey_container";
  var THROTTLE_MS = 200;

  function initMouseTracking() {
    var container = document.getElementById(CONTAINER_ID);
    if (!container) return;

    var lastSent = 0;

    container.addEventListener("mousemove", function (e) {
      var now = Date.now();
      if (now - lastSent < THROTTLE_MS) return;
      lastSent = now;

      var rect = container.getBoundingClientRect();

      Shiny.setInputValue(
        "mouse_move",
        {
          x: Math.round(e.clientX - rect.left),
          y: Math.round(e.clientY - rect.top),
          width: Math.round(rect.width),
          height: Math.round(rect.height),
          client_x: e.clientX,
          client_y: e.clientY,
          client_ts: now
        },
        { priority: "event" }
      );
    });

    container.addEventListener("mouseenter", function () {
      Shiny.setInputValue(
        "mouse_hover",
        { hovering: true, client_ts: Date.now() },
        { priority: "event" }
      );
    });

    container.addEventListener("mouseleave", function () {
      Shiny.setInputValue(
        "mouse_hover",
        { hovering: false, client_ts: Date.now() },
        { priority: "event" }
      );
    });
  }

  $(document).on("shiny:connected", initMouseTracking);
})();
