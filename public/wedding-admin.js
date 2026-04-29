(function () {
  function esc(value) {
    return String(value == null ? "" : value).replace(/[&<>"']/g, function (c) {
      return { "&": "&amp;", "<": "&lt;", ">": "&gt;", "\"": "&quot;", "'": "&#39;" }[c];
    });
  }

  function date(value) {
    return esc(String(value || "").replace("T", " ").slice(0, 19));
  }

  function api(path, options) {
    options = options || {};
    options.credentials = "same-origin";
    return fetch(path, options);
  }

  function jsonApi(path, options) {
    return api(path, options).then(function (response) {
      if (!response.ok) throw new Error(String(response.status));
      return response.json();
    });
  }

  function isAdminRoute() {
    return window.location.hash === "#admin" || window.location.hash === "#/admin";
  }

  function initAdmin(root) {
    if (window.__weddingAdminReady) return;
    window.__weddingAdminReady = true;

    var state = { tab: "invitees", invitees: [], rsvps: [], videos: [] };

    function syncRoute() {
      document.body.classList.toggle("admin-mode", isAdminRoute());
      if (isAdminRoute()) checkSession();
    }

    function checkSession() {
      api("/api/admin/me")
        .then(function (response) {
          if (response.ok) {
            renderShell();
            loadAll();
          } else {
            renderLogin("");
          }
        })
        .catch(function () { renderLogin(""); });
    }

    function renderLogin(message) {
      root.innerHTML =
        '<main class="admin-page"><section class="admin-login">' +
        '<p class="admin-kicker">ADMIN</p><h1>Wedding dashboard</h1>' +
        '<p class="admin-muted">Enter the admin password to manage invitees, RSVPs, and videos.</p>' +
        '<form id="admin-login-form"><input id="admin-password" class="admin-input" type="password" placeholder="Password" autocomplete="current-password" autofocus>' +
        '<button class="admin-btn" type="submit">Enter</button><p class="admin-error">' + esc(message) + "</p></form>" +
        "</section></main>";

      document.getElementById("admin-login-form").addEventListener("submit", function (event) {
        event.preventDefault();
        var password = document.getElementById("admin-password").value;
        api("/api/admin/login", {
          method: "POST",
          headers: { "Content-Type": "application/json" },
          body: JSON.stringify({ password: password })
        })
          .then(function (response) {
            if (!response.ok) throw new Error("bad login");
            renderShell();
            loadAll();
          })
          .catch(function () { renderLogin("Invalid password."); });
      });
    }

    function renderShell() {
      root.innerHTML =
        '<main class="admin-page"><header class="admin-top"><div>' +
        '<p class="admin-kicker">ADMIN</p><h1>Wedding dashboard</h1></div>' +
        '<div class="admin-actions"><a class="admin-link" href="#hero">Public site</a>' +
        '<button id="admin-logout" class="admin-btn ghost" type="button">Log out</button></div></header>' +
        '<nav class="admin-tabs"><button data-tab="invitees">Invitees</button><button data-tab="rsvps">RSVPs</button><button data-tab="videos">Videos</button></nav>' +
        '<section id="admin-panel" class="admin-panel"></section></main>';

      document.getElementById("admin-logout").addEventListener("click", function () {
        api("/api/admin/logout", { method: "POST" }).finally(function () { renderLogin(""); });
      });

      root.querySelectorAll("[data-tab]").forEach(function (button) {
        button.addEventListener("click", function () {
          state.tab = button.getAttribute("data-tab");
          renderPanel();
        });
      });
      renderPanel();
    }

    function loadAll() {
      Promise.all([
        jsonApi("/api/admin/invitees"),
        jsonApi("/api/admin/rsvps"),
        jsonApi("/api/admin/videos")
      ])
        .then(function (items) {
          state.invitees = items[0];
          state.rsvps = items[1];
          state.videos = items[2];
          renderPanel();
        })
        .catch(function () { renderLogin("Session expired."); });
    }

    function setTabs() {
      root.querySelectorAll("[data-tab]").forEach(function (button) {
        button.classList.toggle("active", button.getAttribute("data-tab") === state.tab);
      });
    }

    function renderPanel() {
      var panel = document.getElementById("admin-panel");
      if (!panel) return;
      setTabs();
      if (state.tab === "invitees") renderInvitees(panel);
      else if (state.tab === "rsvps") renderRsvps(panel);
      else renderVideos(panel);
    }

    function renderInvitees(panel) {
      panel.innerHTML =
        '<div class="admin-grid"><form id="invitee-form" class="admin-card admin-form"><h2>Add invitee</h2>' +
        '<input class="admin-input" name="name" placeholder="Name" required>' +
        '<input class="admin-input" name="code" placeholder="Invitation code (optional)">' +
        '<input class="admin-input" name="maxGuests" type="number" min="1" max="20" value="1">' +
        '<textarea class="admin-input" name="notes" placeholder="Notes"></textarea>' +
        '<button class="admin-btn" type="submit">Add invitee</button></form>' +
        '<div class="admin-card"><h2>Invitees (' + state.invitees.length + ')</h2><div class="admin-list">' +
        state.invitees.map(function (invitee) {
          return '<article class="admin-row"><div><strong>' + esc(invitee.name) + '</strong>' +
            '<p>' + esc(invitee.code || "no code") + ' - max ' + esc(invitee.maxGuests) + ' guests</p>' +
            '<p>' + esc(invitee.notes || "") + '</p></div>' +
            '<button class="admin-danger" data-delete="' + esc(invitee.id) + '">Delete</button></article>';
        }).join("") +
        '</div></div></div>';

      document.getElementById("invitee-form").addEventListener("submit", function (event) {
        event.preventDefault();
        var form = event.currentTarget;
        var body = {
          name: form.name.value,
          code: form.code.value || null,
          maxGuests: parseInt(form.maxGuests.value || "1", 10),
          notes: form.notes.value || null
        };
        api("/api/admin/invitees", {
          method: "POST",
          headers: { "Content-Type": "application/json" },
          body: JSON.stringify(body)
        })
          .then(function (response) {
            if (!response.ok) throw new Error("bad create");
            return response.json();
          })
          .then(function (invitee) {
            state.invitees.unshift(invitee);
            renderPanel();
          })
          .catch(function () { alert("Could not create invitee."); });
      });

      panel.querySelectorAll("[data-delete]").forEach(function (button) {
        button.addEventListener("click", function () {
          if (!confirm("Delete invitee?")) return;
          var id = button.getAttribute("data-delete");
          api("/api/admin/invitees/" + encodeURIComponent(id), { method: "DELETE" })
            .then(function (response) {
              if (!response.ok) throw new Error("bad delete");
              state.invitees = state.invitees.filter(function (invitee) { return String(invitee.id) !== String(id); });
              renderPanel();
            })
            .catch(function () { alert("Could not delete invitee."); });
        });
      });
    }

    function renderRsvps(panel) {
      panel.innerHTML = '<div class="admin-card"><h2>RSVPs (' + state.rsvps.length + ')</h2><div class="admin-list">' +
        state.rsvps.map(function (rsvp) {
          return '<article class="admin-row"><div><strong>' + esc(rsvp.name) + '</strong>' +
            '<p>' + esc(rsvp.guestCount) + ' guests - ' + date(rsvp.createdAt) + '</p>' +
            '<p>Invitee: ' + esc(rsvp.inviteeId || "unmatched") + ' - Code: ' + esc(rsvp.invitationCodeUsed || "none") + '</p>' +
            '<p>' + esc(rsvp.dietary || "") + '</p></div></article>';
        }).join("") + '</div></div>';
    }

    function renderVideos(panel) {
      panel.innerHTML = '<div class="admin-card"><h2>Videos (' + state.videos.length + ')</h2><div class="admin-list">' +
        state.videos.map(function (video) {
          return '<article class="admin-row"><div><strong>' + esc(video.originalFilename) + '</strong>' +
            '<p>' + esc(video.contentType) + ' - ' + (Math.round((video.sizeBytes || 0) / 1048576 * 10) / 10) + ' MB - ' + date(video.createdAt) + '</p>' +
            '<p>' + esc(video.submitterName || "anonymous") + ' ' + esc(video.message || "") + '</p></div>' +
            '<a class="admin-btn small" href="/api/admin/videos/' + encodeURIComponent(video.id) + '/download">Download</a></article>';
        }).join("") + '</div></div>';
    }

    window.addEventListener("hashchange", syncRoute);
    syncRoute();
  }

  function initVideoUpload(form) {
    if (window.__weddingVideoUploadReady) return;
    window.__weddingVideoUploadReady = true;

    var status = document.getElementById("video-upload-status");
    var submit = document.getElementById("video-upload-submit");

    function setStatus(message, bad) {
      status.textContent = message || "";
      status.classList.toggle("is-error", !!bad);
    }

    form.addEventListener("submit", function (event) {
      event.preventDefault();
      var file = document.getElementById("video-upload-file").files[0];
      if (!file) {
        setStatus("Choose a video first.", true);
        return;
      }
      var data = new FormData();
      data.append("name", document.getElementById("video-upload-name").value || "");
      data.append("message", document.getElementById("video-upload-message").value || "");
      data.append("video", file, file.name);
      submit.disabled = true;
      setStatus("Uploading... this can take a moment.", false);
      fetch("/api/videos", { method: "POST", body: data })
        .then(function (response) {
          if (!response.ok) throw new Error(String(response.status));
          return response.json();
        })
        .then(function () {
          form.reset();
          setStatus("Video uploaded. Thank you!", false);
        })
        .catch(function () { setStatus("Upload failed. Try again with a smaller video.", true); })
        .finally(function () { submit.disabled = false; });
    });
  }

  function start() {
    var root = document.getElementById("admin-root");
    var form = document.getElementById("video-upload-form");
    if (root) initAdmin(root);
    if (form) initVideoUpload(form);
    return !!(root && form);
  }

  if (!start()) {
    var tries = 0;
    var timer = window.setInterval(function () {
      if (start() || ++tries > 200) window.clearInterval(timer);
    }, 50);
  }
})();
