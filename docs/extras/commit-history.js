(function () {
  const USER   = "olicoside";
  const REPO   = "Scalable_toolbox_online_supplement";
  const BRANCH = "main";
  const MAX    = 5;

  // Québec time (handles DST). Use "fr-CA" for French formatting if you want.
  const TZ = "America/Toronto";
  const LOCALE = undefined; // e.g., "fr-CA" for French Canada

  const box = document.getElementById("commit-history");
  if (!box) return;

  // Map /docs/foo.html -> foo ; strip repo slug if present on GH Pages
  let p = window.location.pathname.replace(/^\/+/, "");
  p = p.replace(new RegExp("^" + REPO + "/", "i"), "");
  if (p === "" || p.endsWith("/")) p += "index.html";
  p = p.replace(/^docs\//, "");
  const stem = p.replace(/\.html?$/i, "");

  // Try common source roots/extensions
  const SRC_DIRS = ["", "ebook/"];
  const EXTS = [".qmd", ".Rmd", ".rmd", ".rmarkdown", ".md"];
  const candidates = [];
  for (const dir of SRC_DIRS) for (const ext of EXTS) candidates.push(dir + stem + ext);

  (async function run() {
    for (const path of candidates) {
      const url = `https://api.github.com/repos/${USER}/${REPO}/commits?path=${encodeURIComponent(path)}&sha=${BRANCH}&per_page=${MAX}`;
      try {
        const r = await fetch(url);
        const data = await r.json();
        if (!Array.isArray(data) || data.message) continue;

        if (data.length) {
          box.innerHTML = data.map(c => {
            const msg = (c.commit.message || "").split("\n")[0]; // first line only
            const whenISO = c.commit.author?.date || c.commit.committer?.date;
            const when = new Date(whenISO);
            const whenStr = when.toLocaleString(LOCALE, {
              dateStyle: "medium",
              timeStyle: "medium",      // includes seconds
              hour12: false,
              timeZone: TZ
            });
            const sha = c.sha.slice(0, 7);
            const href = c.html_url;

            return `<div class="commit">
                      <div class="commit-head">
                        <a class="sha-link" href="${href}" title="View commit ${sha} on GitHub">${sha}</a>
                        <span class="msg">${msg}</span>
                      </div>
                      <div class="muted">${whenStr}</div>
                    </div>`;
          }).join("") +
          `<div class="muted" style="margin-top:.4rem">
             <a class="sha-link" href="https://github.com/${USER}/${REPO}/commits/${BRANCH}/${encodeURIComponent(candidates[0])}">
               Full history
             </a>
           </div>`;
          return;
        }
      } catch (_) {}
    }
    box.textContent = "No history found.";
  })();
})();
