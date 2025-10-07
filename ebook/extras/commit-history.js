(function () {
  const USER   = "olicoside";
  const REPO   = "Scalable_toolbox_online_supplement";
  const BRANCH = "main";
  const MAX    = 5;
  const TZ     = undefined; // e.g., "America/Toronto" if you want a fixed zone

  const box = document.getElementById("commit-history");
  if (!box) return;

  // Map /docs/foo.html -> foo ; strip repo slug if present
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

  (async function tryFetch() {
    for (const path of candidates) {
      const url = `https://api.github.com/repos/${USER}/${REPO}/commits?path=${encodeURIComponent(path)}&sha=${BRANCH}&per_page=${MAX}`;
      try {
        const r = await fetch(url);
        const data = await r.json();
        if (!Array.isArray(data) || data.message) continue;

        if (data.length) {
          box.innerHTML = data.map(c => {
            const whenISO = c.commit.author?.date || c.commit.committer?.date;
            const when = new Date(whenISO);
            const whenStr = when.toLocaleString(undefined, {
              dateStyle: "medium",
              timeStyle: "medium",   // includes seconds
              hour12: false,
              ...(TZ ? { timeZone: TZ } : {})
            });
            const sha  = c.sha.slice(0, 7);
            const url  = c.html_url;

            // Minimal line: timestamp • sha — GitHub (pale link)
            return `<div class="commit">
                      <div class="muted">
                        ${whenStr} • ${sha}
                        <a class="gh-link" href="${url}" aria-label="View commit on GitHub">GitHub</a>
                      </div>
                    </div>`;
          }).join("") +
          `<div class="muted" style="margin-top:.35rem">
             <a class="gh-link" href="https://github.com/${USER}/${REPO}/commits/${BRANCH}/${encodeURIComponent(candidates[0])}">
               Full history
             </a>
           </div>`;
          return;
        }
      } catch (_) { /* try next candidate */ }
    }
    box.textContent = "No history found.";
  })();
})();
