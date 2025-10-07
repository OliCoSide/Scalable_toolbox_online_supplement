(function () {
  const USER   = "olicoside";       // e.g., "olicoside"
  const REPO   = "Scalable_toolbox_online_supplement";          // e.g., "Scalable_toolbox_online_supplement"
  const BRANCH = "main";
  const MAX    = 5;

  const box = document.getElementById("commit-history");
  if (!box) return;

  // Map current URL → source path (docs/foo/bar.html → foo/bar.qmd)
  const slug = REPO.toLowerCase();
  let p = window.location.pathname.replace(/^\/+/, "");

  // Strip "/REPO/" prefix on project pages (no effect on user/organization pages)
  p = p.replace(new RegExp("^" + slug + "/"), "");

  // Default to index.html
  if (p === "" || p.endsWith("/")) p += "index.html";

  // Remove docs/ and switch extension
  p = p.replace(/^docs\//, "");
  const candidates = [
    p.replace(/\.html?$/i, ".qmd"),
    p.replace(/\.html?$/i, ".Rmd"),
    p.replace(/\.html?$/i, ".rmd"),
    p.replace(/\.html?$/i, ".rmarkdown"),
    p.replace(/\.html?$/i, ".md"),
  ];

  // Try candidates until we get commits
  (async function tryFetch() {
    for (const path of candidates) {
      const url = `https://api.github.com/repos/${USER}/${REPO}/commits?path=${encodeURIComponent(path)}&sha=${BRANCH}&per_page=${MAX}`;
      try {
        const r = await fetch(url);
        const data = await r.json();
        if (Array.isArray(data) && data.length) {
          box.innerHTML = data.map(c => {
            const msg  = (c.commit.message || "").split("\n")[0];
            const date = new Date(c.commit.author.date).toLocaleDateString();
            const sha  = c.sha.slice(0, 7);
            return `<div class="commit">
                      <a href="${c.html_url}">${msg}</a>
                      <div class="muted">${date} • ${sha}</div>
                    </div>`;
          }).join("") + 
          `<div class="muted" style="margin-top:.25rem">
             <a href="https://github.com/${USER}/${REPO}/commits/${BRANCH}/${encodeURIComponent(path)}">Full history</a>
           </div>`;
          return;
        }
      } catch (_) {}
    }
    box.textContent = "No history found.";
  })();
})();
