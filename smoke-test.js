// Load every page in headless Chromium and fail on JavaScript errors or
// resources that fail to load (e.g. a broken CDN link).
//
//   npm install && npx playwright install chromium && npm test

const fs = require("fs");
const path = require("path");
const { chromium } = require("playwright");

const root = __dirname;
const pages = ["index.html", ...fs.readdirSync(root)
  .filter(d => fs.existsSync(path.join(root, d, "index.html")))
  .map(d => d + "/index.html")];

(async () => {
  const browser = await chromium.launch();
  let failed = false;
  for (const page of pages) {
    const tab = await browser.newPage();
    const errors = [];
    tab.on("pageerror", e => errors.push(e.message));
    tab.on("console", m => { if (m.type() === "error") errors.push(m.text()); });
    tab.on("requestfailed", r => errors.push("failed to load " + r.url()));
    await tab.goto("file://" + path.join(root, page), { waitUntil: "networkidle" });
    console.log((errors.length ? "FAIL " : "ok   ") + page);
    errors.forEach(e => console.log("     " + e));
    failed = failed || errors.length > 0;
    await tab.close();
  }
  await browser.close();
  process.exit(failed ? 1 : 0);
})();
