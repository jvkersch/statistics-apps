# statistics-apps

Interactive apps to support the course on Probability and Statistics at GUGC,
South Korea. Each app is a self-contained HTML page with plain JavaScript; there
is no build step and no server-side code.

## Running locally

Serve the repository root with any static web server, e.g.

    python3 -m http.server

and open <http://localhost:8000>.

## Publishing

The workflow in `.github/workflows/pages.yml` publishes the repository root to
GitHub Pages on every push to `main`. To enable it, set
*Settings → Pages → Build and deployment → Source* to **GitHub Actions**.

## Adding an app

Create a folder with an `index.html`, link `../style.css` and `../common.js`
for the shared look and helpers, and add a card for it to the grid in
`index.html`.

## History

The apps started out in 2022 as R Shiny apps, written by hand and deployed as a
Shiny Server container on fly.io.

In 2026 the apps were ported to browser-only HTML and JavaScript. These
versions were generated with AI assistance (Claude), using the original Shiny
apps as the reference for their behaviour and data. They superseded the Shiny
apps, which were then removed from the repository; they remain available in
the git history.
