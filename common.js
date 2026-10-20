// Small helpers shared by the apps.

// n evenly spaced numbers from a to b (like R's seq(a, b, length.out = n)).
function seq(a, b, n) {
  const out = [];
  for (let i = 0; i < n; i++) {
    out.push(n === 1 ? a : a + (b - a) * i / (n - 1));
  }
  return out;
}

function sum(xs) {
  return xs.reduce((s, x) => s + x, 0);
}

function mean(xs) {
  return sum(xs) / xs.length;
}

// Standard normal random number (Box-Muller).
function rnorm() {
  const u = 1 - Math.random();
  const v = Math.random();
  return Math.sqrt(-2 * Math.log(u)) * Math.cos(2 * Math.PI * v);
}

// Sample quantile, same as R's default (type 7).
function quantile(xs, p) {
  const sorted = [...xs].sort((a, b) => a - b);
  const h = (sorted.length - 1) * p;
  const lo = Math.floor(h);
  const hi = Math.min(lo + 1, sorted.length - 1);
  return sorted[lo] + (h - lo) * (sorted[hi] - sorted[lo]);
}

// Histogram with "nice" bin widths, similar to R's hist(). The number of bins
// defaults to Sturges' rule. Returns bar centers and densities.
function histogram(xs, nbins) {
  nbins = nbins || Math.ceil(Math.log2(xs.length) + 1);
  const min = Math.min(...xs);
  const max = Math.max(...xs);
  const raw = (max - min) / nbins || 1;
  const mag = Math.pow(10, Math.floor(Math.log10(raw)));
  const width = [1, 2, 5, 10].map(f => f * mag).find(w => w >= raw);

  const start = Math.floor(min / width) * width;
  const count = Math.max(1, Math.ceil((max - start) / width));
  const counts = new Array(count).fill(0);
  for (const x of xs) {
    counts[Math.min(count - 1, Math.floor((x - start) / width))]++;
  }
  return {
    x: counts.map((_, i) => start + (i + 0.5) * width),
    y: counts.map(c => c / (xs.length * width)),
    width: width,
  };
}

// Show a slider's value next to its label and call onChange whenever it moves.
function bindSlider(id, onChange) {
  const input = document.getElementById(id);
  const output = document.getElementById(id + "-value");
  const update = () => {
    if (output) output.textContent = input.value;
    onChange();
  };
  input.addEventListener("input", update);
  if (output) output.textContent = input.value;
}

function value(id) {
  return Number(document.getElementById(id).value);
}

// Theme colors, read from the CSS custom properties in style.css.
function colors() {
  const style = getComputedStyle(document.documentElement);
  const get = name => style.getPropertyValue("--" + name).trim();
  return {
    surface: get("surface"),
    ink: get("ink"),
    ink2: get("ink-2"),
    muted: get("muted"),
    grid: get("grid"),
    axis: get("axis"),
    series1: get("series-1"),
    series2: get("series-2"),
    series3: get("series-3"),
  };
}

// Draw a Plotly chart with the shared look. Axis settings in `layout` are
// merged on top of the defaults; axis titles may be given as plain strings.
function plot(id, traces, layout, config) {
  const c = colors();
  const axisDefaults = {
    gridcolor: c.grid,
    linecolor: c.axis,
    showline: true,
    zeroline: false,
    ticks: "",
    automargin: true,
  };
  const full = {
    paper_bgcolor: "rgba(0,0,0,0)",
    plot_bgcolor: "rgba(0,0,0,0)",
    font: { family: "system-ui, -apple-system, 'Segoe UI', sans-serif", size: 12, color: c.muted },
    margin: { t: 8, r: 8, b: 8, l: 8 },
    showlegend: false,
    legend: { orientation: "h", x: 0, y: 1.02, yanchor: "bottom", font: { color: c.ink2, size: 13 } },
    hoverlabel: { bgcolor: c.surface, bordercolor: c.axis, font: { color: c.ink } },
    barcornerradius: 4,
    ...layout,
  };
  const axes = new Set(["xaxis", "yaxis", ...Object.keys(layout).filter(k => /^[xy]axis\d*$/.test(k))]);
  for (const key of axes) {
    const axis = { ...axisDefaults, ...(layout[key] || {}) };
    if (typeof axis.title === "string") {
      axis.title = { text: axis.title, font: { color: c.ink2, size: 13 } };
    }
    full[key] = axis;
  }
  Plotly.react(id, traces, full, { responsive: true, displayModeBar: false, ...config });
}

// Redraw when the system switches between light and dark mode.
function onThemeChange(redraw) {
  window.matchMedia("(prefers-color-scheme: dark)").addEventListener("change", redraw);
}
