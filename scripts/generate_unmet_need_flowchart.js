const fs = require("fs");
const path = require("path");

const svg = `<?xml version="1.0" encoding="UTF-8"?>
<svg xmlns="http://www.w3.org/2000/svg" width="1200" height="760" viewBox="0 0 1200 760">
  <defs>
    <style>
      .box { fill: #f7f7f2; stroke: #2f2f2f; stroke-width: 2; }
      .accent { fill: #e8f0ea; }
      .text { font-family: "Times New Roman", serif; font-size: 18px; fill: #1f1f1f; }
      .small { font-size: 16px; }
      .title { font-size: 22px; font-weight: bold; }
      .arrow { stroke: #2f2f2f; stroke-width: 2; fill: none; marker-end: url(#arrowhead); }
    </style>
    <marker id="arrowhead" markerWidth="10" markerHeight="7" refX="10" refY="3.5" orient="auto">
      <polygon points="0 0, 10 3.5, 0 7" fill="#2f2f2f" />
    </marker>
  </defs>

  <rect x="30" y="20" width="1140" height="70" class="box accent" />
  <text x="600" y="60" text-anchor="middle" class="text title">Unmet need derivation flow (HNIR52 / HNIR62)</text>

  <rect x="60" y="130" width="320" height="90" class="box" />
  <text x="220" y="165" text-anchor="middle" class="text">Unfecund?</text>
  <text x="220" y="190" text-anchor="middle" class="text small">V215 (994/996) or V376==23</text>
  <text x="220" y="210" text-anchor="middle" class="text small">or V3A08D==1 or V602==6</text>

  <rect x="60" y="260" width="320" height="70" class="box" />
  <text x="220" y="302" text-anchor="middle" class="text">If yes: NA (excluded)</text>

  <rect x="440" y="130" width="320" height="90" class="box" />
  <text x="600" y="165" text-anchor="middle" class="text">Currently pregnant?</text>
  <text x="600" y="190" text-anchor="middle" class="text small">V213==1</text>

  <rect x="440" y="260" width="320" height="110" class="box" />
  <text x="600" y="295" text-anchor="middle" class="text">Pregnancy intention (V225)</text>
  <text x="600" y="318" text-anchor="middle" class="text small">Wanted then = 0</text>
  <text x="600" y="340" text-anchor="middle" class="text small">Wanted later = 1</text>
  <text x="600" y="362" text-anchor="middle" class="text small">Not at all = 2</text>

  <rect x="820" y="130" width="320" height="90" class="box" />
  <text x="980" y="165" text-anchor="middle" class="text">Non-pregnant &amp;</text>
  <text x="980" y="190" text-anchor="middle" class="text small">active or in union?</text>
  <text x="980" y="210" text-anchor="middle" class="text small">Sex in last 30 days or V501</text>

  <rect x="820" y="260" width="320" height="80" class="box" />
  <text x="980" y="295" text-anchor="middle" class="text">Modern method?</text>
  <text x="980" y="318" text-anchor="middle" class="text small">V313==3 => 0</text>

  <rect x="820" y="370" width="320" height="180" class="box" />
  <text x="980" y="405" text-anchor="middle" class="text">Fertility preference</text>
  <text x="980" y="428" text-anchor="middle" class="text small">V602==3 => 2 (limiting)</text>
  <text x="980" y="450" text-anchor="middle" class="text small">V602==2 => 1 (spacing)</text>
  <text x="980" y="472" text-anchor="middle" class="text small">V602==1 with V603 &lt;2y =&gt; 0</text>
  <text x="980" y="494" text-anchor="middle" class="text small">V602==1 with V603 &gt;=2y =&gt; 1</text>
  <text x="980" y="516" text-anchor="middle" class="text small">Unknown/after marriage => 1</text>

  <path d="M220 220 L220 260" class="arrow" />
  <path d="M380 175 L440 175" class="arrow" />
  <path d="M600 220 L600 260" class="arrow" />
  <path d="M760 175 L820 175" class="arrow" />
  <path d="M980 220 L980 260" class="arrow" />
  <path d="M980 340 L980 370" class="arrow" />

  <text x="400" y="165" class="text small">No</text>
  <text x="255" y="245" class="text small">Yes</text>
  <text x="720" y="165" class="text small">No</text>
  <text x="1010" y="245" class="text small">Yes</text>
  <text x="620" y="245" class="text small">Yes</text>
</svg>
`;

const outputPath = path.join(__dirname, "..", "docs", "unmet_need_flowchart.svg");
fs.writeFileSync(outputPath, svg, "utf8");
console.log(`Saved SVG to ${outputPath}`);
