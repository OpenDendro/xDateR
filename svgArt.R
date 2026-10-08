# Decorative SVG art for the welcome states of the Overview and Floater
# panels. Kept out of server.R so the logic there stays readable.

welcomeSVG <- '
  <svg width="100%" viewBox="0 0 680 420" xmlns="http://www.w3.org/2000/svg">
    <g opacity="0.18">
      <circle cx="0" cy="210" r="390" fill="none" stroke="#2C5F2E" stroke-width="18"/>
      <circle cx="0" cy="210" r="360" fill="none" stroke="#2C5F2E" stroke-width="10"/>
      <circle cx="0" cy="210" r="336" fill="none" stroke="#2C5F2E" stroke-width="20"/>
      <circle cx="0" cy="210" r="308" fill="none" stroke="#2C5F2E" stroke-width="8"/>
      <circle cx="0" cy="210" r="288" fill="none" stroke="#2C5F2E" stroke-width="22"/>
      <circle cx="0" cy="210" r="256" fill="none" stroke="#2C5F2E" stroke-width="12"/>
      <circle cx="0" cy="210" r="232" fill="none" stroke="#2C5F2E" stroke-width="16"/>
      <circle cx="0" cy="210" r="206" fill="none" stroke="#2C5F2E" stroke-width="8"/>
      <circle cx="0" cy="210" r="188" fill="none" stroke="#2C5F2E" stroke-width="24"/>
      <circle cx="0" cy="210" r="154" fill="none" stroke="#2C5F2E" stroke-width="10"/>
      <circle cx="0" cy="210" r="134" fill="none" stroke="#2C5F2E" stroke-width="18"/>
      <circle cx="0" cy="210" r="106" fill="none" stroke="#2C5F2E" stroke-width="8"/>
      <circle cx="0" cy="210" r="88"  fill="none" stroke="#2C5F2E" stroke-width="20"/>
      <circle cx="0" cy="210" r="60"  fill="none" stroke="#2C5F2E" stroke-width="14"/>
      <circle cx="0" cy="210" r="38"  fill="none" stroke="#2C5F2E" stroke-width="10"/>
      <circle cx="0" cy="210" r="20"  fill="#2C5F2E" opacity="0.5"/>
    </g>
    <line x1="0" y1="210" x2="420" y2="210" stroke="#2C5F2E" stroke-width="0.5"
          opacity="0.5" stroke-dasharray="4 4"/>
    <text x="340" y="110" text-anchor="middle" font-family="sans-serif"
          font-size="32" font-weight="500" fill="#2C5F2E" opacity="0.9">xDateR</text>
    <text x="340" y="148" text-anchor="middle" font-family="sans-serif"
          font-size="14" fill="#666">Statistical crossdating for tree-ring data</text>
    <line x1="280" y1="168" x2="400" y2="168" stroke="#ccc" stroke-width="0.5"/>
    <circle cx="252" cy="210" r="14" fill="#2C5F2E" opacity="0.15"/>
    <text x="252" y="215" text-anchor="middle" font-family="sans-serif"
          font-size="13" font-weight="500" fill="#2C5F2E">1</text>
    <text x="276" y="207" font-family="sans-serif" font-size="13"
          font-weight="500" fill="#333">Upload a ring-width file</text>
    <text x="276" y="222" font-family="sans-serif" font-size="12"
          fill="#888">Tucson, Heidelberg, compact, TRiDaS, or .csv</text>
    <circle cx="252" cy="262" r="14" fill="#2C5F2E" opacity="0.15"/>
    <text x="252" y="267" text-anchor="middle" font-family="sans-serif"
          font-size="13" font-weight="500" fill="#2C5F2E">2</text>
    <text x="276" y="259" font-family="sans-serif" font-size="13"
          font-weight="500" fill="#333">Inspect and crossdate</text>
    <text x="276" y="274" font-family="sans-serif" font-size="12"
          fill="#888">Correlations, segment analysis, skeleton plots</text>
    <circle cx="252" cy="314" r="14" fill="#2C5F2E" opacity="0.15"/>
    <text x="252" y="319" text-anchor="middle" font-family="sans-serif"
          font-size="13" font-weight="500" fill="#2C5F2E">3</text>
    <text x="276" y="311" font-family="sans-serif" font-size="13"
          font-weight="500" fill="#333">Edit and export</text>
    <text x="276" y="326" font-family="sans-serif" font-size="12"
          fill="#888">Insert or delete rings, download corrected .rwl</text>
    <rect x="208" y="356" width="264" height="32" rx="6"
          fill="#2C5F2E" opacity="0.08"/>
    <text x="340" y="377" text-anchor="middle" font-family="sans-serif"
          font-size="12" fill="#2C5F2E">Or try the example data from the sidebar</text>
  </svg>
'

floaterSVG <- '
  <svg width="100%" viewBox="0 0 680 420" xmlns="http://www.w3.org/2000/svg">
    <defs>
      <marker id="arr-green" viewBox="0 0 10 10" refX="8" refY="5"
              markerWidth="5" markerHeight="5" orient="auto">
        <path d="M2 1L8 5L2 9" fill="none" stroke="#2C5F2E"
              stroke-width="1.5" opacity="0.5"/>
      </marker>
    </defs>
    <text x="340" y="50" text-anchor="middle" font-family="sans-serif"
          font-size="28" font-weight="500" fill="#2C5F2E" opacity="0.9">Floater</text>
    <text x="340" y="72" text-anchor="middle" font-family="sans-serif"
          font-size="13" fill="#888">Date an undated or misdated series against a master chronology</text>
    <text x="40" y="90" font-family="sans-serif" font-size="12" fill="#888">Master chronology (dated)</text>
    <rect x="40" y="100" width="600" height="18" rx="3" fill="#2C5F2E" opacity="0.15"/>
    <line x1="80"  y1="100" x2="80"  y2="118" stroke="#2C5F2E" stroke-width="1.5" opacity="0.4"/>
    <line x1="106" y1="100" x2="106" y2="118" stroke="#2C5F2E" stroke-width="2.5" opacity="0.5"/>
    <line x1="128" y1="100" x2="128" y2="118" stroke="#2C5F2E" stroke-width="1"   opacity="0.4"/>
    <line x1="152" y1="100" x2="152" y2="118" stroke="#2C5F2E" stroke-width="3"   opacity="0.5"/>
    <line x1="172" y1="100" x2="172" y2="118" stroke="#2C5F2E" stroke-width="1.5" opacity="0.4"/>
    <line x1="198" y1="100" x2="198" y2="118" stroke="#2C5F2E" stroke-width="2"   opacity="0.5"/>
    <line x1="220" y1="100" x2="220" y2="118" stroke="#2C5F2E" stroke-width="1"   opacity="0.4"/>
    <line x1="244" y1="100" x2="244" y2="118" stroke="#2C5F2E" stroke-width="2.5" opacity="0.5"/>
    <line x1="268" y1="100" x2="268" y2="118" stroke="#2C5F2E" stroke-width="1.5" opacity="0.4"/>
    <line x1="290" y1="100" x2="290" y2="118" stroke="#2C5F2E" stroke-width="1"   opacity="0.5"/>
    <line x1="314" y1="100" x2="314" y2="118" stroke="#2C5F2E" stroke-width="3"   opacity="0.4"/>
    <line x1="338" y1="100" x2="338" y2="118" stroke="#2C5F2E" stroke-width="1.5" opacity="0.5"/>
    <line x1="360" y1="100" x2="360" y2="118" stroke="#2C5F2E" stroke-width="2"   opacity="0.4"/>
    <line x1="384" y1="100" x2="384" y2="118" stroke="#2C5F2E" stroke-width="1"   opacity="0.5"/>
    <line x1="406" y1="100" x2="406" y2="118" stroke="#2C5F2E" stroke-width="2.5" opacity="0.4"/>
    <line x1="430" y1="100" x2="430" y2="118" stroke="#2C5F2E" stroke-width="1.5" opacity="0.5"/>
    <line x1="452" y1="100" x2="452" y2="118" stroke="#2C5F2E" stroke-width="1"   opacity="0.4"/>
    <line x1="476" y1="100" x2="476" y2="118" stroke="#2C5F2E" stroke-width="2"   opacity="0.5"/>
    <line x1="500" y1="100" x2="500" y2="118" stroke="#2C5F2E" stroke-width="3"   opacity="0.4"/>
    <line x1="522" y1="100" x2="522" y2="118" stroke="#2C5F2E" stroke-width="1.5" opacity="0.5"/>
    <line x1="548" y1="100" x2="548" y2="118" stroke="#2C5F2E" stroke-width="1"   opacity="0.4"/>
    <line x1="572" y1="100" x2="572" y2="118" stroke="#2C5F2E" stroke-width="2.5" opacity="0.5"/>
    <line x1="596" y1="100" x2="596" y2="118" stroke="#2C5F2E" stroke-width="1.5" opacity="0.4"/>
    <line x1="40" y1="122" x2="640" y2="122" stroke="#ccc" stroke-width="0.5"/>
    <text x="40"  y="134" font-family="sans-serif" font-size="11" fill="#aaa" text-anchor="middle">1200</text>
    <text x="190" y="134" font-family="sans-serif" font-size="11" fill="#aaa" text-anchor="middle">1400</text>
    <text x="340" y="134" font-family="sans-serif" font-size="11" fill="#aaa" text-anchor="middle">1600</text>
    <text x="490" y="134" font-family="sans-serif" font-size="11" fill="#aaa" text-anchor="middle">1800</text>
    <text x="640" y="134" font-family="sans-serif" font-size="11" fill="#aaa" text-anchor="middle">2000</text>
    <text x="268" y="162" font-family="sans-serif" font-size="12" fill="#888">Undated series — sliding to find best fit</text>
    <rect x="268" y="172" width="220" height="18" rx="3" fill="#2C5F2E" opacity="0.35"/>
    <line x1="290" y1="172" x2="290" y2="190" stroke="#2C5F2E" stroke-width="1"   opacity="0.7"/>
    <line x1="314" y1="172" x2="314" y2="190" stroke="#2C5F2E" stroke-width="3"   opacity="0.8"/>
    <line x1="338" y1="172" x2="338" y2="190" stroke="#2C5F2E" stroke-width="1.5" opacity="0.7"/>
    <line x1="360" y1="172" x2="360" y2="190" stroke="#2C5F2E" stroke-width="2"   opacity="0.8"/>
    <line x1="384" y1="172" x2="384" y2="190" stroke="#2C5F2E" stroke-width="1"   opacity="0.7"/>
    <line x1="406" y1="172" x2="406" y2="190" stroke="#2C5F2E" stroke-width="2.5" opacity="0.8"/>
    <line x1="430" y1="172" x2="430" y2="190" stroke="#2C5F2E" stroke-width="1.5" opacity="0.7"/>
    <line x1="452" y1="172" x2="452" y2="190" stroke="#2C5F2E" stroke-width="1"   opacity="0.8"/>
    <line x1="476" y1="172" x2="476" y2="190" stroke="#2C5F2E" stroke-width="2"   opacity="0.7"/>
    <line x1="248" y1="181" x2="264" y2="181" stroke="#2C5F2E" stroke-width="1.5"
          opacity="0.5" stroke-dasharray="3 2" marker-end="url(#arr-green)"/>
    <line x1="492" y1="181" x2="508" y2="181" stroke="#2C5F2E" stroke-width="1.5"
          opacity="0.5" stroke-dasharray="3 2" marker-end="url(#arr-green)"/>
  </svg>'
