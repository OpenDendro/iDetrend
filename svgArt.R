# Decorative SVG art for the welcome state of the Overview panel. Kept out
# of server.R so the logic there stays readable. Its colour is the app's
# primary colour (ui.R): change both together.
#
# On the left, what the app does: a ring-width series with a curve fitted to
# it, and under it the indices, the widths divided by the curve. It is the
# plot on the Detrend panel in miniature. The series is simulated (a negative
# exponential decline with autocorrelated noise that scales with the level),
# not data from any site.

welcomeSVG <- '
  <svg width="100%" viewBox="0 0 680 420" xmlns="http://www.w3.org/2000/svg">
    <text x="30" y="52" font-family="sans-serif" font-size="12" fill="#888">Ring widths, and the curve fitted to them</text>
    <path d="M30.0 92.0 L32.4 69.7 L34.8 91.5 L37.2 103.8 L39.5 131.0 L41.9 117.1 L44.3 70.4 L46.7 79.1 L49.1 64.9 L51.5 86.5 L53.9 91.3 L56.2 100.9 L58.6 158.4 L61.0 108.3 L63.4 102.5 L65.8 102.4 L68.2 160.8 L70.6 182.4 L72.9 168.8 L75.3 154.6 L77.7 132.2 L80.1 134.0 L82.5 122.7 L84.9 146.0 L87.2 133.9 L89.6 129.0 L92.0 150.6 L94.4 109.6 L96.8 120.6 L99.2 112.7 L101.6 146.5 L103.9 161.0 L106.3 159.0 L108.7 154.6 L111.1 140.4 L113.5 143.2 L115.9 157.0 L118.3 171.0 L120.6 168.8 L123.0 139.6 L125.4 163.6 L127.8 155.3 L130.2 150.2 L132.6 178.8 L135.0 165.3 L137.3 142.1 L139.7 184.3 L142.1 174.1 L144.5 167.8 L146.9 176.2 L149.3 160.8 L151.7 163.8 L154.0 184.4 L156.4 160.6 L158.8 155.0 L161.2 149.8 L163.6 142.1 L166.0 153.9 L168.3 161.4 L170.7 182.2 L173.1 165.7 L175.5 175.4 L177.9 177.1 L180.3 187.7 L182.7 187.9 L185.0 182.9 L187.4 160.0 L189.8 191.1 L192.2 195.3 L194.6 177.4 L197.0 157.8 L199.4 161.0 L201.7 190.0 L204.1 206.2 L206.5 181.2 L208.9 184.4 L211.3 189.8 L213.7 169.2 L216.1 161.0 L218.4 168.4 L220.8 170.2 L223.2 169.0 L225.6 156.7 L228.0 162.7 L230.4 166.0 L232.8 167.0 L235.1 189.0 L237.5 167.9 L239.9 164.1 L242.3 167.1 L244.7 193.2 L247.1 189.0 L249.4 173.1 L251.8 193.7 L254.2 185.0 L256.6 170.4 L259.0 187.9 L261.4 166.0 L263.8 168.6 L266.1 176.3 L268.5 174.6 L270.9 171.0 L273.3 174.8 L275.7 166.6 L278.1 180.8 L280.5 183.4 L282.8 170.9 L285.2 176.0 L287.6 186.2 L290.0 173.0" fill="none" stroke="#2F4B7C" stroke-width="1.3" opacity="0.45" stroke-linejoin="round"/>
    <path d="M30.0 82.2 L32.4 85.5 L34.8 88.6 L37.2 91.7 L39.5 94.6 L41.9 97.5 L44.3 100.3 L46.7 102.9 L49.1 105.5 L51.5 108.0 L53.9 110.5 L56.2 112.8 L58.6 115.1 L61.0 117.3 L63.4 119.4 L65.8 121.4 L68.2 123.4 L70.6 125.3 L72.9 127.2 L75.3 129.0 L77.7 130.7 L80.1 132.4 L82.5 134.0 L84.9 135.6 L87.2 137.1 L89.6 138.6 L92.0 140.0 L94.4 141.4 L96.8 142.7 L99.2 144.0 L101.6 145.2 L103.9 146.4 L106.3 147.6 L108.7 148.7 L111.1 149.8 L113.5 150.8 L115.9 151.9 L118.3 152.8 L120.6 153.8 L123.0 154.7 L125.4 155.6 L127.8 156.5 L130.2 157.3 L132.6 158.1 L135.0 158.9 L137.3 159.6 L139.7 160.4 L142.1 161.1 L144.5 161.8 L146.9 162.4 L149.3 163.1 L151.7 163.7 L154.0 164.3 L156.4 164.8 L158.8 165.4 L161.2 165.9 L163.6 166.5 L166.0 167.0 L168.3 167.5 L170.7 167.9 L173.1 168.4 L175.5 168.8 L177.9 169.3 L180.3 169.7 L182.7 170.1 L185.0 170.5 L187.4 170.8 L189.8 171.2 L192.2 171.6 L194.6 171.9 L197.0 172.2 L199.4 172.5 L201.7 172.8 L204.1 173.1 L206.5 173.4 L208.9 173.7 L211.3 174.0 L213.7 174.2 L216.1 174.5 L218.4 174.7 L220.8 175.0 L223.2 175.2 L225.6 175.4 L228.0 175.6 L230.4 175.8 L232.8 176.0 L235.1 176.2 L237.5 176.4 L239.9 176.6 L242.3 176.8 L244.7 176.9 L247.1 177.1 L249.4 177.2 L251.8 177.4 L254.2 177.5 L256.6 177.7 L259.0 177.8 L261.4 178.0 L263.8 178.1 L266.1 178.2 L268.5 178.3 L270.9 178.4 L273.3 178.6 L275.7 178.7 L278.1 178.8 L280.5 178.9 L282.8 179.0 L285.2 179.1 L287.6 179.2 L290.0 179.3" fill="none" stroke="#2F4B7C" stroke-width="3.5" opacity="0.85" stroke-linecap="round"/>
    <line x1="30" y1="214" x2="290" y2="214" stroke="#ccc" stroke-width="0.5"/>
    <path d="M160 226 L160 252 M153 245 L160 253 L167 245" fill="none" stroke="#2F4B7C" stroke-width="1.5" opacity="0.5" stroke-linecap="round"/>
    <text x="30" y="282" font-family="sans-serif" font-size="12" fill="#888">Indices: the widths divided by the curve</text>
    <line x1="30" y1="324.7" x2="290" y2="324.7" stroke="#2F4B7C" stroke-width="1" opacity="0.5" stroke-dasharray="4 4"/>
    <path d="M30.0 328.9 L32.4 317.7 L34.8 326.0 L37.2 330.4 L39.5 342.1 L41.9 334.3 L44.3 309.6 L46.7 312.4 L49.1 303.2 L51.5 313.0 L53.9 314.1 L56.2 317.9 L58.6 350.0 L61.0 319.4 L63.4 314.4 L65.8 312.8 L68.2 348.6 L70.6 362.0 L72.9 352.5 L75.3 342.2 L77.7 325.8 L80.1 325.8 L82.5 316.5 L84.9 332.5 L87.2 322.3 L89.6 317.3 L92.0 333.1 L94.4 299.1 L96.8 306.5 L99.2 298.5 L101.6 325.8 L103.9 337.4 L106.3 334.8 L108.7 330.0 L111.1 316.1 L113.5 317.6 L115.9 329.6 L118.3 342.3 L120.6 339.5 L123.0 309.6 L125.4 332.8 L127.8 323.5 L130.2 317.2 L132.6 346.8 L135.0 331.6 L137.3 305.5 L139.7 351.4 L142.1 339.4 L144.5 331.6 L146.9 340.7 L149.3 322.0 L151.7 324.8 L154.0 349.0 L156.4 319.5 L158.8 311.8 L161.2 304.5 L163.6 293.7 L166.0 307.8 L168.3 316.8 L170.7 343.5 L173.1 321.1 L175.5 333.6 L177.9 335.3 L180.3 349.4 L182.7 349.4 L185.0 342.2 L187.4 309.4 L189.8 353.1 L192.2 358.8 L194.6 332.7 L197.0 303.5 L199.4 307.7 L201.7 350.3 L204.1 374.2 L206.5 336.5 L208.9 341.1 L211.3 349.0 L213.7 317.0 L216.1 303.7 L218.4 314.8 L220.8 317.1 L223.2 314.8 L225.6 294.8 L228.0 304.0 L230.4 308.8 L232.8 310.1 L235.1 345.6 L237.5 310.8 L239.9 304.0 L242.3 308.7 L244.7 351.8 L247.1 344.7 L249.4 317.7 L251.8 352.3 L254.2 337.4 L256.6 312.2 L259.0 342.1 L261.4 304.1 L263.8 308.3 L266.1 321.5 L268.5 318.2 L270.9 311.6 L273.3 318.1 L275.7 303.4 L278.1 328.2 L280.5 332.8 L282.8 310.3 L285.2 319.2 L287.6 337.4 L290.0 313.4" fill="none" stroke="#2F4B7C" stroke-width="1.3" opacity="0.6" stroke-linejoin="round"/>
    <text x="490" y="96" text-anchor="middle" font-family="sans-serif" font-size="32" font-weight="500" fill="#2F4B7C" opacity="0.9">iDetrend</text>
    <text x="490" y="130" text-anchor="middle" font-family="sans-serif" font-size="14" fill="#666">Interactive detrending for tree-ring data</text>
    <line x1="430" y1="150" x2="550" y2="150" stroke="#ccc" stroke-width="0.5"/>
    <circle cx="352" cy="192" r="14" fill="#2F4B7C" opacity="0.15"/>
    <text x="352" y="197" text-anchor="middle" font-family="sans-serif" font-size="13" font-weight="500" fill="#2F4B7C">1</text>
    <text x="376" y="189" font-family="sans-serif" font-size="13" font-weight="500" fill="#333">Load a dated ring-width file</text>
    <text x="376" y="204" font-family="sans-serif" font-size="12" fill="#888">Tucson, Heidelberg, compact, TRiDaS, or .csv</text>
    <circle cx="352" cy="238" r="14" fill="#2F4B7C" opacity="0.15"/>
    <text x="352" y="243" text-anchor="middle" font-family="sans-serif" font-size="13" font-weight="500" fill="#2F4B7C">2</text>
    <text x="376" y="235" font-family="sans-serif" font-size="13" font-weight="500" fill="#333">Fit a curve to each series</text>
    <text x="376" y="250" font-family="sans-serif" font-size="12" fill="#888">See the curve and the indices as you choose</text>
    <circle cx="352" cy="284" r="14" fill="#2F4B7C" opacity="0.15"/>
    <text x="352" y="289" text-anchor="middle" font-family="sans-serif" font-size="13" font-weight="500" fill="#2F4B7C">3</text>
    <text x="376" y="281" font-family="sans-serif" font-size="13" font-weight="500" fill="#333">Check the chronology and save</text>
    <text x="376" y="296" font-family="sans-serif" font-size="12" fill="#888">Indices, chronology, and a report with R code</text>
    <rect x="358" y="330" width="264" height="32" rx="6" fill="#2F4B7C" opacity="0.08"/>
    <text x="490" y="351" text-anchor="middle" font-family="sans-serif" font-size="12" fill="#2F4B7C">Or try one of the two examples in the sidebar</text>
  </svg>
'
