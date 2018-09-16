class SessionStorageItem extends HTMLElement {
  constructor() {
    super();
  }

  connectedCallback() {
    sessionStorage.setItem(
      this.getAttribute('key'),
      this.getAttribute('value')
    );
  }

  disconnectedCallback() {
    sessionStorage.removeItem(this.getAttribute('key'));
  }

  static get observedAttributes() {
    return ['key', 'value'];
  }

  attributeChangedCallback(name, oldValue, newValue) {
    // console.log('attributeChangedCallback');

    if (name == 'key') {
      sessionStorage.removeItem(this.getAttribute('key'));
    }
    sessionStorage.setItem(
      this.getAttribute('key'),
      this.getAttribute('value')
    );
  }
}
if (typeof customElements !== 'undefined') {
  customElements.define('session-storage-item', SessionStorageItem);
}

function measureChar(ch) {
  let s = '';
  for (let i = 0; i < 256; i++) {
    s += ch;
  }

  const span = document.getElementById('measureChar')
  || document.createElement('span');
  span.id = 'measureChar';
  span.textContent = s;
  span.className = 'editor line';
  span.style.cssText = 'position:absolute;left:-9999px;';

  return {
    width: span.clientWidth / 256,
    height: span.clientHeight,
  };
}

// eslint-disable-next-line
// copy from: https://github.com/Microsoft/vscode/blob/3a619f24c3b7f760f283193ebd9c3ed601768a83/src/vs/base/common/strings.ts 
// eslint-disable-next-line
const RE_EMOJI = /(?:[\u231A\u231B\u23F0\u23F3\u2600-\u27BF\u2B50\u2B55]|\uD83C[\uDDE6-\uDDFF\uDF00-\uDFFF]|\uD83D[\uDC00-\uDE4F\uDE80-\uDEF8]|\uD83E[\uDD00-\uDDE6])/;
// eslint-disable-next-line
function charWidthType(ch) {
  const codePoint = ch.charCodeAt(0);
  if ((0x3000 == codePoint)
    || (0xFF01 <= codePoint && codePoint <= 0xFF60)
    || (0xFFE0 <= codePoint && codePoint <= 0xFFE6)
  ) {
    return 'F';
  } else if ((0x20A9 == codePoint)
    || (0xFF61 <= codePoint && codePoint <= 0xFFBE)
    || (0xFFC2 <= codePoint && codePoint <= 0xFFC7)
    || (0xFFCA <= codePoint && codePoint <= 0xFFCF)
    || (0xFFD2 <= codePoint && codePoint <= 0xFFD7)
    || (0xFFDA <= codePoint && codePoint <= 0xFFDC)
    || (0xFFE8 <= codePoint && codePoint <= 0xFFEE)
  ) {
    return 'H';
  } else if ((0x1100 <= codePoint && codePoint <= 0x115F)
    || (0x11A3 <= codePoint && codePoint <= 0x11A7)
    || (0x11FA <= codePoint && codePoint <= 0x11FF)
    || (0x2329 <= codePoint && codePoint <= 0x232A)
    || (0x2E80 <= codePoint && codePoint <= 0x2E99)
    || (0x2E9B <= codePoint && codePoint <= 0x2EF3)
    || (0x2F00 <= codePoint && codePoint <= 0x2FD5)
    || (0x2FF0 <= codePoint && codePoint <= 0x2FFB)
    || (0x3001 <= codePoint && codePoint <= 0x303E)
    || (0x3041 <= codePoint && codePoint <= 0x3096)
    || (0x3099 <= codePoint && codePoint <= 0x30FF)
    || (0x3105 <= codePoint && codePoint <= 0x312D)
    || (0x3131 <= codePoint && codePoint <= 0x318E)
    || (0x3190 <= codePoint && codePoint <= 0x31BA)
    || (0x31C0 <= codePoint && codePoint <= 0x31E3)
    || (0x31F0 <= codePoint && codePoint <= 0x321E)
    || (0x3220 <= codePoint && codePoint <= 0x3247)
    || (0x3250 <= codePoint && codePoint <= 0x32FE)
    || (0x3300 <= codePoint && codePoint <= 0x4DBF)
    || (0x4E00 <= codePoint && codePoint <= 0xA48C)
    || (0xA490 <= codePoint && codePoint <= 0xA4C6)
    || (0xA960 <= codePoint && codePoint <= 0xA97C)
    || (0xAC00 <= codePoint && codePoint <= 0xD7A3)
    || (0xD7B0 <= codePoint && codePoint <= 0xD7C6)
    || (0xD7CB <= codePoint && codePoint <= 0xD7FB)
    || (0xF900 <= codePoint && codePoint <= 0xFAFF)
    || (0xFE10 <= codePoint && codePoint <= 0xFE19)
    || (0xFE30 <= codePoint && codePoint <= 0xFE52)
    || (0xFE54 <= codePoint && codePoint <= 0xFE66)
    || (0xFE68 <= codePoint && codePoint <= 0xFE6B)
    || (0x0001B000 <= codePoint && codePoint <= 0x0001B001)
    || (0x0001F200 <= codePoint && codePoint <= 0x0001F202)
    || (0x0001F210 <= codePoint && codePoint <= 0x0001F23A)
    || (0x0001F240 <= codePoint && codePoint <= 0x0001F248)
    || (0x0001F250 <= codePoint && codePoint <= 0x0001F251)
    || (0x00020000 <= codePoint && codePoint <= 0x0002F73F)
    || (0x0002B740 <= codePoint && codePoint <= 0x0002FFFD)
    || (0x00030000 <= codePoint && codePoint <= 0x0003FFFD)
  ) {
    return 'W';
  } else if ((0x20 <= codePoint && codePoint <= 0x7E)
    || (0xA2 <= codePoint && codePoint <= 0xA3)
    || (0xA5 <= codePoint && codePoint <= 0xA6)
    || (0xAC == codePoint)
    || (0xAF == codePoint)
    || (0x27E6 <= codePoint && codePoint <= 0x27ED)
    || (0x2985 <= codePoint && codePoint <= 0x2986)
  ) {
    return 'Na';
  } else if
  ((0xA1 == codePoint)
    || (0xA4 == codePoint)
    || (0xA7 <= codePoint && codePoint <= 0xA8)
    || (0xAA == codePoint)
    || (0xAD <= codePoint && codePoint <= 0xAE)
    || (0xB0 <= codePoint && codePoint <= 0xB4)
    || (0xB6 <= codePoint && codePoint <= 0xBA)
    || (0xBC <= codePoint && codePoint <= 0xBF)
    || (0xC6 == codePoint)
    || (0xD0 == codePoint)
    || (0xD7 <= codePoint && codePoint <= 0xD8)
    || (0xDE <= codePoint && codePoint <= 0xE1)
    || (0xE6 == codePoint)
    || (0xE8 <= codePoint && codePoint <= 0xEA)
    || (0xEC <= codePoint && codePoint <= 0xED)
    || (0xF0 == codePoint)
    || (0xF2 <= codePoint && codePoint <= 0xF3)
    || (0xF7 <= codePoint && codePoint <= 0xFA)
    || (0xFC == codePoint)
    || (0xFE == codePoint)
    || (0x0101 == codePoint)
    || (0x0111 == codePoint)
    || (0x0113 == codePoint)
    || (0x011B == codePoint)
    || (0x0126 <= codePoint && codePoint <= 0x0127)
    || (0x012B == codePoint)
    || (0x0131 <= codePoint && codePoint <= 0x0133)
    || (0x0138 == codePoint)
    || (0x013F <= codePoint && codePoint <= 0x0142)
    || (0x0144 == codePoint)
    || (0x0148 <= codePoint && codePoint <= 0x014B)
    || (0x014D == codePoint)
    || (0x0152 <= codePoint && codePoint <= 0x0153)
    || (0x0166 <= codePoint && codePoint <= 0x0167)
    || (0x016B == codePoint)
    || (0x01CE == codePoint)
    || (0x01D0 == codePoint)
    || (0x01D2 == codePoint)
    || (0x01D4 == codePoint)
    || (0x01D6 == codePoint)
    || (0x01D8 == codePoint)
    || (0x01DA == codePoint)
    || (0x01DC == codePoint)
    || (0x0251 == codePoint)
    || (0x0261 == codePoint)
    || (0x02C4 == codePoint)
    || (0x02C7 == codePoint)
    || (0x02C9 <= codePoint && codePoint <= 0x02CB)
    || (0x02CD == codePoint)
    || (0x02D0 == codePoint)
    || (0x02D8 <= codePoint && codePoint <= 0x02DB)
    || (0x02DD == codePoint)
    || (0x02DF == codePoint)
    || (0x0300 <= codePoint && codePoint <= 0x036F)
    || (0x0391 <= codePoint && codePoint <= 0x03A1)
    || (0x03A3 <= codePoint && codePoint <= 0x03A9)
    || (0x03B1 <= codePoint && codePoint <= 0x03C1)
    || (0x03C3 <= codePoint && codePoint <= 0x03C9)
    || (0x0401 == codePoint)
    || (0x0410 <= codePoint && codePoint <= 0x044F)
    || (0x0451 == codePoint)
    || (0x2010 == codePoint)
    || (0x2013 <= codePoint && codePoint <= 0x2016)
    || (0x2018 <= codePoint && codePoint <= 0x2019)
    || (0x201C <= codePoint && codePoint <= 0x201D)
    || (0x2020 <= codePoint && codePoint <= 0x2022)
    || (0x2024 <= codePoint && codePoint <= 0x2027)
    || (0x2030 == codePoint)
    || (0x2032 <= codePoint && codePoint <= 0x2033)
    || (0x2035 == codePoint)
    || (0x203B == codePoint)
    || (0x203E == codePoint)
    || (0x2074 == codePoint)
    || (0x207F == codePoint)
    || (0x2081 <= codePoint && codePoint <= 0x2084)
    || (0x20AC == codePoint)
    || (0x2103 == codePoint)
    || (0x2105 == codePoint)
    || (0x2109 == codePoint)
    || (0x2113 == codePoint)
    || (0x2116 == codePoint)
    || (0x2121 <= codePoint && codePoint <= 0x2122)
    || (0x2126 == codePoint)
    || (0x212B == codePoint)
    || (0x2153 <= codePoint && codePoint <= 0x2154)
    || (0x215B <= codePoint && codePoint <= 0x215E)
    || (0x2160 <= codePoint && codePoint <= 0x216B)
    || (0x2170 <= codePoint && codePoint <= 0x2179)
    || (0x2189 == codePoint)
    || (0x2190 <= codePoint && codePoint <= 0x2199)
    || (0x21B8 <= codePoint && codePoint <= 0x21B9)
    || (0x21D2 == codePoint)
    || (0x21D4 == codePoint)
    || (0x21E7 == codePoint)
    || (0x2200 == codePoint)
    || (0x2202 <= codePoint && codePoint <= 0x2203)
    || (0x2207 <= codePoint && codePoint <= 0x2208)
    || (0x220B == codePoint)
    || (0x220F == codePoint)
    || (0x2211 == codePoint)
    || (0x2215 == codePoint)
    || (0x221A == codePoint)
    || (0x221D <= codePoint && codePoint <= 0x2220)
    || (0x2223 == codePoint)
    || (0x2225 == codePoint)
    || (0x2227 <= codePoint && codePoint <= 0x222C)
    || (0x222E == codePoint)
    || (0x2234 <= codePoint && codePoint <= 0x2237)
    || (0x223C <= codePoint && codePoint <= 0x223D)
    || (0x2248 == codePoint)
    || (0x224C == codePoint)
    || (0x2252 == codePoint)
    || (0x2260 <= codePoint && codePoint <= 0x2261)
    || (0x2264 <= codePoint && codePoint <= 0x2267)
    || (0x226A <= codePoint && codePoint <= 0x226B)
    || (0x226E <= codePoint && codePoint <= 0x226F)
    || (0x2282 <= codePoint && codePoint <= 0x2283)
    || (0x2286 <= codePoint && codePoint <= 0x2287)
    || (0x2295 == codePoint)
    || (0x2299 == codePoint)
    || (0x22A5 == codePoint)
    || (0x22BF == codePoint)
    || (0x2312 == codePoint)
    || (0x2460 <= codePoint && codePoint <= 0x24E9)
    || (0x24EB <= codePoint && codePoint <= 0x254B)
    || (0x2550 <= codePoint && codePoint <= 0x2573)
    || (0x2580 <= codePoint && codePoint <= 0x258F)
    || (0x2592 <= codePoint && codePoint <= 0x2595)
    || (0x25A0 <= codePoint && codePoint <= 0x25A1)
    || (0x25A3 <= codePoint && codePoint <= 0x25A9)
    || (0x25B2 <= codePoint && codePoint <= 0x25B3)
    || (0x25B6 <= codePoint && codePoint <= 0x25B7)
    || (0x25BC <= codePoint && codePoint <= 0x25BD)
    || (0x25C0 <= codePoint && codePoint <= 0x25C1)
    || (0x25C6 <= codePoint && codePoint <= 0x25C8)
    || (0x25CB == codePoint)
    || (0x25CE <= codePoint && codePoint <= 0x25D1)
    || (0x25E2 <= codePoint && codePoint <= 0x25E5)
    || (0x25EF == codePoint)
    || (0x2605 <= codePoint && codePoint <= 0x2606)
    || (0x2609 == codePoint)
    || (0x260E <= codePoint && codePoint <= 0x260F)
    || (0x2614 <= codePoint && codePoint <= 0x2615)
    || (0x261C == codePoint)
    || (0x261E == codePoint)
    || (0x2640 == codePoint)
    || (0x2642 == codePoint)
    || (0x2660 <= codePoint && codePoint <= 0x2661)
    || (0x2663 <= codePoint && codePoint <= 0x2665)
    || (0x2667 <= codePoint && codePoint <= 0x266A)
    || (0x266C <= codePoint && codePoint <= 0x266D)
    || (0x266F == codePoint)
    || (0x269E <= codePoint && codePoint <= 0x269F)
    || (0x26BE <= codePoint && codePoint <= 0x26BF)
    || (0x26C4 <= codePoint && codePoint <= 0x26CD)
    || (0x26CF <= codePoint && codePoint <= 0x26E1)
    || (0x26E3 == codePoint)
    || (0x26E8 <= codePoint && codePoint <= 0x26FF)
    || (0x273D == codePoint)
    || (0x2757 == codePoint)
    || (0x2776 <= codePoint && codePoint <= 0x277F)
    || (0x2B55 <= codePoint && codePoint <= 0x2B59)
    || (0x3248 <= codePoint && codePoint <= 0x324F)
    || (0xE000 <= codePoint && codePoint <= 0xF8FF)
    || (0xFE00 <= codePoint && codePoint <= 0xFE0F)
    || (0xFFFD == codePoint)
    || (0x0001F100 <= codePoint && codePoint <= 0x0001F10A)
    || (0x0001F110 <= codePoint && codePoint <= 0x0001F12D)
    || (0x0001F130 <= codePoint && codePoint <= 0x0001F169)
    || (0x0001F170 <= codePoint && codePoint <= 0x0001F19A)
    || (0x000E0100 <= codePoint && codePoint <= 0x000E01EF)
    || (0x000F0000 <= codePoint && codePoint <= 0x000FFFFD)
    || (0x00100000 <= codePoint && codePoint <= 0x0010FFFD)
  ) {
    return 'A';
  } else if (RE_EMOJI.test(ch)) {
    return 'EMOJI';
  } else {
    return 'N';
  }
}

function measureFont() {
  const span = document.createElement('span');
  span.id = 'measureChar';
  document.body.insertBefore(span, document.body.firstChild);

  const size = measureChar('m');
  const size2 = measureChar('中');
  const size3 = measureChar('😄');
  const style = window.getComputedStyle(span, null);
  const fontSize = parseInt(style.getPropertyValue('font-size'));
  const fontName = style.getPropertyValue('font-family');
  const widths = [
    ['F', size2.width],
    ['H', size.width],
    ['W', size2.width],
    ['Na', size.width],
    ['A', size.width],
    ['N', size.width],
    ['EMOJI', size3.width],
  ];
  span.remove();
  return {
    widths,
    lineHeight: size.height,
    size: fontSize,
    name: fontName,
  };
}

const safeJsonParse = (s) => {
  if (s) {
    return JSON.parse(s);
  } else {
    return null;
  }
};

const getTheme = () => {
  const m = location.search.match(/[&?]theme=([^&]*)/i);
  return m ? m[1] : null;
};

const host = location.hostname || 'localhost';
let scheme = '';
if (!location.hostname) {
  scheme = 'http:';
}

const flags = {
  service: `${scheme}//${host}:8899`,
  activeBuffer: safeJsonParse(sessionStorage.getItem('activeBuffer')),
  buffers: safeJsonParse(sessionStorage.getItem('buffers')) || [],
  registers: safeJsonParse(sessionStorage.getItem('registers')) || {},
  height: window.innerHeight,
  cwd: sessionStorage.getItem('cwd') || '',
  pathSeperator: '',
  fontInfo: measureFont(),
};

const applyCss = (url) => {
  const link = document.createElement('link');
  link.rel = 'stylesheet';
  link.href = url;
  document.head.appendChild(link);
};

applyCss(`${flags.service}/css?theme=${getTheme() || 'Solarized Dark'}`);

// console.log("flags", flags);
const app = Elm.Main.init({
  flags,
});

const debouncers = {};
app.ports.debounce.subscribe(({action, time, payload}) => {
  const debouncer = debouncers[action] || {
    payloads: [],
  };
  debouncers[action] = debouncer;

  // console.log('debounce', action);
  if (payload !== undefined) {
    debouncer.payloads.push(payload);
  }

  clearTimeout(debouncer.timer);
  debouncer.timer = setTimeout(() => {
    // console.log('send debounced', action);
    const payloads = debouncer.payloads;
    debouncer.payloads = [];
    app.ports.onDebounce.send({
      action,
      payloads,
    });
  }, time);
});
