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
  // https://github.com/Microsoft/vscode/blob/3a619f24c3b7f760f283193ebd9c3ed601768a83/src/vs/base/common/strings.ts#L535
  if (
    (codePoint >= 0x2E80 && codePoint <= 0xD7AF)
    || (codePoint >= 0xF900 && codePoint <= 0xFAFF)
    || (codePoint >= 0xFF01 && codePoint <= 0xFF5E)
  ) {
    return 'FULL';
  } else {
    if (RE_EMOJI.test(ch)) {
      return 'EMOJI';
    } else {
      return 'HALF';
    }
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
    ['FULL', size2.width],
    ['HALF', size.width],
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
