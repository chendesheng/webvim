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
  const widthByType = [
    ['F', measureChar(String.fromCodePoint(0xFF0A)).width],
    ['H', measureChar(String.fromCodePoint(0x20A9)).width],
    ['W', measureChar(String.fromCodePoint(0x1100)).width],
    ['Na', measureChar(String.fromCodePoint(0x003F)).width],
    ['A', measureChar(String.fromCodePoint(0xA1)).width],
    ['N', measureChar(String.fromCodePoint(0x0CDE)).width],
  ];
  span.remove();
  return {
    widths: [{
      from: 256,
      to: 0x1f600,
      width: size2.width,
    }, {
      from: 0x1f600,
      to: 0xffffffff,
      width: size3.width,
    }],
    widthByType,
    asciiCharWidth: size.width,
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
