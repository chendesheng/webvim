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
  const repeatn = 2;
  for (let i = 0; i < repeatn; i++) {
    s += ch;
  }

  const span = document.getElementById('measureChar')
  || document.createElement('span');
  span.id = 'measureChar';
  span.textContent = s;
  span.className = 'editor line';
  span.style.cssText = 'position:absolute;left:-9999px;';

  // console.log(span.clientWidth);
  // console.log(span.offsetWidth);
  return {
    width: span.clientWidth / repeatn,
    height: span.clientHeight,
  };
}

function measureFont() {
  const span = document.createElement('span');
  span.id = 'measureChar';
  document.body.insertBefore(span, document.body.firstChild);

  const size = measureChar('m');
  // console.log(size);
  const size2 = measureChar('中');
  const size3 = measureChar('😄');
  const style = window.getComputedStyle(span, null);
  const fontSize = parseInt(style.getPropertyValue('font-size'));
  const fontName = style.getPropertyValue('font-family');
  const widths = [
    ['HALF', size.width],
    ['FULL', size2.width],
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

function storageGetArray(key, isJson) {
  let i = 0;
  let item = null;
  const res = [];
  while (item = sessionStorage.getItem(`${key}[${i}]`)) {
    res[i] = isJson ? safeJsonParse(item) : item;
    i++;
  }
  return res;
}

function main() {
  const flags = {
    service: `${scheme}//${host}:8899`,
    activeBuffer: safeJsonParse(sessionStorage.getItem('activeBuffer')),
    buffers: storageGetArray('buffers', true),
    registers: safeJsonParse(sessionStorage.getItem('registers')) || {},
    height: window.innerHeight,
    cwd: sessionStorage.getItem('cwd') || '',
    pathSeperator: '',
    fontInfo: measureFont(),
    homedir: '',
    isSafari: navigator.userAgent.indexOf('Safari') !== -1
      && navigator.userAgent.indexOf('Chrome') === -1,
    exHistory: storageGetArray('exHistory', false),
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
}

window.onload = function() {
  // measureFont() returns wrong result after window.location.reload() on Safari
  // Add delay here seems solve the issue
  setTimeout(main, 100);
};

