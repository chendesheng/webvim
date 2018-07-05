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
  lineHeight,
  service: `${scheme}//${host}:8899`,
  activeBuffer: safeJsonParse(sessionStorage.getItem('activeBuffer')),
  buffers: safeJsonParse(sessionStorage.getItem('buffers')) || [],
  registers: safeJsonParse(sessionStorage.getItem('registers')) || {},
  height: window.innerHeight,
  cwd: sessionStorage.getItem('cwd') || '',
  pathSeperator: '',
};

const applyCss = (url) => {
  const link = document.createElement('link');
  link.rel = 'stylesheet';
  link.href = url;
  document.head.appendChild(link);
};

applyCss(`${flags.service}/css?theme=${getTheme() || 'Solarized Dark'}`);

// console.log("flags", flags);
const app = Elm.Main.fullscreen(flags);

app.ports.setTitle.subscribe((title) => {
  document.title = title || 'no name';
});

let debouncers = {};
app.ports.debounce.subscribe(({action, time, payload}) => {
  const data = debouncers[action] || {};
  // console.log('debounce', action);
  if (payload !== undefined) {
    const payloads = data.payloads || [];
    payloads.push(payload);
    data.payloads = payloads;
  }

  clearTimeout(data.timer);
  data.timer = setTimeout(() => {
    // console.log('send debounced', action);
    app.ports.onDebounce.send({
      action,
      payloads: debouncers[action].payloads || [],
    });
    debouncers[action].payloads = null;
  }, time);

  debouncers[action] = data;
});
