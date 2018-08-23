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
const app = Elm.Main.init({
  flags,
});

document.addEventListener('keydown', function(event) {
  event.preventDefault();
  event.stopPropagation();

  app.ports.onKeyDown.send(event);
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
