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

function storageGetArray(key, isJson) {
  let i = 0;
  let item = null;
  const res = [];
  while (item = sessionStorage.getItem(`${key}[${i}]`)) {
    res[i] = isJson ? safeJsonParse(item) : item;
    i++;
  }
  return res.length > 0 ? res : null;
}

const host = location.hostname || 'localhost';
const scheme = location.hostname ? '' : 'http:';

function main() {
  const flags = {
    service: `${scheme}//${host}:8899`,
    window: safeJsonParse(sessionStorage.getItem('window')) || {},
    buffers: storageGetArray('buffers', true),
    registers: safeJsonParse(sessionStorage.getItem('registers')) || {},
    cwd: sessionStorage.getItem('cwd') || '',
    exHistory: storageGetArray('exHistory', false) || [],
  };

  // console.log("flags", flags);
  Elm.Main.init({
    flags,
  });
}

window.onload = function() {
  // measureFont() returns wrong result after window.location.reload() on Safari
  // Add delay here seems solve the issue
  setTimeout(main, 200);
};

