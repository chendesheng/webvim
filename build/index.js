const getBuffers = () => {
  const bufs = sessionStorage.getItem('buffers')
  if (bufs) {
    return Object.values(JSON.parse(bufs)).reduce((result, buf) => {
      result[buf.path] = buf;
      return result;
    }, {});
  }
  return {};
}

const buffers = getBuffers();
let activeBuffer = sessionStorage.getItem('activeBuffer');

const restoreBuffer = (path) => {
  return Object.assign({
    path,
    cursor: [0, 0],
    scrollTop : 0,
    content: null,
  }, buffers[path]);
};


const flags = {
  lineHeight,
  service: `//${location.hostname}:8080`,
  syntaxService: `//${location.hostname}:8765`,
  buffer: activeBuffer,
};
const app = Elm.Main.fullscreen(flags);

const applyCss = url => {
  const link = document.createElement('link');
  link.rel = "stylesheet";
  link.href = url;
  document.head.appendChild(link);
};

applyCss(`${flags.syntaxService}/css`);

app.ports.saveBuffer.subscribe((buf) => {
  buffers[buf.path] = buf;
});

app.ports.setTitle.subscribe(title => {
  document.title = title;
});

app.ports.getBuffer.subscribe((path) => {
  const buf = restoreBuffer(path);
  buffers[path] = buf;
  app.ports.restoreBuffer.send(buf);
  activeBuffer = path;
});

let debouncers = {};
app.ports.debounce.subscribe(({ action, time, payload }) => {
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

window.onbeforeunload = () => {
  sessionStorage.setItem('buffers', JSON.stringify(Object.values(buffers)));
  if (activeBuffer) {
    sessionStorage.setItem('activeBuffer', activeBuffer);
  }
};
