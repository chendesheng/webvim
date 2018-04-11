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


const app = Elm.Main.fullscreen({
  lineHeight,
  service: `//${location.hostname}:8080`,
  buffer: activeBuffer,
});

app.ports.saveBuffer.subscribe((buf) => {
  buffers[buf.path] = buf;
});

app.ports.getBuffer.subscribe((path) => {
  const buf = restoreBuffer(path);
  buffers[path] = buf;
  app.ports.restoreBuffer.send(buf);
  activeBuffer = path;
});

window.onbeforeunload = () => {
  sessionStorage.setItem('buffers', JSON.stringify(Object.values(buffers)));
  if (activeBuffer) {
    sessionStorage.setItem('activeBuffer', activeBuffer);
  }
};
