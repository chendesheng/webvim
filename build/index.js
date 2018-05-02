const host = location.hostname || 'localhost';
let scheme = '';
if (!location.hostname) {
  scheme = 'http:';
}

const flags = {
  lineHeight,
  service: `${scheme}//${host}:8080`,
  syntaxService: `${scheme}//${host}:8765`,
};
const app = Elm.Main.fullscreen(flags);

const applyCss = url => {
  const link = document.createElement('link');
  link.rel = "stylesheet";
  link.href = url;
  document.head.appendChild(link);
};

applyCss(`${flags.syntaxService}/css`);

app.ports.setTitle.subscribe(title => {
  document.title = title;
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
