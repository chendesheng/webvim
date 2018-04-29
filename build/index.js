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
let activeBuffer = sessionStorage.getItem('activeBuffer') || '';

const restoreBuffer = (path, cursor) => {
  const buf = Object.assign({
    path,
    cursor,
    scrollTop: 0,
    content: null,
  }, buffers[path]);
  buffers[path] = buf;
  app.ports.restoreBuffer.send(buf);
  activeBuffer = path;
};


const host = location.hostname || 'localhost';
let scheme = '';
if (!location.hostname) {
  scheme = 'http:';
}

const flags = {
  lineHeight,
  service: `${scheme}//${host}:8080`,
  syntaxService: `${scheme}//${host}:8765`,
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
  restoreBuffer(path, [0, 0]);
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


// jumps list
let jumps = {
  cursor: buffers[activeBuffer] ? buffers[activeBuffer].cursor : [0,0],
  buffer: activeBuffer,
};
const jump2str = jump => {
  if (jump) return `${jump.buffer}:${jump.cursor[0]}:${jump.cursor[1]}`;
  else return '';
};
const printJumps = (jumps) => {
  const join = (a, b) => {
    if (a && b) {
      return `${a}\n\t\t↑\n${b}`;
    }
    return a || b;
  };
  const ups = (jumps) => {
    let res = '';
    let prev = jumps.prev;
    while (prev != null) {
      res = join(jump2str(prev), res);
      prev = prev.prev;
    }
    return res;
  };
  const downs = (jumps) => {
    let res = '';
    let next = jumps.next;
    while (next != null) {
      res = join(res, jump2str(next));
      next = next.next;
    }
    return res;
  };
  return join(join(ups(jumps), `${jump2str(jumps)} ←`), downs(jumps));
};
console.log(printJumps(jumps));
// prev2 <- prev <- current -> next -> next2 
// add => prev2 <- prev <- current -> next -> next2 
app.ports.saveCursorPosition.subscribe(cursor => {
  console.log('saveCursorPosition before');
  console.log(printJumps(jumps))

  let prev = jumps.prev;
  let next = jumps.next;
  while (next != null) {
    const next2 = next.next;
    prev = Object.assign(next, { next: null, prev });
    next = next2;
  }
  if (prev) {
    console.log('prev');
    console.log(printJumps(prev))
  }
  jumps = Object.assign(jumps, { prev, next: null });
  
  jumps = {
    cursor,
    buffer: activeBuffer,
    prev: jumps,
  };
  console.log('saveCursorPosition after');
  console.log(printJumps(jumps))
});

app.ports.jump.subscribe(isForward => {
  console.log('jump before:', isForward);
  console.log(printJumps(jumps))

  const jumpTo = isForward ? jumps.next : jumps.prev;
  if (!jumpTo) {
    return;
  }
  if (isForward) {
    jumps = Object.assign(jumps.next, {
      prev: Object.assign(jumps, { next: null}),
    });
  } else {
    jumps = Object.assign(jumps.prev, {
      next: Object.assign(jumps, { prev: null }), 
    });
  }

  console.log('after jump');
  console.log(printJumps(jumps))

  if (jumpTo.buffer === activeBuffer) {
    app.ports.onJump.send(jumpTo.cursor);
  } else {
    restoreBuffer(jumpTo.buffer, jumpTo.cursor);
  }
});

// window.onbeforeunload = () => {
//   sessionStorage.setItem('buffers', JSON.stringify(Object.values(buffers)));
//   if (activeBuffer) {
//     sessionStorage.setItem('activeBuffer', activeBuffer);
//   }
// };
