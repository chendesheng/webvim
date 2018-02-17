(function() {
  const flags = {
    siteId: 1000007,
    campaignId: 1628,
    dynamic: false,
  };

  function safeParse(o) {
    if (typeof o !== 'string') return null;
    return JSON.parse(o);
  }

  flags.visitor = safeParse(localStorage.getItem('visitor_' + flags.siteId));

  const app = Elm.Main.embed(document.getElementById('container'), flags);
  app.ports.writeLocalStorage.subscribe(([key, val]) => {
    localStorage.setItem(key + '_' + flags.siteId, JSON.stringify(val));
  });

  app.ports.setWindowTitle.subscribe(title => {
    window.document.title = title;
  });
  app.ports.openCustomOffline.subscribe(([url, isNewWindow]) => {
    if (isNewWindow) {
      window.open(url);
    } else {
      window.top.location.href = url;
    }
  });
  app.ports.closeWindow.subscribe(() => {
    window.close();
  });
})();
