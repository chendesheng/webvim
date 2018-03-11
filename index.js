(function() {
  const flags = { };

  const app = Elm.Main.embed(document.getElementById('container'), flags);
  // app.ports.writeLocalStorage.subscribe(([key, val]) => {
  //   localStorage.setItem(key + '_' + flags.siteId, JSON.stringify(val));
  // });

})();
