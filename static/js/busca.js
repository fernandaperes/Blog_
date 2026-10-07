(function () {
  var input = document.getElementById('busca-input');
  if (!input) return;
  var caixa = document.getElementById('busca-resultados');
  var lista = document.getElementById('lista-posts');
  var fuse = null, carregando = null;

  function aviso(msg) {
    lista.hidden = true;
    caixa.hidden = false;
    caixa.innerHTML = '<p class="busca-vazio">' + msg + '</p>';
  }

  /* Baixa o índice só quando a pessoa clica na caixa (não pesa a página) */
  function carregar() {
    if (!carregando) {
      carregando = fetch('/index.json')
        .then(function (r) {
          if (!r.ok) throw new Error('index.json: HTTP ' + r.status);
          return r.json();
        })
        .then(function (dados) {
          if (typeof Fuse === 'undefined') throw new Error('Fuse.js não carregou');
          fuse = new Fuse(dados, {
            keys: [
              { name: 'title', weight: 3 },
              { name: 'tags', weight: 2 },
              { name: 'summary', weight: 1.5 },
              { name: 'content', weight: 1 }
            ],
            threshold: 0.3,          /* menor = mais exigente; maior = mais tolerante */
            ignoreLocation: true,    /* acha a palavra em qualquer ponto do texto */
            ignoreDiacritics: true,  /* "estatistica" = "estatística" (Fuse 7.1+) */
            minMatchCharLength: 2
          });
        });
      carregando.catch(function () { carregando = null; });  /* permite tentar de novo */
    }
    return carregando;
  }

  function esc(s) {
    return String(s).replace(/&/g, '&amp;').replace(/</g, '&lt;').replace(/>/g, '&gt;').replace(/"/g, '&quot;');
  }
  /* tira fórmulas \( ... \) do trecho exibido, inclusive as cortadas no meio */
  function limpa(s) {
    return String(s).replace(/\\[(\[][\s\S]*?\\[)\]]|\\[(\[][\s\S]*$/g, '');
  }

  function buscar() {
    var q = input.value.trim();
    if (q.length < 2) { caixa.hidden = true; lista.hidden = false; return; }
    carregar().then(function () {
      var res = fuse.search(q, { limit: 20 });
      lista.hidden = true;
      caixa.hidden = false;
      if (!res.length) {
        caixa.innerHTML = '<p class="busca-vazio">No results for “' + esc(q) + '”.</p>';
        return;
      }
      caixa.innerHTML = res.map(function (r) {
        var p = r.item;
        return '<a class="busca-item" href="' + esc(p.url) + '">' +
          '<span class="busca-data">' + esc(p.date) + '</span>' +
          '<strong>' + esc(p.title) + '</strong>' +
          '<span class="busca-resumo">' + esc(limpa(p.summary)) + '</span></a>';
      }).join('');
    }).catch(function (e) {
      console.error('Busca:', e);
      aviso('Search is unavailable right now (' + esc(e.message) + ').');
    });
  }

  input.addEventListener('focus', function () { carregar().catch(function () {}); });
  input.addEventListener('input', buscar);
  input.addEventListener('keydown', function (e) {
    if (e.key === 'Enter') { e.preventDefault(); buscar(); }
    if (e.key === 'Escape') { input.value = ''; buscar(); }
  });
})();