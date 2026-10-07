(function () {
  function copiar(texto) {
    if (navigator.clipboard && window.isSecureContext) {
      return navigator.clipboard.writeText(texto);
    }
    return new Promise(function (ok, erro) {
      var t = document.createElement('textarea');
      t.value = texto; t.setAttribute('readonly', '');
      t.style.position = 'fixed'; t.style.opacity = '0';
      document.body.appendChild(t); t.select();
      try { document.execCommand('copy') ? ok() : erro(); } catch (e) { erro(e); }
      document.body.removeChild(t);
    });
  }

  function iniciar() {
    document.querySelectorAll('.article-post pre').forEach(function (pre) {
      if (pre.parentElement.classList.contains('bloco-codigo')) return;

      var bloco = document.createElement('div');
      bloco.className = 'bloco-codigo';
      pre.parentNode.insertBefore(bloco, pre);
      bloco.appendChild(pre);

      var botao = document.createElement('button');
      botao.type = 'button';
      botao.className = 'botao-copiar';
      botao.textContent = 'Copiar';
      botao.setAttribute('aria-label', 'Copiar o código');
      bloco.appendChild(botao);

      botao.addEventListener('click', function () {
        copiar(pre.textContent).then(function () {
          botao.textContent = 'Copiado!';
          botao.classList.add('copiado');
        }, function () {
          botao.textContent = 'Selecione e copie';
        }).then(function () {
          setTimeout(function () {
            botao.textContent = 'Copiar';
            botao.classList.remove('copiado');
          }, 1800);
        });
      });
    });
  }

  if (document.readyState === 'loading') document.addEventListener('DOMContentLoaded', iniciar);
  else iniciar();
})();