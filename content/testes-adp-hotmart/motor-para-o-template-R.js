/* Motor dos testes de autoavaliação · ADP
   Lê as questões geradas pelo R e mostra uma de cada vez.
   Só funciona dentro da área de membros (Hotmart) ou no seu computador.
   Você não precisa mexer neste arquivo, só na lista DOMINIOS_PERMITIDOS. */
(function () {
  // ---------- CONTROLES ----------
  // Domínios em que o teste pode ser aberto (vale para os subdomínios também).
  var DOMINIOS_PERMITIDOS = ["hotmart.com"];
  var LINK_FORA_DO_CURSO = "https://curso.fernandafperes.com.br/";
  // --------------------------------

  var LETRAS = "ABCDEFGHIJ";
  var raiz = document.getElementById("adp");
  var palco = document.getElementById("palco");
  var progresso = document.getElementById("progresso");
  var contador = document.getElementById("contador");
  var embaralhar = raiz.getAttribute("data-embaralhar") === "true";
  var proximaAula = raiz.getAttribute("data-proxima") || "";
  var questoes = Array.prototype.slice.call(document.querySelectorAll(".questao"));

  // ---------- Trava: só abre dentro do curso ----------
  function dominioPermitido(url) {
    try {
      var h = new URL(url).hostname;
      return DOMINIOS_PERMITIDOS.some(function (d) { return h === d || h.slice(-(d.length + 1)) === "." + d; });
    } catch (e) { return false; }
  }

  function acessoPermitido() {
    var h = location.hostname;
    // No seu computador (arquivo aberto direto ou servidor local), sempre abre
    if (location.protocol === "file:" || h === "localhost" || h === "127.0.0.1") return true;

    var emIframe;
    try { emIframe = window.self !== window.top; } catch (e) { emIframe = true; }
    if (!emIframe) return false; // aberto numa aba normal

    // Dentro de um iframe: confere quem está "por fora", quando o navegador informa
    var origens = [];
    if (location.ancestorOrigins && location.ancestorOrigins.length) {
      origens = Array.prototype.slice.call(location.ancestorOrigins);
    } else if (document.referrer) {
      origens = [document.referrer];
    }
    // Se o navegador não informa, confiamos no cabeçalho frame-ancestors do _headers,
    // que já impede outros sites de incorporar o teste.
    if (!origens.length) return true;
    return origens.some(dominioPermitido);
  }

  if (!acessoPermitido()) {
    questoes.forEach(function (q) { q.parentNode.removeChild(q); });
    progresso.innerHTML = "";
    contador.textContent = "";
    palco.innerHTML =
      '<article class="card bloqueio">' +
        '<h2>Este teste é exclusivo para alunos do ADP</h2>' +
        '<p>Ele faz parte do curso <strong>Análise de Dados para Pesquisadores</strong> e só pode ser feito dentro da área de membros.</p>' +
        '<p>Já é aluno? Acesse o teste pela aula correspondente no curso.</p>' +
        '<div class="acoes"><a class="btn" href="' + LINK_FORA_DO_CURSO + '" target="_top" rel="noopener">Conhecer o curso</a></div>' +
      '</article>';
    return;
  }
  // -----------------------------------------------------

  var atual = 0, respostas = [];

  // Lê o conteúdo de cada questão uma única vez
  var dados = questoes.map(function (q) {
    q.parentNode.removeChild(q);
    var alts = Array.prototype.map.call(q.querySelectorAll(".alt"), function (li) {
      return {
        texto: li.querySelector(".alt-texto").innerHTML,
        feedback: li.querySelector(".alt-feedback").innerHTML,
        certa: li.getAttribute("data-certa") === "true"
      };
    });
    var fig = q.querySelector(":scope > figure");
    var rev = q.querySelector(".revela");
    var tre = q.querySelector(":scope > .trecho");
    return {
      enunciado: q.querySelector(".enunciado").innerHTML,
      textoResumo: q.querySelector(".enunciado").textContent,
      trecho: tre ? tre.outerHTML : "",
      figura: fig ? fig.outerHTML : "",
      revela: rev ? rev.innerHTML : "",
      alts: alts
    };
  });

  function sortear(lista) {
    var a = lista.slice();
    for (var i = a.length - 1; i > 0; i--) {
      var j = Math.floor(Math.random() * (i + 1)), t = a[i]; a[i] = a[j]; a[j] = t;
    }
    return a;
  }

  // Dentro do iframe, rola só o próprio teste (sem arrastar a página da Hotmart)
  function irParaOTopo() {
    try { window.scrollTo({ top: 0, behavior: "smooth" }); } catch (e) { window.scrollTo(0, 0); }
  }

  function desenharProgresso() {
    progresso.innerHTML = dados.map(function (_, i) {
      var c = respostas[i] === undefined ? (i === atual ? "atual" : "") : (respostas[i] ? "ok" : "erro");
      return '<span class="' + c + '"></span>';
    }).join("");
  }

  function tipografarFormulas(el) {
    // MathJax só existe se o cabeçalho tiver formulas = TRUE
    if (window.MathJax && MathJax.typesetPromise) MathJax.typesetPromise([el]);
  }

  function mostrarQuestao() {
    var d = dados[atual];
    var ordem = embaralhar ? sortear(d.alts) : d.alts;
    desenharProgresso();
    contador.textContent = "Questão " + (atual + 1) + " de " + dados.length;

    palco.innerHTML =
      '<article class="questao ativa">' +
        '<div class="enunciado"><span class="n">' + (atual + 1) + '.</span>' + d.enunciado + '</div>' +
        d.trecho +
        d.figura +
        '<ul class="alternativas">' +
          ordem.map(function (a, i) {
            return '<li><button class="op" data-i="' + i + '"><span class="letra">' + LETRAS[i] +
                   '</span><span>' + a.texto + '</span></button></li>';
          }).join("") +
        '</ul>' +
        '<div class="feedback" aria-live="polite"></div>' +
        '<div class="acoes"><button class="btn prox" hidden>' +
          (atual === dados.length - 1 ? "Ver resultado" : "Próxima questão →") +
        '</button></div>' +
      '</article>';

    var botoes = palco.querySelectorAll(".op");
    Array.prototype.forEach.call(botoes, function (b) {
      b.addEventListener("click", function () { responder(ordem, +b.getAttribute("data-i")); });
    });
    palco.querySelector(".prox").addEventListener("click", avancar);
    tipografarFormulas(palco);
  }

  function responder(ordem, i) {
    var escolha = ordem[i], acertou = escolha.certa;
    var iCerta = ordem.findIndex(function (a) { return a.certa; });
    respostas[atual] = acertou;

    Array.prototype.forEach.call(palco.querySelectorAll(".op"), function (b, j) {
      b.disabled = true;
      if (ordem[j].certa) b.classList.add("certa");
      else if (j === i) b.classList.add("errada");
      else b.classList.add("apagada");
    });

    var html = '<div class="veredito">' + (acertou ? "Resposta certa!" : "Não é essa.") + '</div>' +
               '<p>' + escolha.feedback + '</p>';
    if (!acertou) {
      html += '<p><b>A resposta certa é a ' + LETRAS[iCerta] + '.</b> ' +
              ordem[iCerta].feedback.replace(/^\s*(Isso!|Exatamente\.?|Isso mesmo!?)\s*/, "") + '</p>';
    }
    html += dados[atual].revela;

    var fb = palco.querySelector(".feedback");
    fb.innerHTML = html;
    fb.className = "feedback mostrar " + (acertou ? "ok" : "erro");
    desenharProgresso();
    var prox = palco.querySelector(".prox");
    prox.hidden = false;
    prox.focus({ preventScroll: true });
    var y = fb.getBoundingClientRect().top + window.pageYOffset - 16;
    try { window.scrollTo({ top: y, behavior: "smooth" }); } catch (e) { window.scrollTo(0, y); }
    tipografarFormulas(fb);
  }

  function avancar() {
    atual++;
    if (atual < dados.length) {
      mostrarQuestao();
      irParaOTopo();
    } else {
      mostrarResultado();
      irParaOTopo();
    }
  }

  function mostrarResultado() {
    var acertos = respostas.filter(Boolean).length, total = dados.length;
    desenharProgresso();
    contador.textContent = "Teste concluído";
    var msg = acertos === total ? "Perfeito! Você acertou todas."
      : acertos >= total * 0.7 ? "Muito bem! Revise as questões que errou e volte à aula se precisar."
      : "Vale a pena rever a aula com calma e refazer o teste depois. Errar aqui faz parte do aprendizado!";
    var botaoProxima = proximaAula
      ? '<a class="btn" href="' + proximaAula + '" target="_top">Ir para a próxima aula →</a>'
      : "";
    var dicaProxima = proximaAula ? ""
      : '<p class="dica-proxima">Quando estiver pronto, siga para a próxima aula no menu do curso.</p>';
    palco.innerHTML =
      '<article class="card resultado">' +
        '<h2>Seu resultado</h2>' +
        '<div class="placar">' + acertos + '<small> / ' + total + '</small></div>' +
        '<p>' + msg + '</p>' +
        '<ul class="resumo">' +
          dados.map(function (d, i) {
            return '<li><span class="ic ' + (respostas[i] ? "ok" : "erro") + '">' + (respostas[i] ? "✓" : "✗") +
                   '</span><span>' + (i + 1) + '. ' + d.textoResumo + '</span></li>';
          }).join("") +
        '</ul>' +
        '<div class="acoes"><button class="btn btn-contorno refazer">Refazer o teste</button>' + botaoProxima + '</div>' +
        dicaProxima +
      '</article>';
    palco.querySelector(".refazer").addEventListener("click", function () {
      atual = 0; respostas = []; mostrarQuestao();
      irParaOTopo();
    });
  }

  if (!dados.length) {
    palco.innerHTML = '<article class="card">Nenhuma questão encontrada neste arquivo.</article>';
    return;
  }
  mostrarQuestao();
})();
