-- | The browser half of a widget: one script, shipped as a literal so the
-- package keeps to @base@ alone (the same trade "Sabela.Notebook.Anim" makes).
--
-- It binds every control the Haskell side rendered, appends what the reader did
-- to that control's log, and posts the log back. It holds no state of its own
-- and knows nothing about any particular widget.
module Sabela.Notebook.Widget.Runtime (runtimeJs, runtimeCss) where

runtimeCss :: String
runtimeCss =
    unlines
        [ ".sbw{font-family:sans-serif;font-size:13px;color:#223;display:flex;flex-direction:column;gap:8px}"
        , ".sbw-across{display:flex;flex-direction:row;gap:10px;align-items:center;flex-wrap:wrap}"
        , ".sbw-down{display:flex;flex-direction:column;gap:8px}"
        , ".sbw-field{display:flex;gap:6px;align-items:center}"
        , ".sbw-field>span{color:#667;min-width:5em}"
        , ".sbw-press{padding:4px 12px;border:1px solid #c9c9d4;border-radius:6px;"
            ++ "background:#f7f7fb;cursor:pointer}"
        , ".sbw-press:hover{background:#ececf4}"
        , ".sbw-read{color:#667;min-width:3em}"
        , ".sbw-say{line-height:1.5}"
        ]

runtimeJs :: String
runtimeJs =
    unlines
        [ "function sabelaUi(cfg) {"
        , "  var root = document.getElementById(cfg.root);"
        , "  if (!root) return;"
        , "  var controls = root.querySelectorAll('[data-slot]');"
        , "  for (var i = 0; i < controls.length; i++) bind(controls[i]);"
        , "  function bind(el) {"
        , "    var kind = el.getAttribute('data-kind');"
        , "    var fired = kind === 'press' ? 'click' : kind === 'text' ? 'change' : 'change';"
        , "    el.addEventListener(fired, function () { record(el, kind); });"
        , "    if (kind === 'number') {"
        , "      el.addEventListener('input', function () {"
        , "        var read = el.parentNode.querySelector('.sbw-read');"
        , "        if (read) read.textContent = el.value;"
        , "      });"
        , "    }"
        , "  }"
        , "  function valueOf(el, kind) {"
        , "    if (kind === 'press') return '';"
        , "    if (kind === 'switch') return el.checked ? 'True' : 'False';"
        , "    return el.value;"
        , "  }"
        , "  function record(el, kind) {"
        , "    var log = el.getAttribute('data-log') || '[]';"
        , "    var entry = '(' + (Date.now() / 1000).toFixed(3) + ',' + quote(valueOf(el, kind)) + ')';"
        , "    var next = log === '[]' ? '[' + entry + ']' : log.slice(0, -1) + ',' + entry + ']';"
        , "    next = capped(next);"
        , "    el.setAttribute('data-log', next);"
        , "    parent.postMessage("
        , "      { type: 'widget', cellId: cfg.cid, name: el.getAttribute('data-slot'), value: next },"
        , "      '*'"
        , "    );"
        , "  }"
        , "  function quote(s) {"
        , "    return ("
        , "      '\"' +"
        , "      String(s).replace(/\\\\/g, '\\\\\\\\').replace(/\"/g, '\\\\\"').replace(/\\n/g, '\\\\n') +"
        , "      '\"'"
        , "    );"
        , "  }"
        , "  function capped(s) {"
        , "    while (s.length > 8000) {"
        , "      var cut = s.indexOf('),(');"
        , "      if (cut < 0) break;"
        , "      s = '[' + s.slice(cut + 2);"
        , "    }"
        , "    return s;"
        , "  }"
        , "}"
        ]
