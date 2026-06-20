// Shared runtime for Sabela's input widgets (slider, dropdown, checkbox,
// text input, button). Runs inside a cell's sandboxed output iframe and reports
// changes to the editor via parent.postMessage, where 22-widget-bridge.js POSTs
// them to /api/widget. The Haskell side (Sabela.Output.Widgets) embeds this file
// and emits a tiny sabelaXxx(cfg) bootstrap per widget — no inline event
// handlers, and values are set via the DOM (not string-concatenated HTML), so a
// value can never break out of its attribute.

// Report a widget change to the editor. `extra` carries optional fields
// (e.g. the text-cursor position) merged into the message.
function _sabelaPost(cid, name, value, extra) {
  var msg = { type: 'widget', cellId: cid, name: name, value: value };
  if (extra) {
    for (var k in extra) {
      if (Object.prototype.hasOwnProperty.call(extra, k)) msg[k] = extra[k];
    }
  }
  parent.postMessage(msg, '*');
}

// Replace the placeholder div (cfg.elId) with a freshly built control, and
// register it by name so a kernel→browser update (see below) can set its value.
function _sabelaMount(cfg, el, kind) {
  var host = document.getElementById(cfg.elId);
  if (!host) return;
  host.innerHTML = '';
  host.appendChild(el);
  _sabelaControls[cfg.name] = { el: el, kind: kind };
}

// Controls in this output iframe, keyed by widget name.
var _sabelaControls = {};

// Receive a value pushed from the kernel (04-sse.js forwards EvWidget here) and
// set the matching control. Setting .value/.checked programmatically does NOT
// fire input/change, so this cannot echo back out through the bridge.
window.addEventListener('message', function (e) {
  var d = e.data;
  if (!d || d.type !== 'widgetUpdate') return;
  var c = _sabelaControls[d.name];
  if (!c) return;
  if (c.kind === 'checkbox') c.el.checked = d.value === 'true';
  else {
    c.el.value = d.value;
    if (c.el._sabelaFit) c.el._sabelaFit();
  }
});

function sabelaSlider(cfg) {
  var el = document.createElement('input');
  el.type = 'range';
  el.min = cfg.min;
  el.max = cfg.max;
  if (cfg.step != null) el.step = cfg.step;
  el.value = cfg.value;
  el.addEventListener('input', function () {
    _sabelaPost(cfg.cid, cfg.name, el.value);
  });
  _sabelaMount(cfg, el, 'slider');
}

function sabelaDropdown(cfg) {
  var el = document.createElement('select');
  for (var i = 0; i < cfg.options.length; i++) {
    var opt = document.createElement('option');
    opt.textContent = cfg.options[i];
    if (cfg.options[i] === cfg.value) opt.selected = true;
    el.appendChild(opt);
  }
  el.addEventListener('change', function () {
    _sabelaPost(cfg.cid, cfg.name, el.value);
  });
  _sabelaMount(cfg, el, 'dropdown');
}

function sabelaCheckbox(cfg) {
  var el = document.createElement('input');
  el.type = 'checkbox';
  el.checked = !!cfg.checked;
  el.addEventListener('change', function () {
    _sabelaPost(cfg.cid, cfg.name, String(el.checked));
  });
  _sabelaMount(cfg, el, 'checkbox');
}

function sabelaTextInput(cfg) {
  var el = document.createElement('input');
  el.type = 'text';
  el.value = cfg.value;
  // Size the box to fit its text (with a sensible minimum) so the value isn't clipped.
  var fit = function () {
    el.size = Math.max(10, Math.min(80, el.value.length + 1));
  };
  fit();
  el.addEventListener('input', function () {
    fit();
    _sabelaPost(cfg.cid, cfg.name, el.value, { sel: el.selectionStart });
  });
  el._sabelaFit = fit;
  _sabelaMount(cfg, el, 'text');
}

function sabelaButton(cfg) {
  var el = document.createElement('button');
  el.textContent = cfg.label;
  el.addEventListener('click', function () {
    _sabelaPost(cfg.cid, cfg.name, 'clicked');
  });
  _sabelaMount(cfg, el, 'button');
}
