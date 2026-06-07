const examples = {
  basics: {
    title: 'Basics',
    code: `import "std/Print.i"

add: proc<i32>(x: i32, y: i32)->i32 = {
    return x + y;
}

main: proc(argc: i32, argv: **char)->i32 = {
    total: i32 = add<i32>(5, 6);
    printfmt("total = {}\\n", total);
    return 0;
}`
  },
  generics: {
    title: 'Generics',
    code: `import "std/memops.i"
import "std/Array.i"

Payload: struct = {
    x: f32;
    y: f32;
}

add: proc<Payload>(a: Payload, b: Payload)->Payload = {
    return { .x = a.x + b.x, .y = a.y + b.y };
}

sum: proc<T>(items: *T, count: u64)->T = {
    result: T = {};
    for (i: u64 = 0; i < count; i += 1) {
        result = add<T>(result, items[i]);
    }
    return result;
}`
  },
  control: {
    title: 'Control flow',
    code: `Mode: enum = {
    Idle,
    Run,
    Attack,
}

step: proc(mode: Mode, value: i32)->i32 = {
    while (value > 0) {
        value -= 1;

        if (mode == Mode.Attack and value == 2) {
            continue;
        }

        switch (value) {
            case 1:
                break;
            default:
                value = value shl 1;
                break;
        }
    }

    return value | 1;
}`
  },
  types: {
    title: 'Types and reflection',
    code: `Payload: struct = {
    id: u32;
    name: *const char;
    samples: [4]f32;
}

main: proc()->i32 = {
    printfmt("type {} has {} fields\\n", Payload<>.name, Payload<>.field_count);

    for (i: u64 = 0; i < Payload<>.field_count; i += 1) {
        printfmt("field[{}] = {}\\n", i, Payload<>.fields[i].name);
    }
    return 0;
}`
  }
};

const exampleCode = document.querySelector('#exampleCode');
const exampleTitle = document.querySelector('#exampleTitle');
const tabs = [...document.querySelectorAll('.tab')];
const copyButton = document.querySelector('#copyCode');

const syntax = {
  keyword: new Set('proc return import cinclude define'.split(' ')),
  structure: new Set('struct enum union alias'.split(' ')),
  storage: new Set('const external external_emit'.split(' ')),
  conditional: new Set('if else switch case default'.split(' ')),
  repeat: new Set('for while do break continue'.split(' ')),
  operatorWord: new Set('and or shl shr'.split(' ')),
  boolean: new Set('true false'.split(' ')),
  coreType: new Set('i8 i16 i32 i64 u8 u16 u32 u64 f32 f64 usize b32 bool void char va_list FILE'.split(' ')),
  interopType: new Set('HWND HINSTANCE HMODULE HCURSOR HMENU HBRUSH HDC HGDIOBJ ATOM BOOL DWORD UINT INT LONG ULONG HRESULT WPARAM LPARAM LRESULT MSG WNDCLASSA PAINTSTRUCT RECT ID3D11Device ID3D11DeviceContext IDXGISwapChain ID3D11Buffer ID3D11Texture2D ID3D11RenderTargetView ID3D11DepthStencilView ID3D11ShaderResourceView ID3D11SamplerState ID3DBlob'.split(' ')),
  builtin: new Set('alignof cast null offsetof printf sizeof va_arg va_end va_start'.split(' '))
};

function escapeHtml(text) {
  return text
    .replaceAll('&', '&amp;')
    .replaceAll('<', '&lt;')
    .replaceAll('>', '&gt;');
}

function span(cls, text) {
  return `<span class="${cls}">${escapeHtml(text)}</span>`;
}

function isIdentStart(ch) {
  return /[A-Za-z_]/.test(ch);
}

function isIdent(ch) {
  return /[A-Za-z0-9_]/.test(ch);
}

function peekWord(code, index) {
  let i = index;
  while (/\s/.test(code[i] || '')) i++;
  if (!isIdentStart(code[i] || '')) return '';
  let start = i;
  while (isIdent(code[i] || '')) i++;
  return code.slice(start, i);
}

function peekAfterSpaces(code, index) {
  let i = index;
  while (/\s/.test(code[i] || '')) i++;
  return { ch: code[i] || '', index: i };
}

function previousSignificant(text) {
  for (let i = text.length - 1; i >= 0; i--) {
    if (!/\s/.test(text[i])) return text[i];
  }
  return '';
}

function colorizeI(code) {
  let out = '';
  let i = 0;

  while (i < code.length) {
    const ch = code[i];

    if (ch === '#') {
      const lineStart = code.lastIndexOf('\n', i - 1) + 1;
      if (/^\s*$/.test(code.slice(lineStart, i))) {
        const end = code.indexOf('\n', i);
        const stop = end === -1 ? code.length : end;
        out += span('tok-comment', code.slice(i, stop));
        i = stop;
        continue;
      }
    }

    if (ch === '"' || ch === "'") {
      const quote = ch;
      let j = i + 1;
      while (j < code.length) {
        if (code[j] === '\\') {
          j += 2;
          continue;
        }
        if (code[j] === quote) {
          j++;
          break;
        }
        j++;
      }
      out += span(quote === '"' ? 'tok-string' : 'tok-char', code.slice(i, j));
      i = j;
      continue;
    }

    const numberMatch = code.slice(i).match(/^(?:0x[0-9A-Fa-f]+|0b[01]+|(?:[0-9]+\.[0-9]*|[0-9]*\.[0-9]+)(?:f32|f64|f)?|[0-9]+(?:[uif][0-9]+)?)/);
    if (numberMatch) {
      out += span('tok-number', numberMatch[0]);
      i += numberMatch[0].length;
      continue;
    }

    if (isIdentStart(ch)) {
      const start = i;
      i++;
      while (isIdent(code[i] || '')) i++;
      const word = code.slice(start, i);
      const after = peekAfterSpaces(code, i);
      const nextWordAfterColon = after.ch === ':' ? peekWord(code, after.index + 1) : '';
      const prev = previousSignificant(code.slice(0, start));

      let cls = 'tok-ident';
      if (after.ch === ':' && nextWordAfterColon === 'proc') cls = 'tok-function';
      else if (after.ch === ':' && syntax.structure.has(nextWordAfterColon)) cls = 'tok-structure';
      else if (after.ch === '(' && prev !== ':') cls = 'tok-function';
      else if (syntax.keyword.has(word)) cls = 'tok-keyword';
      else if (syntax.structure.has(word)) cls = 'tok-structure';
      else if (syntax.storage.has(word)) cls = 'tok-storage';
      else if (syntax.conditional.has(word)) cls = 'tok-conditional';
      else if (syntax.repeat.has(word)) cls = 'tok-repeat';
      else if (syntax.operatorWord.has(word)) cls = 'tok-operator';
      else if (syntax.boolean.has(word)) cls = 'tok-boolean';
      else if (syntax.coreType.has(word)) cls = 'tok-type';
      else if (syntax.interopType.has(word)) cls = 'tok-interop';
      else if (syntax.builtin.has(word)) cls = 'tok-builtin';
      else if (after.ch === '<') cls = 'tok-structure';

      out += span(cls, word);
      continue;
    }

    const three = code.slice(i, i + 3);
    const two = code.slice(i, i + 2);
    if (three === '...') {
      out += span('tok-operator', three);
      i += 3;
      continue;
    }
    if (['.*', '.&', '->', '==', '!=', '<=', '>=', '&&', '||', '+=', '-=', '*=', '/=', '%=', '&=', '^=', '|='].includes(two)) {
      out += span('tok-operator', two);
      i += 2;
      continue;
    }
    if ('(){}[]<>'.includes(ch)) {
      out += span('tok-delimiter', ch);
      i++;
      continue;
    }
    if (':=,;.@&|^%*/!+?~-'.includes(ch)) {
      out += span('tok-operator', ch);
      i++;
      continue;
    }

    out += escapeHtml(ch);
    i++;
  }

  return out;
}

function setExample(name) {
  const example = examples[name] || examples.basics;
  exampleTitle.textContent = example.title;
  exampleCode.innerHTML = colorizeI(example.code);
  tabs.forEach(tab => tab.classList.toggle('active', tab.dataset.example === name));
  copyButton.dataset.copy = example.code;
}

tabs.forEach(tab => {
  tab.addEventListener('click', () => setExample(tab.dataset.example));
});

copyButton.addEventListener('click', async () => {
  const text = copyButton.dataset.copy || '';
  try {
    await navigator.clipboard.writeText(text);
    copyButton.textContent = 'Copied';
    setTimeout(() => { copyButton.textContent = 'Copy'; }, 1100);
  } catch {
    copyButton.textContent = 'Select text';
    setTimeout(() => { copyButton.textContent = 'Copy'; }, 1300);
  }
});

setExample('basics');

const navLinks = [...document.querySelectorAll('.nav a')];
const sections = navLinks
  .map(link => document.querySelector(link.getAttribute('href')))
  .filter(Boolean);

const observer = new IntersectionObserver(entries => {
  entries.forEach(entry => {
    if (!entry.isIntersecting) return;
    navLinks.forEach(link => {
      link.classList.toggle('active', link.getAttribute('href') === `#${entry.target.id}`);
    });
  });
}, { rootMargin: '-30% 0px -55% 0px', threshold: 0 });

sections.forEach(section => observer.observe(section));

const canvas = document.querySelector('#pipelineCanvas');
const ctx = canvas.getContext('2d');
let frame = 0;

function resizeCanvas() {
  const rect = canvas.getBoundingClientRect();
  const scale = window.devicePixelRatio || 1;
  canvas.width = Math.max(1, Math.floor(rect.width * scale));
  canvas.height = Math.max(1, Math.floor(rect.height * scale));
  ctx.setTransform(scale, 0, 0, scale, 0, 0);
}

function drawPipeline() {
  const w = canvas.clientWidth;
  const h = canvas.clientHeight;
  ctx.clearRect(0, 0, w, h);

  const nodes = [
    { x: w * 0.15, y: h * 0.36, label: '.i', color: '#8fc75a' },
    { x: w * 0.38, y: h * 0.24, label: 'parse', color: '#5eb3c6' },
    { x: w * 0.62, y: h * 0.40, label: 'emit C', color: '#e2b857' },
    { x: w * 0.84, y: h * 0.28, label: 'exe', color: '#df7b62' },
  ];

  ctx.lineWidth = 1;
  for (let i = 0; i < nodes.length - 1; i++) {
    const a = nodes[i];
    const b = nodes[i + 1];
    ctx.strokeStyle = 'rgba(240,243,238,.22)';
    ctx.beginPath();
    ctx.moveTo(a.x, a.y);
    ctx.bezierCurveTo((a.x + b.x) / 2, a.y - 70, (a.x + b.x) / 2, b.y + 70, b.x, b.y);
    ctx.stroke();

    const t = (frame / 100 + i * .23) % 1;
    const px = a.x + (b.x - a.x) * t;
    const py = a.y + (b.y - a.y) * t + Math.sin(t * Math.PI) * -42;
    ctx.fillStyle = b.color;
    ctx.fillRect(px - 3, py - 3, 6, 6);
  }

  ctx.strokeStyle = 'rgba(255,255,255,.08)';
  for (let x = 28; x < w; x += 48) {
    ctx.beginPath();
    ctx.moveTo(x, 20);
    ctx.lineTo(x, h - 68);
    ctx.stroke();
  }
  for (let y = 28; y < h - 64; y += 48) {
    ctx.beginPath();
    ctx.moveTo(20, y);
    ctx.lineTo(w - 20, y);
    ctx.stroke();
  }

  nodes.forEach((node, index) => {
    const pulse = Math.sin((frame + index * 17) / 18) * 3;
    ctx.fillStyle = '#101314';
    ctx.strokeStyle = node.color;
    ctx.lineWidth = 2;
    ctx.fillRect(node.x - 46 - pulse, node.y - 26 - pulse, 92 + pulse * 2, 52 + pulse * 2);
    ctx.strokeRect(node.x - 46 - pulse, node.y - 26 - pulse, 92 + pulse * 2, 52 + pulse * 2);
    ctx.fillStyle = node.color;
    ctx.font = '700 15px Consolas, monospace';
    ctx.textAlign = 'center';
    ctx.textBaseline = 'middle';
    ctx.fillText(node.label, node.x, node.y);
  });

  frame += 1;
  requestAnimationFrame(drawPipeline);
}

window.addEventListener('resize', resizeCanvas);
resizeCanvas();
drawPipeline();
