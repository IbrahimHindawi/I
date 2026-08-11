if vim.b.i_lsp_started then
  return
end

if not (vim.lsp and vim.lsp.start) then
  return
end

-- Resolution order, most specific first:
--   1. vim.g.i_lsp_command, for anyone who wants to override it outright
--   2. $I_HOME/scripts/i_lsp.py, the packaged toolchain layout
--   3. the repo path recorded by nvim/install.py at install time
local function default_command()
  local i_home = vim.env.I_HOME
  if i_home and i_home ~= "" then
    local packaged = i_home .. "/scripts/i_lsp.py"
    if vim.fn.filereadable(packaged) == 1 then
      return { "python", "-u", packaged }
    end
  end

  -- install.py rewrites this placeholder with the repo it was run from.
  local repo = "@I_REPO@"
  if not repo:find("^@") then
    return { "python", "-u", repo .. "/scripts/i_lsp.py" }
  end

  return { "python", "-u", "scripts/i_lsp.py" }
end

local command = vim.g.i_lsp_command or default_command()
if type(command) == "string" then
  command = { command }
end

local root_markers = { "bunyan.py", "CMakeLists.txt", ".git" }
local function find_root()
  if vim.fs and vim.fs.root then
    return vim.fs.root(0, root_markers)
  end

  local file = vim.api.nvim_buf_get_name(0)
  local dir = file ~= "" and vim.fn.fnamemodify(file, ":p:h") or vim.fn.getcwd()
  for _, marker in ipairs(root_markers) do
    local found = vim.fn.findfile(marker, dir .. ";")
    if found ~= "" then
      return vim.fn.fnamemodify(found, ":p:h")
    end
    found = vim.fn.finddir(marker, dir .. ";")
    if found ~= "" then
      return vim.fn.fnamemodify(found, ":p:h")
    end
  end

  return vim.fn.getcwd()
end

local root = find_root() or vim.fn.getcwd()

if vim.diagnostic then
  pcall(vim.diagnostic.config, {
    underline = true,
    signs = true,
    virtual_text = false,
    update_in_insert = true,
    severity_sort = true,
  }, 0)
  if vim.diagnostic.enable then
    if not pcall(vim.diagnostic.enable, true, { bufnr = 0 }) then
      pcall(vim.diagnostic.enable, 0)
    end
  end
end

local get_clients = vim.lsp.get_clients or vim.lsp.get_active_clients
if get_clients then
  local ok, clients = pcall(get_clients, { name = "i-lsp" })
  if not ok then
    clients = get_clients()
  end

  for _, client in ipairs(clients or {}) do
    if client.name == "i-lsp" and client.config and client.config.root_dir == root then
      vim.lsp.buf_attach_client(0, client.id)
      vim.b.i_lsp_started = true
      return
    end
  end
end

local client_id = vim.lsp.start({
  name = "i-lsp",
  cmd = command,
  root_dir = root,
  cmd_cwd = root,
  flags = {
    debounce_text_changes = 75,
  },
})

if client_id then
  vim.b.i_lsp_started = true
else
  vim.b.i_lsp_started = false
  vim.notify("failed to start i-lsp", vim.log.levels.WARN)
end
