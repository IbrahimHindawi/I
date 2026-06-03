if vim.b.i_lsp_started then
  return
end

if not (vim.lsp and vim.lsp.start) then
  return
end

local command = vim.g.i_lsp_command or { "python", "-u", "C:/devel/i/scripts/i_lsp.py" }
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

vim.lsp.start({
  name = "i-lsp",
  cmd = command,
  root_dir = root,
})

vim.b.i_lsp_started = true
