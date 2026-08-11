-- Filetype detection for the I language.
--
-- Neovim's built-in table already claims *.i for Progress, so a plain autocmd
-- races against it. vim.filetype.add() is consulted before the built-in table,
-- which makes the mapping deterministic.
--
-- *.I is deliberately not claimed: on case-insensitive filesystems it is the same
-- extension, and on case-sensitive ones it collides with assembler conventions.

vim.filetype.add({
  extension = {
    i = "i",
  },
})

-- Fallback for the case where this file is sourced after a buffer already exists
-- (for example when installing while Neovim is running).
vim.api.nvim_create_autocmd({ "BufRead", "BufNewFile" }, {
  group = vim.api.nvim_create_augroup("i_filetype", { clear = true }),
  pattern = "*.i",
  callback = function(args)
    if vim.bo[args.buf].filetype ~= "i" then
      vim.bo[args.buf].filetype = "i"
    end
  end,
})
