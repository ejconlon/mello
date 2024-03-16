local M = {}

function M.setup(args)
  local ts_parsers = require('nvim-treesitter.parsers')
  parse_config = ts_parsers.get_parser_configs()
  parser_config.mello = {
    install_info = {
      url = 'grammar',
      files = {'src/parser.c'},
      generate_requires_npm = false,
      requires_generate_from_grammar = false,
    },
  }
end

return M
