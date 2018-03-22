# Regex to find empty lines.
# EMPTY_LINE_REGEX <- "( |\n|\t|#.*?[\n]*)(\n|$)"

# Regex to check empty strings, or strings with only commentaries. Use Perl.
EMPTY_PERL_REGEX <- "^(([ \n\t]*(#.*?(\n|$))*))*$"

# Regex to search allowed functions and any function inside a list.
# Only the functions listed will be allowed to be executed.
# allowed <- paste(c("list", "c", "if", "for", "while", "function", "return", 
#                    "eval", "showNotification", "apply", "lapply", 
#                    "TemporalFunction", "names", "paste", "paste0", "print",
#                    "dim", "nrow", "ncol", "NROW", "NCOL", "length")
#                  , collapse = "|")
# 
# REGEX_ALLOWED <- paste0(
#   "(", 
#   allowed, 
#   "|(\\$|f_)([a-z]|[A-Z]|_)([a-z]|[A-Z]|[0-9]|_|\\.)*)( |\t)*\\(.*?\\)"
# )

# Regex to search commentaries
# REGEX_COMMENT <- "#.*?\n"
# 
# # Regex to search any functions calls
# REGEX_FUNCTION_CALL <- "([a-z]|[A-Z])([a-z]|[A-Z]|[0-9]|_|\\.)*( |\t)*\\(.*?\\)"
# 
# # Regex to search auxiliary equation definitions
# REGEX_AUXILIARY_EQUATIONS <- paste0(
#   "[ \t\n]([a-zA-Z0-9_]+[ \t]*\\<-[ \t]", 
#   "*function[ \t]*\\(.*?\\)[ \t\n]*(\\{([^{}]|(?2))*\\}))|\n"
# )