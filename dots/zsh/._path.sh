# muh binaries
USER_BIN="$HOME/bin:$HOME/.local/bin"

# golang compiler
GO_PATH="$HOME/bin/golang/bin"

# ruby version manager
RVM_PATH="$HOME/.rvm/bin"

# export the whole shebang
export PATH="$PATH:$USER_BIN:$GO_PATH:$RVM_PATH"
