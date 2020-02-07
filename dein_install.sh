#!/bin/sh
# Standalone installer for Unixs
# Original version is created by shoma2da
# https://github.com/shoma2da/neobundle_installer

# dein.vim/LICENSE:
# License: MIT license
# AUTHOR:  Shougo Matsushita <Shougo.Matsu at gmail.com>
#
# Permission is hereby granted, free of charge, to any person obtaining
# a copy of this software and associated documentation files (the
# "Software"), to deal in the Software without restriction, including
# without limitation the rights to use, copy, modify, merge, publish,
# distribute, sublicense, and/or sell copies of the Software, and to
# permit persons to whom the Software is furnished to do so, subject to
# the following conditions:
#
# The above copyright notice and this permission notice shall be included
# in all copies or substantial portions of the Software.
#
# THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS
# OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
# MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
# IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY
# CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT,
# TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE
# SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

# Default install directory
INPUT_DIR="$HOME/.config/nvim/deind"
if [ $# -eq 1 ]; then
    INPUT_DIR="$1"
fi

# Convert the installation directory to absolute path
case "$INPUT_DIR" in
  /*) PLUGIN_DIR="$INPUT_DIR";;
  *) PLUGIN_DIR="$PWD/$INPUT_DIR";;
esac
INSTALL_DIR="${PLUGIN_DIR}/repos/github.com/Shougo/dein.vim"
echo "Install to \"$INSTALL_DIR\"..."
if [ -e "$INSTALL_DIR" ]; then
  echo "\"$INSTALL_DIR\" already exists!"
fi

echo ""

# check git command
type git || {
  echo 'Please install git or update your path to include the git executable!'
  exit 1
}
echo ""

# make plugin dir and fetch dein
if ! [ -e "$INSTALL_DIR" ]; then
  echo "Begin fetching dein..."
  mkdir -p "$PLUGIN_DIR"
  git clone https://github.com/Shougo/dein.vim "$INSTALL_DIR"
  echo "Done."
  echo ""
fi

echo "Complete setup dein!"
