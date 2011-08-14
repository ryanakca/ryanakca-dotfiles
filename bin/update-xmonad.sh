#!/bin/sh
cd /usr/src/xmonad
darcs pull
runhaskell Setup.lhs configure
runhaskell Setup.lhs build
runhaskell Setup.lhs install
cd /usr/src/XMonadContrib
darcs pull
runhaskell Setup.lhs configure
runhaskell Setup.lhs build
runhaskell Setup.lhs install
