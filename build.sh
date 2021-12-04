cabal new-install tasty
cabal new-install tasty-hunit
cabal new-configure --enable-tests
cabal build
cabal test 
