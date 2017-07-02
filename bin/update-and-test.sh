#! /bin/bash

set -e

echo -e "\n\nInstalling packages"
elm-package install --yes

echo -e "\n\nValidating format"
elm-format --validate src tests demo

echo -e "\n\nUpdating demo and gh page"
elm-make demo/Demo.elm --output demo/demo.js

echo -e "\n\nTesting..."
cd tests
elm-package install --yes
cd ..
elm-test


