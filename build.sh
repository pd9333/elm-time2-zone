#! /bin/bash

set -e

fetch_latest_tz() {
  if [ ! -d tz/.git ]; then
    git clone https://github.com/eggert/tz.git
  else
    git -C tz checkout main && git -C tz pull
  fi
}
#fetch_latest_tz

# tz: checkout latest version

version=$(git -C tz describe --tags --abbrev=0)
git -C tz -c advice.detachedHead=false checkout $version

# build file

zone_module="src/Time2/Zone.elm"
echo "Creating file $zone_module for version $version"
./build.py tz $version $zone_module
elm-format --yes $zone_module

backward_module="src/Backward.elm"
python3 ./build_backward_module.py tz $backward_module
elm-format --yes $backward_module

# tz: checkout main

git -C tz checkout main
