export POLLEN=BUILD
if [ "$1" == "clean" ]; then 
    rm -rf ~/git/pollen-tfl/*
    cd ~/git/pollen-tfl
    git reset --hard HEAD
fi
if [ "$1" != "quick" ]; then 
  raco pollen reset
fi
raco pollen setup
raco pollen render
raco pollen publish
unset POLLEN