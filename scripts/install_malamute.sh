#!/bin/bash
x=$(pwd)
cd /tmp/
sudo apt-get update && sudo apt-get install libtool autoconf automake -y
git clone --branch stable git://github.com/jedisct1/libsodium.git
git clone git://github.com/zeromq/libzmq.git
git clone git://github.com/zeromq/czmq.git
git clone git://github.com/zeromq/malamute.git
for project in libsodium libzmq czmq malamute; do
    cd $project
    echo $(pwd)
    ./autogen.sh
    ./configure && make check -j$(nproc --a)
    sudo make install
    sudo ldconfig
    cd ..
done
cd ${x}
