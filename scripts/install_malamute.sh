git clone git://github.com/jedisct1/libsodium.git
git clone git://github.com/zeromq/libzmq.git
git clone git://github.com/zeromq/czmq.git
git clone git://github.com/zeromq/malamute.git
for project in libsodium libzmq czmq malamute; do
    cd $project
    ./autogen.sh
    ./configure && make check
    sudo make install
    sudo ldconfig
    cd ..
done
